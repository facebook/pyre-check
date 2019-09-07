(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Expression
open Pyre
open Statement

type t = { unannotated_global_environment: UnannotatedGlobalEnvironment.ReadOnly.t }

module UnresolvedAlias = struct
  type t = {
    qualifier: Reference.t;
    target: Reference.t;
    value: Expression.expression_t;
  }
  [@@deriving sexp, compare, hash]
end

module SharedMemory = struct
  (** Values *)
  module FunctionKeyValue = struct
    type t = Reference.t list

    let prefix = Prefix.make ()

    let description = "Function keys"

    let unmarshall value = Marshal.from_string value 0
  end

  module GlobalKeyValue = struct
    type t = Reference.t list

    let prefix = Prefix.make ()

    let description = "Global keys"

    let unmarshall value = Marshal.from_string value 0
  end

  module AliasKeyValue = struct
    type t = Identifier.t list

    let prefix = Prefix.make ()

    let description = "Alias keys"

    let unmarshall value = Marshal.from_string value 0
  end

  module DependentKeyValue = struct
    type t = Reference.t list

    let prefix = Prefix.make ()

    let description = "Dependent keys"

    let unmarshall value = Marshal.from_string value 0
  end

  module ClassMetadataValue = struct
    type t = GlobalResolution.class_metadata

    let prefix = Prefix.make ()

    let description = "Class metadata"

    let unmarshall value = Marshal.from_string value 0

    let compare = GlobalResolution.compare_class_metadata
  end

  module AliasValue = struct
    type t = Type.alias

    let prefix = Prefix.make ()

    let description = "Alias"

    let unmarshall value = Marshal.from_string value 0

    let compare = Type.compare_alias
  end

  module GlobalValue = struct
    type t = GlobalResolution.global

    let prefix = Prefix.make ()

    let description = "Global"

    let unmarshall value = Marshal.from_string value 0

    let compare = GlobalResolution.compare_global
  end

  module DependentValue = struct
    type t = Reference.Set.Tree.t

    let prefix = Prefix.make ()

    let description = "Dependent"

    let unmarshall value = Marshal.from_string value 0
  end

  module EdgeValue = struct
    type t = ClassHierarchy.Target.t list [@@deriving compare]

    let prefix = Prefix.make ()

    let description = "Edges"

    let unmarshall value = Marshal.from_string value 0
  end

  module BackedgeValue = struct
    type t = ClassHierarchy.Target.Set.Tree.t

    let prefix = Prefix.make ()

    let description = "Backedges"

    let unmarshall value = Marshal.from_string value 0
  end

  module OrderKeyValue = struct
    type t = IndexTracker.t list [@@deriving compare]

    let prefix = Prefix.make ()

    let description = "Order keys"

    let unmarshall value = Marshal.from_string value 0
  end

  module UndecoratedFunctionValue = struct
    type t = Type.t Type.Callable.overload [@@deriving compare]

    let prefix = Prefix.make ()

    let description = "Undecorated functions"

    let unmarshall value = Marshal.from_string value 0
  end

  (** Shared memory maps *)

  module ClassMetadata =
    Memory.DependencyTrackedTableWithCache
      (SharedMemoryKeys.StringKey)
      (SharedMemoryKeys.ReferenceDependencyKey)
      (ClassMetadataValue)
  module Aliases =
    Memory.DependencyTrackedTableNoCache
      (SharedMemoryKeys.StringKey)
      (SharedMemoryKeys.ReferenceDependencyKey)
      (AliasValue)
  module Globals =
    Memory.DependencyTrackedTableWithCache
      (SharedMemoryKeys.ReferenceKey)
      (SharedMemoryKeys.ReferenceDependencyKey)
      (GlobalValue)
  module Dependents = Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (DependentValue)
  module UndecoratedFunctions =
    Memory.DependencyTrackedTableWithCache
      (SharedMemoryKeys.ReferenceKey)
      (SharedMemoryKeys.ReferenceDependencyKey)
      (UndecoratedFunctionValue)

  module FunctionKeys = Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (FunctionKeyValue)
  (** Keys *)

  module GlobalKeys = Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (GlobalKeyValue)
  module AliasKeys = Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (AliasKeyValue)
  module DependentKeys = Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (DependentKeyValue)

  (** Type order maps *)

  module OrderEdges =
    Memory.DependencyTrackedTableWithCache
      (IndexTracker.IndexKey)
      (SharedMemoryKeys.ReferenceDependencyKey)
      (EdgeValue)
  module OrderBackedges = Memory.WithCache.Make (IndexTracker.IndexKey) (BackedgeValue)
  module OrderKeys =
    Memory.DependencyTrackedTableWithCache
      (Memory.SingletonKey)
      (SharedMemoryKeys.ReferenceDependencyKey)
      (OrderKeyValue)
end

module SharedMemoryClassHierarchyHandler = struct
  open SharedMemory

  let edges = OrderEdges.get ?dependency:None

  let set_edges ~key ~data =
    OrderEdges.remove_batch (OrderEdges.KeySet.singleton key);
    OrderEdges.add key data


  let backedges key = OrderBackedges.get key >>| ClassHierarchy.Target.Set.of_tree

  let set_backedges ~key ~data =
    let value = ClassHierarchy.Target.Set.to_tree data in
    OrderBackedges.remove_batch (OrderBackedges.KeySet.singleton key);
    OrderBackedges.add key value


  let connect ?(parameters = Type.OrderedTypes.Concrete []) ~predecessor ~successor =
    let index_of annotation = IndexTracker.index annotation in
    let predecessor = index_of predecessor in
    let successor = index_of successor in
    (* Add edges. *)
    let successors = edges predecessor |> Option.value ~default:[] in
    set_edges
      ~key:predecessor
      ~data:({ ClassHierarchy.Target.target = successor; parameters } :: successors);

    (* Add backedges. *)
    let predecessors =
      backedges successor |> Option.value ~default:ClassHierarchy.Target.Set.empty
    in
    set_backedges
      ~key:successor
      ~data:(Set.add predecessors { ClassHierarchy.Target.target = predecessor; parameters })


  let disconnect_backedges purged_indices =
    let all_successors =
      let all_successors = IndexTracker.Hash_set.create () in
      let add_successors key =
        match edges key with
        | Some successors ->
            List.iter successors ~f:(fun { ClassHierarchy.Target.target; _ } ->
                Hash_set.add all_successors target)
        | None -> ()
      in
      IndexTracker.Set.iter purged_indices ~f:add_successors;
      all_successors
    in
    let remove_backedges successor =
      backedges successor
      >>| (fun current_predecessors ->
            let new_predecessors =
              Set.filter
                ~f:(fun { ClassHierarchy.Target.target; _ } ->
                  not (IndexTracker.Set.mem purged_indices target))
                current_predecessors
            in
            set_backedges ~key:successor ~data:new_predecessors)
      |> ignore
    in
    Hash_set.iter all_successors ~f:remove_backedges


  let deduplicate ~annotations =
    let deduplicate_annotation index =
      let module Deduplicator (ListOrSet : ClassHierarchy.Target.ListOrSet) = struct
        let deduplicate edges set_edges =
          let keep_first (visited, edges) ({ ClassHierarchy.Target.target; _ } as edge) =
            if Set.mem visited target then
              visited, edges
            else
              Set.add visited target, ListOrSet.add edges edge
          in
          let deduplicate found =
            ListOrSet.fold found ~f:keep_first ~init:(IndexTracker.Set.empty, ListOrSet.empty)
            |> snd
          in
          match edges index with
          | Some found -> set_edges ~key:index ~data:(deduplicate found)
          | None -> ()
      end
      in
      let module EdgeDeduplicator = Deduplicator (ClassHierarchy.Target.List) in
      let module BackedgeDeduplicator = Deduplicator (ClassHierarchy.Target.Set) in
      EdgeDeduplicator.deduplicate edges set_edges;
      BackedgeDeduplicator.deduplicate backedges set_backedges
    in
    annotations |> List.map ~f:IndexTracker.index |> List.iter ~f:deduplicate_annotation


  let remove_extra_edges_to_object annotations =
    let index_of annotation = IndexTracker.index annotation in
    let keys = List.map annotations ~f:index_of in
    let object_index = index_of "object" in
    let remove_extra_references key =
      edges key
      >>| (fun connected ->
            let disconnected =
              ClassHierarchy.Target.List.filter
                connected
                ~f:(fun { ClassHierarchy.Target.target; _ } -> target <> object_index)
            in
            if ClassHierarchy.Target.List.is_empty disconnected then
              []
            else (
              set_edges ~key ~data:disconnected;
              [key] ))
      |> Option.value ~default:[]
    in
    let removed_indices =
      List.concat_map ~f:remove_extra_references keys |> IndexTracker.Set.of_list
    in
    backedges object_index
    >>| (fun edges ->
          let edges =
            ClassHierarchy.Target.Set.filter edges ~f:(fun { ClassHierarchy.Target.target; _ } ->
                not (Set.mem removed_indices target))
          in
          set_backedges ~key:object_index ~data:edges)
    |> Option.value ~default:()
end

module SharedMemoryDependencyHandler = struct
  open SharedMemory

  let add_new_key ~get ~add ~qualifier ~key =
    let existing = get qualifier in
    match existing with
    | None -> add qualifier [key]
    | Some keys -> add qualifier (key :: keys)


  let add_function_key ~qualifier reference =
    add_new_key ~qualifier ~key:reference ~get:FunctionKeys.get ~add:FunctionKeys.add


  let add_alias_key ~qualifier alias =
    add_new_key ~qualifier ~key:alias ~get:AliasKeys.get ~add:AliasKeys.add


  let add_global_key ~qualifier global =
    add_new_key ~qualifier ~key:global ~get:GlobalKeys.get ~add:GlobalKeys.add


  let add_dependent_key ~qualifier dependent =
    add_new_key ~qualifier ~key:dependent ~get:DependentKeys.get ~add:DependentKeys.add


  let add_dependent ~qualifier dependent =
    add_dependent_key ~qualifier dependent;
    match Dependents.get dependent with
    | None -> Dependents.add dependent (Reference.Set.Tree.singleton qualifier)
    | Some dependencies -> Dependents.add dependent (Reference.Set.Tree.add dependencies qualifier)


  let get_function_keys ~qualifier = FunctionKeys.get qualifier |> Option.value ~default:[]

  let get_alias_keys ~qualifier = AliasKeys.get qualifier |> Option.value ~default:[]

  let get_global_keys ~qualifier = GlobalKeys.get qualifier |> Option.value ~default:[]

  let get_dependent_keys ~qualifier = DependentKeys.get qualifier |> Option.value ~default:[]

  let remove_from_dependency_graph qualifiers =
    let keys =
      List.concat_map ~f:(fun qualifier -> get_dependent_keys ~qualifier) qualifiers
      |> List.dedup_and_sort ~compare:Reference.compare
    in
    let new_dependents = Reference.Table.create () in
    let recompute_dependents key dependents =
      let qualifiers = Reference.Set.Tree.of_list qualifiers in
      Hashtbl.set new_dependents ~key ~data:(Reference.Set.Tree.diff dependents qualifiers)
    in
    List.iter ~f:(fun key -> Dependents.get key >>| recompute_dependents key |> ignore) keys;
    Dependents.remove_batch (Dependents.KeySet.of_list (Hashtbl.keys new_dependents));
    Hashtbl.iteri new_dependents ~f:(fun ~key ~data -> Dependents.add key data);
    DependentKeys.remove_batch (Dependents.KeySet.of_list qualifiers)


  let clear_keys_batch qualifiers =
    FunctionKeys.remove_batch (FunctionKeys.KeySet.of_list qualifiers);
    AliasKeys.remove_batch (AliasKeys.KeySet.of_list qualifiers);
    GlobalKeys.remove_batch (GlobalKeys.KeySet.of_list qualifiers);
    DependentKeys.remove_batch (DependentKeys.KeySet.of_list qualifiers)


  let dependents = Dependents.get

  let normalize qualifiers =
    let normalize_keys qualifier =
      ( match FunctionKeys.get qualifier with
      | Some keys ->
          FunctionKeys.remove_batch (FunctionKeys.KeySet.singleton qualifier);
          FunctionKeys.add qualifier (List.dedup_and_sort ~compare:Reference.compare keys)
      | None -> () );
      ( match AliasKeys.get qualifier with
      | Some keys ->
          AliasKeys.remove_batch (AliasKeys.KeySet.singleton qualifier);
          AliasKeys.add qualifier (List.dedup_and_sort ~compare:Identifier.compare keys)
      | None -> () );
      ( match GlobalKeys.get qualifier with
      | Some keys ->
          GlobalKeys.remove_batch (GlobalKeys.KeySet.singleton qualifier);
          GlobalKeys.add qualifier (List.dedup_and_sort ~compare:Reference.compare keys)
      | None -> () );
      match DependentKeys.get qualifier with
      | Some keys ->
          DependentKeys.remove_batch (DependentKeys.KeySet.singleton qualifier);
          DependentKeys.add qualifier (List.dedup_and_sort ~compare:Reference.compare keys)
      | None -> ()
    in
    List.iter qualifiers ~f:normalize_keys;
    let normalize_dependents name =
      match Dependents.get name with
      | Some unnormalized ->
          Dependents.remove_batch (Dependents.KeySet.singleton name);
          Reference.Set.Tree.to_list unnormalized
          |> List.sort ~compare:Reference.compare
          |> Reference.Set.Tree.of_list
          |> Dependents.add name
      | None -> ()
    in
    List.concat_map qualifiers ~f:(fun qualifier -> get_dependent_keys ~qualifier)
    |> List.dedup_and_sort ~compare:Reference.compare
    |> List.iter ~f:normalize_dependents


  type table_keys = {
    aliases: SharedMemory.Aliases.KeySet.t;
    undecorated_signatures: SharedMemory.UndecoratedFunctions.KeySet.t;
    globals: SharedMemory.Globals.KeySet.t;
  }

  let get_all_dependent_table_keys qualifiers =
    let alias_keys = List.concat_map ~f:(fun qualifier -> get_alias_keys ~qualifier) qualifiers in
    let global_keys =
      List.concat_map ~f:(fun qualifier -> get_global_keys ~qualifier) qualifiers
    in
    let function_keys =
      List.concat_map ~f:(fun qualifier -> get_function_keys ~qualifier) qualifiers
    in
    let open SharedMemory in
    {
      aliases = Aliases.KeySet.of_list alias_keys;
      undecorated_signatures = UndecoratedFunctions.KeySet.of_list global_keys;
      (* We add a global name for each function definition as well. *)
      globals = Globals.KeySet.of_list (global_keys @ function_keys);
    }


  let find_added_dependent_table_keys qualifiers ~old_keys =
    let new_keys = get_all_dependent_table_keys qualifiers in
    {
      aliases = Aliases.KeySet.diff new_keys.aliases old_keys.aliases;
      undecorated_signatures =
        UndecoratedFunctions.KeySet.diff
          new_keys.undecorated_signatures
          old_keys.undecorated_signatures;
      globals = Globals.KeySet.diff new_keys.globals old_keys.globals;
    }
end

let register_alias _ ~qualifier ~key ~data =
  SharedMemoryDependencyHandler.add_alias_key ~qualifier key;
  SharedMemory.Aliases.add key data


module ResolvedAlias = struct
  type t = {
    qualifier: Reference.t;
    name: Type.Primitive.t;
    annotation: Type.alias;
  }
  [@@deriving sexp, compare, hash]

  let register environment { qualifier; name; annotation } =
    register_alias environment ~qualifier ~key:name ~data:annotation
end

let untracked_class_hierarchy_handler { unannotated_global_environment; _ } =
  ( module struct
    let edges = SharedMemory.OrderEdges.get ?dependency:None

    let backedges key = SharedMemory.OrderBackedges.get key >>| ClassHierarchy.Target.Set.of_tree

    let contains =
      UnannotatedGlobalEnvironment.ReadOnly.class_exists
        ?dependency:None
        unannotated_global_environment
  end : ClassHierarchy.Handler )


let connect_annotations_to_object environment annotations =
  let connect_to_top annotation =
    let index = IndexTracker.index annotation in
    let annotation = IndexTracker.annotation index in
    let object_primitive = "object" in
    if
      not
        (ClassHierarchy.is_transitive_successor
           (untracked_class_hierarchy_handler environment)
           ~source:object_primitive
           ~target:annotation)
    then
      match SharedMemoryClassHierarchyHandler.edges index with
      | Some targets when List.length targets > 0 -> ()
      | _ ->
          SharedMemoryClassHierarchyHandler.connect
            ?parameters:None
            ~predecessor:annotation
            ~successor:object_primitive
  in
  List.iter ~f:connect_to_top annotations


let resolution_implementation ?dependency { unannotated_global_environment } () =
  let ast_environment =
    UnannotatedGlobalEnvironment.ReadOnly.ast_environment unannotated_global_environment
  in
  GlobalResolution.create
    ~ast_environment
    ~aliases:(SharedMemory.Aliases.get ?dependency)
    ~module_definition:(AstEnvironment.ReadOnly.get_module_metadata ?dependency ast_environment)
    ~class_definition:
      (UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
         unannotated_global_environment
         ?dependency)
    ~class_metadata:(SharedMemory.ClassMetadata.get ?dependency)
    ~undecorated_signature:(SharedMemory.UndecoratedFunctions.get ?dependency)
    ~global:(SharedMemory.Globals.get ?dependency)
    ~edges:(SharedMemory.OrderEdges.get ?dependency)
    ~backedges:SharedMemory.OrderBackedges.get
    (module Annotated.Class)


let resolution = resolution_implementation ?dependency:None

let dependency_tracked_resolution environment ~dependency () =
  resolution_implementation ~dependency environment ()


let ast_environment { unannotated_global_environment; _ } =
  UnannotatedGlobalEnvironment.ReadOnly.ast_environment unannotated_global_environment


let connect_definition
    { unannotated_global_environment; _ }
    ~(resolution : GlobalResolution.t)
    ~definition:({ Node.value = { Class.name; bases; _ }; _ } as definition)
  =
  let annotated = Annotated.Class.create definition in
  (* We have to split the type here due to our built-in aliasing. Namely, the "list" and "dict"
     classes get expanded into parametric types of List[Any] and Dict[Any, Any]. *)
  let connect ~predecessor ~successor ~parameters =
    let primitive_cycle =
      (* Primitive cycles can be introduced by meta-programming. *)
      String.equal predecessor successor
    in
    if not primitive_cycle then
      SharedMemoryClassHierarchyHandler.connect ~predecessor ~successor ~parameters
  in
  let primitive = Reference.show name in
  ( match Annotated.Class.inferred_callable_type annotated ~resolution with
  | Some callable ->
      connect
        ~predecessor:primitive
        ~successor:"typing.Callable"
        ~parameters:(Concrete [Type.Callable callable])
  | None -> () );

  (* Register normal annotations. *)
  let register_supertype { Expression.Call.Argument.value; _ } =
    let value = Expression.delocalize value in
    match Node.value value with
    | Call _
    | Name _ -> (
        let supertype, parameters =
          (* While building environment, allow untracked to parse into primitives *)
          GlobalResolution.parse_annotation
            ~allow_untracked:true
            ~allow_invalid_type_parameters:true
            resolution
            value
          |> Type.split
        in
        match supertype with
        | Type.Top ->
            Statistics.event
              ~name:"superclass of top"
              ~section:`Environment
              ~normals:["unresolved name", Expression.show value]
              ()
        | Type.Primitive primitive
          when not
                 (UnannotatedGlobalEnvironment.ReadOnly.class_exists
                    unannotated_global_environment
                    primitive) ->
            Log.log ~section:`Environment "Superclass annotation %a is missing" Type.pp supertype
        | Type.Primitive supertype ->
            connect ~predecessor:primitive ~successor:supertype ~parameters
        | _ ->
            Log.log ~section:`Environment "Superclass annotation %a is missing" Type.pp supertype )
    | _ -> ()
  in
  let inferred_base = Annotated.Class.inferred_generic_base annotated ~resolution in
  if not (SharedMemory.OrderBackedges.mem (IndexTracker.index primitive)) then
    SharedMemoryClassHierarchyHandler.set_backedges
      ~key:(IndexTracker.index primitive)
      ~data:ClassHierarchy.Target.Set.empty;

  (* Don't register metaclass=abc.ABCMeta, etc. superclasses. *)
  inferred_base @ bases
  |> List.filter ~f:(fun { Expression.Call.Argument.name; _ } -> Option.is_none name)
  |> List.iter ~f:register_supertype;
  if not (SharedMemory.OrderEdges.mem (IndexTracker.index primitive)) then
    SharedMemoryClassHierarchyHandler.set_edges ~key:(IndexTracker.index primitive) ~data:[]


let register_class_metadata ({ unannotated_global_environment; _ } as environment) class_name =
  let open SharedMemory in
  let successors =
    ClassHierarchy.successors (untracked_class_hierarchy_handler environment) class_name
  in
  let is_final =
    UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
      unannotated_global_environment
      class_name
    >>| (fun { Node.value = definition; _ } -> Class.is_final definition)
    |> Option.value ~default:false
  in
  let in_test =
    let is_unit_test { Node.value = definition; _ } = Class.is_unit_test definition in
    let successor_classes =
      List.filter_map
        ~f:
          (UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
             unannotated_global_environment)
        successors
    in
    List.exists ~f:is_unit_test successor_classes
  in
  let extends_placeholder_stub_class =
    UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
      unannotated_global_environment
      class_name
    >>| Annotated.Class.create
    >>| Annotated.Class.extends_placeholder_stub_class
          ~aliases:SharedMemory.Aliases.get
          ~module_definition:
            (AstEnvironment.ReadOnly.get_module_metadata (ast_environment environment))
    |> Option.value ~default:false
  in
  ClassMetadata.add
    class_name
    { GlobalResolution.is_test = in_test; successors; is_final; extends_placeholder_stub_class }


let register_global _ ?qualifier ~reference ~global =
  Option.iter qualifier ~f:(fun qualifier ->
      SharedMemoryDependencyHandler.add_global_key ~qualifier reference);
  SharedMemory.Globals.add reference global


let add_special_globals environment =
  (* Add `None` constant to globals. *)
  let annotation annotation =
    Annotation.create_immutable ~global:true annotation |> Node.create_with_default_location
  in
  register_global
    environment
    ?qualifier:None
    ~reference:(Reference.create "None")
    ~global:(annotation Type.none);
  register_global
    environment
    ?qualifier:None
    ~reference:(Reference.create "...")
    ~global:(annotation Type.Any)


let dependencies _ = SharedMemory.Dependents.get

let collect_aliases
    ({ unannotated_global_environment; _ } as environment)
    { Source.statements; qualifier; _ }
  =
  let rec visit_statement ~qualifier aliases { Node.value; _ } =
    match value with
    | Statement.Assign { Assign.target = { Node.value = Name target; _ }; annotation; value; _ }
      when Expression.is_simple_name target -> (
        let target =
          let target = Expression.name_to_reference_exn target |> Reference.sanitize_qualified in
          Reference.combine qualifier target
        in
        let target_annotation =
          Type.create
            ~aliases:SharedMemory.Aliases.get
            (Expression.from_reference ~location:Location.Reference.any target)
        in
        match Node.value value, annotation with
        | ( _,
            Some
              {
                Node.value =
                  Call
                    {
                      callee =
                        {
                          Node.value =
                            Name
                              (Name.Attribute
                                {
                                  base =
                                    {
                                      Node.value =
                                        Name
                                          (Name.Attribute
                                            {
                                              base =
                                                { Node.value = Name (Name.Identifier "typing"); _ };
                                              attribute = "Type";
                                              _;
                                            });
                                      _;
                                    };
                                  attribute = "__getitem__";
                                  _;
                                });
                          _;
                        };
                      arguments =
                        [ {
                            Call.Argument.value =
                              {
                                Node.value =
                                  Call
                                    {
                                      callee =
                                        {
                                          Node.value =
                                            Name
                                              (Name.Attribute
                                                {
                                                  base =
                                                    {
                                                      Node.value =
                                                        Name
                                                          (Name.Attribute
                                                            {
                                                              base =
                                                                {
                                                                  Node.value =
                                                                    Name
                                                                      (Name.Identifier
                                                                        "mypy_extensions");
                                                                  _;
                                                                };
                                                              attribute = "TypedDict";
                                                              _;
                                                            });
                                                      _;
                                                    };
                                                  attribute = "__getitem__";
                                                  _;
                                                });
                                          _;
                                        };
                                      _;
                                    };
                                _;
                              };
                            _;
                          } ];
                    };
                _;
              } ) ->
            if not (Type.is_top target_annotation) then
              { UnresolvedAlias.qualifier; target; value } :: aliases
            else
              aliases
        | ( Call _,
            Some
              {
                Node.value =
                  Name
                    (Name.Attribute
                      {
                        base = { Node.value = Name (Name.Identifier "typing"); _ };
                        attribute = "TypeAlias";
                        _;
                      });
                _;
              } )
        | ( Name _,
            Some
              {
                Node.value =
                  Name
                    (Name.Attribute
                      {
                        base = { Node.value = Name (Name.Identifier "typing"); _ };
                        attribute = "TypeAlias";
                        _;
                      });
                _;
              } )
        | Call _, None
        | Name _, None -> (
            let value = Expression.delocalize value in
            match Type.Variable.parse_declaration value ~target with
            | Some variable ->
                register_alias
                  environment
                  ~qualifier
                  ~key:(Reference.show target)
                  ~data:(Type.VariableAlias variable);
                aliases
            | None ->
                let value_annotation = Type.create ~aliases:SharedMemory.Aliases.get value in
                if
                  not
                    ( Type.is_top target_annotation
                    || Type.is_top value_annotation
                    || Type.equal value_annotation target_annotation )
                then
                  { UnresolvedAlias.qualifier; target; value } :: aliases
                else
                  aliases )
        | _ -> aliases )
    | Import { Import.from = Some _; imports = [{ Import.name; _ }] }
      when String.equal (Reference.show name) "*" ->
        (* Don't register x.* as an alias when a user writes `from x import *`. *)
        aliases
    | Import { Import.from; imports } ->
        let from =
          match from >>| Reference.show with
          | None
          | Some "future.builtins"
          | Some "builtins" ->
              Reference.empty
          | Some from -> Reference.create from
        in
        let import_to_alias { Import.name; alias } =
          let qualified_name =
            match alias with
            | None -> Reference.combine qualifier name
            | Some alias -> Reference.combine qualifier alias
          in
          let original_name = Reference.combine from name in
          (* A module might import T and define a T that shadows it. In this case, we do not want
             to create the alias. *)
          if
            UnannotatedGlobalEnvironment.ReadOnly.class_exists
              unannotated_global_environment
              (Reference.show qualified_name)
          then
            []
          else
            match Reference.as_list qualified_name, Reference.as_list original_name with
            | [single_identifier], [typing; identifier]
              when String.equal typing "typing" && String.equal single_identifier identifier ->
                (* builtins has a bare qualifier. Don't export bare aliases from typing. *)
                []
            | _ ->
                [ {
                    UnresolvedAlias.qualifier;
                    target = qualified_name;
                    value =
                      Expression.from_reference ~location:Location.Reference.any original_name;
                  } ]
        in
        List.rev_append (List.concat_map ~f:import_to_alias imports) aliases
    | _ -> aliases
  in
  List.fold ~init:[] ~f:(visit_statement ~qualifier) statements


let resolve_alias environment { UnresolvedAlias.qualifier; target; value } =
  let order = untracked_class_hierarchy_handler environment in
  let target_primitive_name = Reference.show target in
  let value_annotation =
    match Type.create ~aliases:SharedMemory.Aliases.get value with
    | Type.Variable variable ->
        if Type.Variable.Unary.contains_subvariable variable then
          Type.Any
        else
          Type.Variable { variable with variable = Reference.show target }
    | annotation -> annotation
  in
  let dependencies = String.Hash_set.create () in
  let module TrackedTransform = Type.Transform.Make (struct
    type state = unit

    let visit_children_before _ = function
      | Type.Optional Bottom -> false
      | _ -> true


    let visit_children_after = false

    let visit _ annotation =
      let new_state, transformed_annotation =
        match annotation with
        | Type.Parametric { name = primitive; _ }
        | Primitive primitive ->
            let reference =
              match Node.value (Type.expression (Type.Primitive primitive)) with
              | Expression.Name name when Expression.is_simple_name name ->
                  Expression.name_to_reference_exn name
              | _ -> Reference.create "typing.Any"
            in
            let module_definition =
              AstEnvironment.ReadOnly.get_module_metadata (ast_environment environment)
            in
            if Module.from_empty_stub ~reference ~module_definition then
              (), Type.Any
            else if
              ClassHierarchy.contains order primitive
              || Option.is_some (module_definition (Reference.create primitive))
            then
              (), annotation
            else
              let _ = Hash_set.add dependencies primitive in
              (), annotation
        | _ -> (), annotation
      in
      { Type.Transform.transformed_annotation; new_state }
  end)
  in
  let _, annotation = TrackedTransform.visit () value_annotation in
  if Hash_set.is_empty dependencies then
    Result.Ok
      {
        ResolvedAlias.qualifier;
        name = target_primitive_name;
        annotation = Type.TypeAlias annotation;
      }
  else
    Result.Error (Hash_set.to_list dependencies)


let register_aliases environment sources =
  Type.Cache.disable ();
  let register_aliases unresolved =
    let resolution_dependency = String.Table.create () in
    let worklist = Queue.create () in
    Queue.enqueue_all worklist unresolved;
    let rec fixpoint () =
      match Queue.dequeue worklist with
      | None -> ()
      | Some unresolved ->
          let _ =
            match resolve_alias environment unresolved with
            | Result.Error dependencies ->
                let add_dependency =
                  let update_dependency = function
                    | None -> [unresolved]
                    | Some entries -> unresolved :: entries
                  in
                  String.Table.update resolution_dependency ~f:update_dependency
                in
                List.iter dependencies ~f:add_dependency
            | Result.Ok ({ ResolvedAlias.name; _ } as resolved) -> (
                ResolvedAlias.register environment resolved;
                match Hashtbl.find resolution_dependency name with
                | Some entries ->
                    Queue.enqueue_all worklist entries;
                    Hashtbl.remove resolution_dependency name
                | None -> () )
          in
          fixpoint ()
    in
    fixpoint ()
  in
  List.concat_map ~f:(collect_aliases environment) sources |> register_aliases;
  Type.Cache.enable ()


let register_undecorated_functions _ (resolution : GlobalResolution.t) source =
  let module Visit = Visit.MakeStatementVisitor (struct
    type t = unit

    let visit_children = function
      | { Node.value = Statement.Define _; _ } ->
          (* inner functions are not globals *)
          false
      | _ -> true


    let statement _ _ { Node.value; location } =
      let register ~reference ~annotation =
        SharedMemory.UndecoratedFunctions.add reference annotation
      in
      match value with
      | Statement.Class definition -> (
          let annotation =
            AnnotatedClass.create { Node.value = definition; location }
            |> AnnotatedClass.inferred_callable_type ~resolution
          in
          match annotation with
          | Some { Type.Callable.implementation; overloads = []; _ } ->
              register ~reference:definition.Class.name ~annotation:implementation
          | _ -> () )
      | Define ({ Define.signature = { Define.Signature.name; _ }; _ } as define) ->
          if Define.is_overloaded_method define then
            ()
          else
            register
              ~reference:name
              ~annotation:(Annotated.Callable.create_overload ~resolution define)
      | _ -> ()
  end)
  in
  Visit.visit () source


let register_values
    environment
    (resolution : GlobalResolution.t)
    ({ Source.statements; qualifier; _ } as source)
  =
  let qualified_reference reference =
    let reference =
      let builtins = Reference.create "builtins" in
      if Reference.is_strict_prefix ~prefix:builtins reference then
        Reference.drop_prefix ~prefix:builtins reference
      else
        reference
    in
    Reference.sanitize_qualified reference
  in
  let module CollectCallables = Visit.MakeStatementVisitor (struct
    type t = Type.Callable.t Node.t list Reference.Map.t

    let visit_children = function
      | { Node.value = Statement.Define _; _ } ->
          (* inner functions are not globals *)
          false
      | _ -> true


    let statement { Source.qualifier; _ } callables statement =
      let collect_callable ~name callables callable =
        SharedMemoryDependencyHandler.add_function_key ~qualifier name;

        (* Register callable global. *)
        let change callable = function
          | None -> Some [callable]
          | Some existing -> Some (existing @ [callable])
        in
        Map.change callables name ~f:(change callable)
      in
      match statement with
      | {
       Node.location;
       value = Statement.Define ({ Define.signature = { name; parent; _ }; _ } as define);
      } ->
          let parent =
            if Define.is_class_method define then
              parent
              >>= fun reference -> Some (Type.Primitive (Reference.show reference)) >>| Type.meta
            else
              None
          in
          Annotated.Callable.apply_decorators ~resolution ~location define
          |> (fun overload -> [Define.is_overloaded_method define, overload])
          |> Annotated.Callable.create ~resolution ~parent ~name:(Reference.show name)
          |> Node.create ~location
          |> collect_callable ~name callables
      | _ -> callables
  end)
  in
  let register_callables ~key ~data =
    assert (not (List.is_empty data));
    let location = List.hd_exn data |> Node.location in
    data
    |> List.map ~f:Node.value
    |> Type.Callable.from_overloads
    >>| (fun callable -> Type.Callable callable)
    >>| Annotation.create_immutable ~global:true
    >>| Node.create ~location
    >>| (fun global -> register_global environment ~qualifier ~reference:key ~global)
    |> ignore
  in
  CollectCallables.visit Reference.Map.empty source |> Map.iteri ~f:register_callables;

  (* Register meta annotations for classes. *)
  let module Visit = Visit.MakeStatementVisitor (struct
    type t = unit

    let visit_children _ = true

    let statement { Source.qualifier; _ } _ = function
      | { Node.location; value = Statement.Class { Class.name; _ } } ->
          (* Register meta annotation. *)
          let primitive = Type.Primitive (Reference.show name) in
          let global =
            Annotation.create_immutable
              ~global:true
              ~original:(Some Type.Top)
              (Type.meta primitive)
            |> Node.create ~location
          in
          register_global environment ~qualifier ~reference:(qualified_reference name) ~global
      | _ -> ()
  end)
  in
  Visit.visit () source |> ignore;
  let rec visit statement =
    match statement with
    | { Node.value = Statement.If { If.body; orelse; _ }; _ } ->
        (* TODO(T28732125): Properly take an intersection here. *)
        List.iter ~f:visit body;
        List.iter ~f:visit orelse
    | { Node.value = Assign { Assign.target; annotation; value; _ }; _ } ->
        let explicit = Option.is_some annotation in
        let literal_annotation = GlobalResolution.resolve_literal resolution value in
        let annotation =
          annotation
          >>| Expression.delocalize
          >>| Type.create ~aliases:SharedMemory.Aliases.get
          >>= (fun annotation -> Option.some_if (not (Type.is_type_alias annotation)) annotation)
          |> Option.value ~default:literal_annotation
        in
        let rec register_assign ~target ~annotation =
          let register ~location reference annotation =
            let reference = qualified_reference (Reference.combine qualifier reference) in
            (* Don't register attributes or chained accesses as globals *)
            if Reference.length (Reference.drop_prefix ~prefix:qualifier reference) = 1 then
              let register_global global =
                Node.create ~location global
                |> fun global -> register_global environment ~qualifier ~reference ~global
              in
              let exists = Option.is_some (SharedMemory.Globals.get reference) in
              if explicit then
                Annotation.create_immutable ~global:true annotation |> register_global
              else if not exists then
                (* Treat literal globals as having been explicitly annotated. *)
                let original =
                  if Type.is_partially_typed annotation then Some Type.Top else None
                in
                Annotation.create_immutable ~global:true ~original annotation |> register_global
              else
                ()
          in
          match target.Node.value, annotation with
          | Name name, _ when Expression.is_simple_name name ->
              register
                ~location:target.Node.location
                (Expression.name_to_reference_exn name)
                annotation
          | Tuple elements, Type.Tuple (Type.Bounded (Concrete parameters))
            when List.length elements = List.length parameters ->
              List.map2_exn
                ~f:(fun target annotation -> register_assign ~target ~annotation)
                elements
                parameters
              |> ignore
          | Tuple elements, Type.Tuple (Type.Unbounded parameter) ->
              List.map ~f:(fun target -> register_assign ~target ~annotation:parameter) elements
              |> ignore
          | Tuple elements, _ ->
              List.map ~f:(fun target -> register_assign ~target ~annotation:Type.Top) elements
              |> ignore
          | _ -> ()
        in
        register_assign ~target ~annotation
    | _ -> ()
  in
  List.iter ~f:visit statements


let is_module { unannotated_global_environment; _ } =
  let ast_environment =
    UnannotatedGlobalEnvironment.ReadOnly.ast_environment unannotated_global_environment
  in
  AstEnvironment.ReadOnly.is_module ast_environment


let register_dependencies environment source =
  let module Visit = Visit.MakeStatementVisitor (struct
    type t = unit

    let visit_children _ = true

    let statement { Source.qualifier; _ } _ = function
      | { Node.value = Statement.Import { Import.from; imports }; _ } ->
          let imports =
            let imports =
              match from with
              | None ->
                  (* If analyzing `import a, b, c`, add `a`, `b`, `c` to the dependencies. *)
                  imports |> List.map ~f:(fun { Import.name; _ } -> name)
              | Some base_module ->
                  (* If analyzing `from x import a, b, c`, add `x`, `x.a`, `x.b`, `x.c` to the
                     dependencies, if they are module names. *)
                  base_module
                  :: List.map imports ~f:(fun { Import.name; _ } ->
                         Reference.combine base_module name)
                  |> List.filter ~f:(is_module environment)
            in
            let qualify_builtins import =
              match Reference.single import with
              | Some "builtins" -> Reference.empty
              | _ -> import
            in
            List.map imports ~f:qualify_builtins
          in
          let register dependency =
            Log.log
              ~section:`Dependencies
              "Adding dependency from %a to %a"
              Reference.pp
              dependency
              Reference.pp
              qualifier;
            SharedMemoryDependencyHandler.add_dependent ~qualifier dependency
          in
          List.iter ~f:register imports
      | _ -> ()
  end)
  in
  Visit.visit () source


let propagate_nested_classes ({ unannotated_global_environment; _ } as environment) source =
  let propagate ~qualifier ({ Class.name; _ } as definition) successors =
    let nested_class_names { Class.name; body; _ } =
      let extract_classes = function
        | { Node.value = Statement.Class { name = nested_name; _ }; _ } ->
            Some (Reference.drop_prefix nested_name ~prefix:name, nested_name)
        | _ -> None
      in
      List.filter_map ~f:extract_classes body
    in
    let create_alias added_sofar (stripped_name, full_name) =
      let alias = Reference.combine name stripped_name in
      if List.exists added_sofar ~f:(Reference.equal alias) then
        added_sofar
      else
        let primitive name = Type.Primitive (Reference.show name) in
        register_alias
          environment
          ~qualifier
          ~key:(Reference.show alias)
          ~data:(Type.TypeAlias (primitive full_name));
        alias :: added_sofar
    in
    let own_nested_classes = nested_class_names definition |> List.map ~f:snd in
    successors
    |> List.filter_map
         ~f:
           (UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
              unannotated_global_environment)
    |> List.map ~f:Node.value
    |> List.concat_map ~f:nested_class_names
    |> List.fold ~f:create_alias ~init:own_nested_classes
    |> ignore
  in
  let module Visit = Visit.MakeStatementVisitor (struct
    type t = unit

    let visit_children _ = true

    let statement { Source.qualifier; _ } _ = function
      | { Node.value = Statement.Class ({ Class.name; _ } as definition); _ } ->
          SharedMemory.ClassMetadata.get (Reference.show name)
          |> Option.iter ~f:(fun { GlobalResolution.successors; _ } ->
                 propagate ~qualifier definition successors)
      | _ -> ()
  end)
  in
  Visit.visit () source


let deduplicate_class_hierarchy = SharedMemoryClassHierarchyHandler.deduplicate

let remove_extra_edges_to_object = SharedMemoryClassHierarchyHandler.remove_extra_edges_to_object

let dependency_handler _ = (module SharedMemoryDependencyHandler : Dependencies.Handler)

let transaction _ ?(only_global_keys = false) ~f () =
  let open SharedMemory in
  if only_global_keys then
    GlobalKeys.LocalChanges.push_stack ()
  else (
    FunctionKeys.LocalChanges.push_stack ();
    AliasKeys.LocalChanges.push_stack ();
    GlobalKeys.LocalChanges.push_stack ();
    DependentKeys.LocalChanges.push_stack ();
    Dependents.LocalChanges.push_stack ();
    ClassMetadata.LocalChanges.push_stack ();
    Aliases.LocalChanges.push_stack ();
    OrderEdges.LocalChanges.push_stack ();
    OrderBackedges.LocalChanges.push_stack ();
    OrderKeys.LocalChanges.push_stack ();
    Globals.LocalChanges.push_stack () );
  let result = f () in
  if only_global_keys then
    GlobalKeys.LocalChanges.commit_all ()
  else (
    FunctionKeys.LocalChanges.commit_all ();
    AliasKeys.LocalChanges.commit_all ();
    GlobalKeys.LocalChanges.commit_all ();
    DependentKeys.LocalChanges.commit_all ();
    Dependents.LocalChanges.commit_all ();
    ClassMetadata.LocalChanges.commit_all ();
    Globals.LocalChanges.commit_all ();
    Aliases.LocalChanges.commit_all ();
    OrderEdges.LocalChanges.commit_all ();
    OrderBackedges.LocalChanges.commit_all ();
    OrderKeys.LocalChanges.commit_all () );
  if only_global_keys then
    GlobalKeys.LocalChanges.pop_stack ()
  else (
    FunctionKeys.LocalChanges.pop_stack ();
    AliasKeys.LocalChanges.pop_stack ();
    GlobalKeys.LocalChanges.pop_stack ();
    DependentKeys.LocalChanges.pop_stack ();
    Dependents.LocalChanges.pop_stack ();
    ClassMetadata.LocalChanges.pop_stack ();
    Globals.LocalChanges.pop_stack ();
    Aliases.LocalChanges.pop_stack ();
    OrderEdges.LocalChanges.pop_stack ();
    OrderBackedges.LocalChanges.pop_stack ();
    OrderKeys.LocalChanges.pop_stack () );
  result


let fill_shared_memory_with_default_typeorder _ =
  let object_primitive = "object" in
  let integer = "int" in
  let float = "float" in
  let default_annotations =
    [ (* Numerical hierarchy. *)
      [integer; float; "complex"; "numbers.Complex"; "numbers.Number"; object_primitive];
      [integer; "numbers.Integral"; object_primitive];
      [float; "numbers.Rational"; object_primitive];
      [float; "numbers.Real"; object_primitive] ]
  in
  let rec connect_primitive_chain annotations =
    match annotations with
    | predecessor :: successor :: rest ->
        SharedMemoryClassHierarchyHandler.connect ?parameters:None ~predecessor ~successor;
        connect_primitive_chain (successor :: rest)
    | _ -> ()
  in
  List.iter ~f:connect_primitive_chain default_annotations


let check_class_hierarchy_integrity ({ unannotated_global_environment; _ } as environment) =
  let indices =
    UnannotatedGlobalEnvironment.ReadOnly.all_classes unannotated_global_environment
    |> Type.Primitive.Set.of_list
    |> IndexTracker.indices
    |> IndexTracker.Set.to_list
  in
  ClassHierarchy.check_integrity (untracked_class_hierarchy_handler environment) ~indices


let purge environment ?(debug = false) (qualifiers : Reference.t list) ~update_result =
  let current_and_removed_classes =
    UnannotatedGlobalEnvironment.UpdateResult.current_classes_and_removed_classes update_result
  in
  let { SharedMemoryDependencyHandler.aliases; undecorated_signatures; globals } =
    SharedMemoryDependencyHandler.get_all_dependent_table_keys qualifiers
  in
  let open SharedMemory in
  (* Must disconnect backedges before deleting edges, since this relies on edges *)
  let index_keys = IndexTracker.indices current_and_removed_classes in
  SharedMemoryClassHierarchyHandler.disconnect_backedges index_keys;
  let caml_index_keys = IndexTracker.Set.to_list index_keys |> OrderEdges.KeySet.of_list in
  let caml_current_and_removed_classes =
    Type.Primitive.Set.to_list current_and_removed_classes |> ClassMetadata.KeySet.of_list
  in
  Globals.remove_batch globals;
  OrderEdges.remove_batch caml_index_keys;
  OrderBackedges.remove_batch caml_index_keys;
  ClassMetadata.remove_batch caml_current_and_removed_classes;
  Aliases.remove_batch aliases;
  UndecoratedFunctions.remove_batch undecorated_signatures;

  SharedMemoryDependencyHandler.remove_from_dependency_graph qualifiers;
  SharedMemoryDependencyHandler.clear_keys_batch qualifiers;

  if debug then (* If in debug mode, make sure the ClassHierarchy is still consistent. *)
    check_class_hierarchy_integrity environment


let update_and_compute_dependencies _ qualifiers ~update ~update_result =
  let current_and_removed_classes =
    UnannotatedGlobalEnvironment.UpdateResult.current_classes_and_removed_classes update_result
  in
  let old_dependent_table_keys =
    SharedMemoryDependencyHandler.get_all_dependent_table_keys qualifiers
  in
  let { SharedMemoryDependencyHandler.aliases; undecorated_signatures; globals } =
    old_dependent_table_keys
  in
  let open SharedMemory in
  (* Backedges are not tracked *)
  let index_keys = IndexTracker.indices current_and_removed_classes in
  SharedMemoryClassHierarchyHandler.disconnect_backedges index_keys;
  let caml_index_keys = IndexTracker.Set.to_list index_keys |> OrderEdges.KeySet.of_list in
  let caml_current_and_removed_classes =
    Type.Primitive.Set.to_list current_and_removed_classes |> ClassMetadata.KeySet.of_list
  in
  OrderBackedges.remove_batch caml_index_keys;

  SharedMemoryDependencyHandler.remove_from_dependency_graph qualifiers;
  SharedMemoryDependencyHandler.clear_keys_batch qualifiers;

  let update, mutation_triggers =
    SharedMemoryKeys.ReferenceDependencyKey.Transaction.empty
    |> Aliases.add_to_transaction ~keys:aliases
    |> ClassMetadata.add_to_transaction ~keys:caml_current_and_removed_classes
    |> UndecoratedFunctions.add_to_transaction ~keys:undecorated_signatures
    |> Globals.add_to_transaction ~keys:globals
    |> OrderEdges.add_to_transaction ~keys:caml_index_keys
    |> SharedMemoryKeys.ReferenceDependencyKey.Transaction.execute ~update
  in
  let { SharedMemoryDependencyHandler.aliases; undecorated_signatures; globals } =
    SharedMemoryDependencyHandler.find_added_dependent_table_keys
      qualifiers
      ~old_keys:old_dependent_table_keys
  in
  let mutation_and_addition_triggers =
    [ Aliases.get_all_dependents aliases;
      UndecoratedFunctions.get_all_dependents undecorated_signatures;
      Globals.get_all_dependents globals ]
    |> List.fold ~init:mutation_triggers ~f:SharedMemoryKeys.ReferenceDependencyKey.KeySet.union
  in
  update, mutation_and_addition_triggers


let shared_memory_handler unannotated_global_environment = { unannotated_global_environment }

let normalize_shared_memory qualifiers =
  (* Since we don't provide an API to the raw order keys in the type order handler, handle it
     inline here. *)
  let open SharedMemory in
  ( match OrderKeys.get Memory.SingletonKey.key with
  | None -> ()
  | Some keys ->
      OrderKeys.remove_batch (OrderKeys.KeySet.singleton Memory.SingletonKey.key);
      List.sort ~compare:IndexTracker.compare keys |> OrderKeys.add Memory.SingletonKey.key );
  SharedMemoryDependencyHandler.normalize qualifiers


let shared_memory_hash_to_key_map ~qualifiers () =
  let extend_map map ~new_map =
    Map.merge_skewed map new_map ~combine:(fun ~key:_ value _ -> value)
  in
  let open SharedMemory in
  (* Type order. *)
  let map =
    let order_keys = OrderKeys.find_unsafe Memory.SingletonKey.key in
    OrderKeys.compute_hashes_to_keys ~keys:[Memory.SingletonKey.key]
    |> extend_map ~new_map:(OrderEdges.compute_hashes_to_keys ~keys:order_keys)
    |> extend_map ~new_map:(OrderBackedges.compute_hashes_to_keys ~keys:order_keys)
  in
  (* Handle-based keys. *)
  let map =
    map
    |> extend_map ~new_map:(FunctionKeys.compute_hashes_to_keys ~keys:qualifiers)
    |> extend_map ~new_map:(GlobalKeys.compute_hashes_to_keys ~keys:qualifiers)
    |> extend_map ~new_map:(AliasKeys.compute_hashes_to_keys ~keys:qualifiers)
    |> extend_map ~new_map:(DependentKeys.compute_hashes_to_keys ~keys:qualifiers)
  in
  (* Aliases. *)
  let map =
    let keys = List.filter_map qualifiers ~f:AliasKeys.get |> List.concat in
    extend_map map ~new_map:(Aliases.compute_hashes_to_keys ~keys)
  in
  (* Globals and undecorated functions. *)
  let map =
    let keys = List.filter_map qualifiers ~f:GlobalKeys.get |> List.concat in
    extend_map map ~new_map:(Globals.compute_hashes_to_keys ~keys)
    |> extend_map ~new_map:(UndecoratedFunctions.compute_hashes_to_keys ~keys)
  in
  (* Dependents. *)
  let map =
    let keys = List.filter_map qualifiers ~f:DependentKeys.get |> List.concat in
    extend_map map ~new_map:(Dependents.compute_hashes_to_keys ~keys)
  in
  map


let serialize_decoded decoded =
  let decode index = IndexTracker.annotation index in
  let decode_target { ClassHierarchy.Target.target; parameters } =
    Format.asprintf "%s[%a]" (decode target) Type.OrderedTypes.pp_concise parameters
  in
  let open SharedMemory in
  match decoded with
  | ClassMetadata.Decoded (key, value) ->
      let value =
        match value with
        | Some { GlobalResolution.successors; is_test; is_final; extends_placeholder_stub_class }
          ->
            `Assoc
              [ "successors", `String (List.to_string ~f:Type.Primitive.show successors);
                "is_test", `Bool is_test;
                "is_final", `Bool is_final;
                "extends_placeholder_stub_class", `Bool extends_placeholder_stub_class ]
            |> Yojson.to_string
            |> Option.some
        | None -> None
      in
      Some (ClassMetadataValue.description, key, value)
  | Aliases.Decoded (key, value) -> Some (AliasValue.description, key, value >>| Type.show_alias)
  | Globals.Decoded (key, value) ->
      let value = value >>| Node.value >>| Annotation.sexp_of_t >>| Sexp.to_string in
      Some (GlobalValue.description, Reference.show key, value)
  | UndecoratedFunctions.Decoded (key, value) ->
      Some
        ( UndecoratedFunctionValue.description,
          Reference.show key,
          value >>| Type.Callable.show_overload Type.pp )
  | Dependents.Decoded (key, value) ->
      Some
        ( DependentValue.description,
          Reference.show key,
          value >>| Reference.Set.Tree.to_list >>| List.to_string ~f:Reference.show )
  | FunctionKeys.Decoded (key, value) ->
      Some
        ( FunctionKeyValue.description,
          Reference.show key,
          value >>| List.to_string ~f:Reference.show )
  | GlobalKeys.Decoded (key, value) ->
      Some
        (GlobalKeyValue.description, Reference.show key, value >>| List.to_string ~f:Reference.show)
  | AliasKeys.Decoded (key, value) ->
      Some (AliasKeyValue.description, Reference.show key, value >>| List.to_string ~f:ident)
  | DependentKeys.Decoded (key, value) ->
      Some
        ( DependentKeyValue.description,
          Reference.show key,
          value >>| List.to_string ~f:Reference.show )
  | OrderEdges.Decoded (key, value) ->
      Some (EdgeValue.description, decode key, value >>| List.to_string ~f:decode_target)
  | OrderBackedges.Decoded (key, value) ->
      Some
        ( BackedgeValue.description,
          decode key,
          value >>| ClassHierarchy.Target.Set.Tree.to_list >>| List.to_string ~f:decode_target )
  | OrderKeys.Decoded (key, value) ->
      Some (OrderKeyValue.description, Int.to_string key, value >>| List.to_string ~f:decode)
  | _ -> None


let decoded_equal first second =
  let open SharedMemory in
  match first, second with
  | ClassMetadata.Decoded (_, first), ClassMetadata.Decoded (_, second) ->
      Some (Option.equal GlobalResolution.equal_class_metadata first second)
  | Aliases.Decoded (_, first), Aliases.Decoded (_, second) ->
      Some (Option.equal Type.equal_alias first second)
  | Globals.Decoded (_, first), Globals.Decoded (_, second) ->
      Some (Option.equal Annotation.equal (first >>| Node.value) (second >>| Node.value))
  | UndecoratedFunctions.Decoded (_, first), UndecoratedFunctions.Decoded (_, second) ->
      Some (Option.equal (Type.Callable.equal_overload Type.equal) first second)
  | Dependents.Decoded (_, first), Dependents.Decoded (_, second) ->
      Some (Option.equal Reference.Set.Tree.equal first second)
  | FunctionKeys.Decoded (_, first), FunctionKeys.Decoded (_, second) ->
      Some (Option.equal (List.equal Reference.equal) first second)
  | GlobalKeys.Decoded (_, first), GlobalKeys.Decoded (_, second) ->
      Some (Option.equal (List.equal Reference.equal) first second)
  | AliasKeys.Decoded (_, first), AliasKeys.Decoded (_, second) ->
      Some (Option.equal (List.equal Identifier.equal) first second)
  | DependentKeys.Decoded (_, first), DependentKeys.Decoded (_, second) ->
      Some (Option.equal (List.equal Reference.equal) first second)
  | OrderEdges.Decoded (_, first), OrderEdges.Decoded (_, second) ->
      Some (Option.equal (List.equal ClassHierarchy.Target.equal) first second)
  | OrderBackedges.Decoded (_, first), OrderBackedges.Decoded (_, second) ->
      Some (Option.equal ClassHierarchy.Target.Set.Tree.equal first second)
  | OrderKeys.Decoded (_, first), OrderKeys.Decoded (_, second) ->
      Some (Option.equal (List.equal IndexTracker.equal) first second)
  | _ -> None


let indices { unannotated_global_environment } =
  UnannotatedGlobalEnvironment.ReadOnly.all_classes unannotated_global_environment
  |> Type.Primitive.Set.of_list
  |> IndexTracker.indices
  |> IndexTracker.Set.to_list


let class_hierarchy_json environment =
  ClassHierarchy.to_json
    (untracked_class_hierarchy_handler environment)
    ~indices:(indices environment)


let class_hierarchy_dot environment =
  ClassHierarchy.to_dot
    (untracked_class_hierarchy_handler environment)
    ~indices:(indices environment)
