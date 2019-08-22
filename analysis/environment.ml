(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Expression
open Pyre
open Statement

type t = { ast_environment: AstEnvironment.ReadOnly.t }

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

  module ClassKeyValue = struct
    type t = Identifier.t list

    let prefix = Prefix.make ()

    let description = "Class keys"

    let unmarshall value = Marshal.from_string value 0
  end

  module DependentKeyValue = struct
    type t = Reference.t list

    let prefix = Prefix.make ()

    let description = "Dependent keys"

    let unmarshall value = Marshal.from_string value 0
  end

  module ClassValue = struct
    type t = Statement.Class.t Node.t

    let prefix = Prefix.make ()

    let description = "Class"

    let unmarshall value = Marshal.from_string value 0

    let compare = Node.compare Statement.Class.compare
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

  module OrderIndexValue = struct
    type t = int

    let prefix = Prefix.make ()

    let description = "Order indices"

    let unmarshall value = Marshal.from_string value 0

    let compare = compare_int
  end

  module OrderAnnotationValue = struct
    type t = string

    let prefix = Prefix.make ()

    let description = "Order annotations"

    (* Strings are not marshalled by shared memory *)
    let unmarshall value = value

    let compare = compare_string
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
    type t = int list [@@deriving compare]

    let prefix = Prefix.make ()

    let description = "Order keys"

    let unmarshall value = Marshal.from_string value 0
  end

  module ModuleValue = struct
    type t = Module.t

    let prefix = Prefix.make ()

    let description = "Module"

    let unmarshall value = Marshal.from_string value 0

    let compare = Module.compare
  end

  module UndecoratedFunctionValue = struct
    type t = Type.t Type.Callable.overload [@@deriving compare]

    let prefix = Prefix.make ()

    let description = "Undecorated functions"

    let unmarshall value = Marshal.from_string value 0
  end

  module ClassDefinitions =
    Memory.DependencyTrackedTableWithCache
      (SharedMemoryKeys.StringKey)
      (SharedMemoryKeys.ReferenceDependencyKey)
      (ClassValue)
  (** Shared memory maps *)

  module Modules =
    Memory.DependencyTrackedTableWithCache
      (SharedMemoryKeys.ReferenceKey)
      (SharedMemoryKeys.ReferenceDependencyKey)
      (ModuleValue)
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

  module ClassKeys = Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (ClassKeyValue)
  module GlobalKeys = Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (GlobalKeyValue)
  module AliasKeys = Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (AliasKeyValue)
  module DependentKeys = Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (DependentKeyValue)

  module OrderIndices =
    Memory.DependencyTrackedTableWithCache
      (SharedMemoryKeys.StringKey)
      (SharedMemoryKeys.ReferenceDependencyKey)
      (OrderIndexValue)
  (** Type order maps *)

  module OrderAnnotations =
    Memory.DependencyTrackedTableWithCache
      (SharedMemoryKeys.IntKey)
      (SharedMemoryKeys.ReferenceDependencyKey)
      (OrderAnnotationValue)
  module OrderEdges =
    Memory.DependencyTrackedTableWithCache
      (SharedMemoryKeys.IntKey)
      (SharedMemoryKeys.ReferenceDependencyKey)
      (EdgeValue)
  module OrderBackedges = Memory.WithCache.Make (SharedMemoryKeys.IntKey) (BackedgeValue)
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


  let indices = OrderIndices.get ?dependency:None

  let set_indices ~key ~data =
    OrderIndices.remove_batch (OrderIndices.KeySet.singleton key);
    OrderIndices.add key data


  let annotations = OrderAnnotations.get ?dependency:None

  let set_annotations ~key ~data =
    OrderAnnotations.remove_batch (OrderAnnotations.KeySet.singleton key);
    OrderAnnotations.add key data


  let find_unsafe get key = Option.value_exn (get key)

  let contains get key = Option.is_some (get key)

  let add_key key =
    match OrderKeys.get Memory.SingletonKey.key with
    | None -> OrderKeys.add Memory.SingletonKey.key [key]
    | Some keys ->
        OrderKeys.remove_batch (OrderKeys.KeySet.singleton Memory.SingletonKey.key);
        OrderKeys.add Memory.SingletonKey.key (key :: keys)


  let keys () = Option.value ~default:[] (OrderKeys.get Memory.SingletonKey.key)

  let remove_keys removed =
    match OrderKeys.get Memory.SingletonKey.key with
    | None -> ()
    | Some keys ->
        OrderKeys.remove_batch (OrderKeys.KeySet.singleton Memory.SingletonKey.key);
        let remaining = OrderEdges.KeySet.diff (OrderEdges.KeySet.of_list keys) removed in
        OrderKeys.add Memory.SingletonKey.key (OrderEdges.KeySet.elements remaining)


  let insert annotation =
    if not (contains indices annotation) then (
      let index =
        let initial = Type.Primitive.hash annotation in
        let rec pick_index index =
          if contains annotations index then
            pick_index (index + 1)
          else
            index
        in
        pick_index initial
      in
      add_key index;
      set_indices ~key:annotation ~data:index;
      set_annotations ~key:index ~data:annotation;
      set_edges ~key:index ~data:[];
      set_backedges ~key:index ~data:ClassHierarchy.Target.Set.empty )


  let connect ?(parameters = Type.OrderedTypes.Concrete []) ~predecessor ~successor =
    if (not (contains indices predecessor)) || not (contains indices successor) then
      Statistics.event
        ~name:"invalid type order connection"
        ~integers:[]
        ~normals:["Predecessor", predecessor; "Successor", successor]
        ()
    else
      let index_of annotation = find_unsafe indices annotation in
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


  let disconnect_backedges purged_annotations =
    let keys_to_remove = List.filter_map purged_annotations ~f:indices |> Int.Hash_set.of_list in
    let all_successors =
      let all_successors = Int.Hash_set.create () in
      let add_successors key =
        match edges key with
        | Some successors ->
            List.iter successors ~f:(fun { ClassHierarchy.Target.target; _ } ->
                Hash_set.add all_successors target)
        | None -> ()
      in
      Hash_set.iter keys_to_remove ~f:add_successors;
      all_successors
    in
    let remove_backedges successor =
      backedges successor
      >>| (fun current_predecessors ->
            let new_predecessors =
              Set.filter
                ~f:(fun { ClassHierarchy.Target.target; _ } ->
                  not (Hash_set.mem keys_to_remove target))
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
            ListOrSet.fold found ~f:keep_first ~init:(Int.Set.empty, ListOrSet.empty) |> snd
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
    annotations |> List.map ~f:(find_unsafe indices) |> List.iter ~f:deduplicate_annotation


  let remove_extra_edges_to_object annotations =
    let index_of annotation = find_unsafe indices annotation in
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
    let removed_indices = List.concat_map ~f:remove_extra_references keys |> Int.Set.of_list in
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


  let add_class_key ~qualifier class_type =
    add_new_key ~qualifier ~key:class_type ~get:ClassKeys.get ~add:ClassKeys.add


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

  let get_class_keys ~qualifier = ClassKeys.get qualifier |> Option.value ~default:[]

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
    ClassKeys.remove_batch (ClassKeys.KeySet.of_list qualifiers);
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
      ( match ClassKeys.get qualifier with
      | Some keys ->
          ClassKeys.remove_batch (ClassKeys.KeySet.singleton qualifier);
          ClassKeys.add qualifier (List.dedup_and_sort ~compare:Identifier.compare keys)
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
    modules: SharedMemory.Modules.KeySet.t;
    class_definitions: SharedMemory.ClassDefinitions.KeySet.t;
    class_metadata: SharedMemory.ClassMetadata.KeySet.t;
    undecorated_signatures: SharedMemory.UndecoratedFunctions.KeySet.t;
    globals: SharedMemory.Globals.KeySet.t;
    edges: SharedMemory.OrderEdges.KeySet.t;
    backedges: SharedMemory.OrderBackedges.KeySet.t;
    indices: SharedMemory.OrderIndices.KeySet.t;
    annotations: SharedMemory.OrderAnnotations.KeySet.t;
  }

  let get_all_dependent_table_keys qualifiers =
    let alias_keys = List.concat_map ~f:(fun qualifier -> get_alias_keys ~qualifier) qualifiers in
    let class_keys = List.concat_map ~f:(fun qualifier -> get_class_keys ~qualifier) qualifiers in
    let index_keys = List.filter_map class_keys ~f:SharedMemoryClassHierarchyHandler.indices in
    let global_keys =
      List.concat_map ~f:(fun qualifier -> get_global_keys ~qualifier) qualifiers
    in
    let function_keys =
      List.concat_map ~f:(fun qualifier -> get_function_keys ~qualifier) qualifiers
    in
    let open SharedMemory in
    {
      aliases = Aliases.KeySet.of_list alias_keys;
      modules = Modules.KeySet.of_list qualifiers;
      class_definitions = ClassDefinitions.KeySet.of_list class_keys;
      class_metadata = ClassMetadata.KeySet.of_list class_keys;
      undecorated_signatures = UndecoratedFunctions.KeySet.of_list global_keys;
      (* We add a global name for each function definition as well. *)
      globals = Globals.KeySet.of_list (global_keys @ function_keys);
      edges = OrderEdges.KeySet.of_list index_keys;
      backedges = OrderBackedges.KeySet.of_list index_keys;
      indices = OrderIndices.KeySet.of_list class_keys;
      annotations = OrderAnnotations.KeySet.of_list index_keys;
    }


  let find_added_dependent_table_keys qualifiers ~old_keys =
    let new_keys = get_all_dependent_table_keys qualifiers in
    {
      aliases = Aliases.KeySet.diff new_keys.aliases old_keys.aliases;
      modules = Modules.KeySet.diff new_keys.modules old_keys.modules;
      class_definitions =
        ClassDefinitions.KeySet.diff new_keys.class_definitions old_keys.class_definitions;
      class_metadata = ClassMetadata.KeySet.diff new_keys.class_metadata old_keys.class_metadata;
      undecorated_signatures =
        UndecoratedFunctions.KeySet.diff
          new_keys.undecorated_signatures
          old_keys.undecorated_signatures;
      globals = Globals.KeySet.diff new_keys.globals old_keys.globals;
      edges = OrderEdges.KeySet.diff new_keys.edges old_keys.edges;
      backedges = OrderBackedges.KeySet.diff new_keys.backedges old_keys.backedges;
      indices = OrderIndices.KeySet.diff new_keys.indices old_keys.indices;
      annotations = OrderAnnotations.KeySet.diff new_keys.annotations old_keys.annotations;
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

let connect_annotations_to_object annotations =
  let indices = SharedMemoryClassHierarchyHandler.indices in
  let connect_to_top annotation =
    let index = SharedMemoryClassHierarchyHandler.find_unsafe indices annotation in
    let annotation =
      SharedMemoryClassHierarchyHandler.find_unsafe
        SharedMemoryClassHierarchyHandler.annotations
        index
    in
    let object_primitive = "object" in
    if
      not
        (ClassHierarchy.is_transitive_successor
           (module SharedMemoryClassHierarchyHandler)
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


let resolution_implementation ?dependency { ast_environment } () =
  GlobalResolution.create
    ~ast_environment
    ~aliases:(SharedMemory.Aliases.get ?dependency)
    ~module_definition:(SharedMemory.Modules.get ?dependency)
    ~class_definition:(SharedMemory.ClassDefinitions.get ?dependency)
    ~class_metadata:(SharedMemory.ClassMetadata.get ?dependency)
    ~undecorated_signature:(SharedMemory.UndecoratedFunctions.get ?dependency)
    ~global:(SharedMemory.Globals.get ?dependency)
    ~edges:(SharedMemory.OrderEdges.get ?dependency)
    ~backedges:SharedMemory.OrderBackedges.get
    ~indices:(SharedMemory.OrderIndices.get ?dependency)
    ~annotations:(SharedMemory.OrderAnnotations.get ?dependency)
    (module Annotated.Class)


let resolution = resolution_implementation ?dependency:None

let dependency_tracked_resolution environment ~dependency () =
  resolution_implementation ~dependency environment ()


let ast_environment { ast_environment } = ast_environment

let connect_definition
    _
    ~(resolution : GlobalResolution.t)
    ~definition:({ Node.value = { Class.name; bases; _ }; _ } as definition)
  =
  let annotated = Annotated.Class.create definition in
  (* We have to split the type here due to our built-in aliasing. Namely, the "list" and "dict"
     classes get expanded into parametric types of List[Any] and Dict[Any, Any]. *)
  let connect ~predecessor ~successor ~parameters =
    let annotations_tracked =
      SharedMemoryClassHierarchyHandler.contains
        SharedMemoryClassHierarchyHandler.indices
        predecessor
      && SharedMemoryClassHierarchyHandler.contains
           SharedMemoryClassHierarchyHandler.indices
           successor
    in
    let primitive_cycle =
      (* Primitive cycles can be introduced by meta-programming. *)
      String.equal predecessor successor
    in
    if annotations_tracked && not primitive_cycle then
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
          when not (ClassHierarchy.contains (module SharedMemoryClassHierarchyHandler) primitive)
          ->
            Log.log ~section:`Environment "Superclass annotation %a is missing" Type.pp supertype
        | Type.Primitive supertype ->
            connect ~predecessor:primitive ~successor:supertype ~parameters
        | _ ->
            Log.log ~section:`Environment "Superclass annotation %a is missing" Type.pp supertype )
    | _ -> ()
  in
  let inferred_base = Annotated.Class.inferred_generic_base annotated ~resolution in
  inferred_base @ bases
  (* Don't register metaclass=abc.ABCMeta, etc. superclasses. *)
  |> List.filter ~f:(fun { Expression.Call.Argument.name; _ } -> Option.is_none name)
  |> List.iter ~f:register_supertype


let register_class_metadata _ class_name =
  let open SharedMemory in
  let open Statement in
  let successors =
    ClassHierarchy.successors (module SharedMemoryClassHierarchyHandler) class_name
  in
  let is_final =
    ClassDefinitions.get class_name
    >>| (fun { Node.value = definition; _ } -> Class.is_final definition)
    |> Option.value ~default:false
  in
  let in_test =
    let is_unit_test { Node.value = definition; _ } = Class.is_unit_test definition in
    let successor_classes = List.filter_map ~f:ClassDefinitions.get successors in
    List.exists ~f:is_unit_test successor_classes
  in
  let extends_placeholder_stub_class =
    ClassDefinitions.get class_name
    >>| Annotated.Class.create
    >>| Annotated.Class.extends_placeholder_stub_class
          ~aliases:SharedMemory.Aliases.get
          ~module_definition:SharedMemory.Modules.get
    |> Option.value ~default:false
  in
  ClassMetadata.add
    class_name
    { GlobalResolution.is_test = in_test; successors; is_final; extends_placeholder_stub_class }


let set_class_definition _ ~name ~definition =
  let open SharedMemory in
  let definition =
    match ClassDefinitions.get name with
    | Some { Node.location; value = preexisting } ->
        {
          Node.location;
          value = Statement.Class.update preexisting ~definition:(Node.value definition);
        }
    | _ -> definition
  in
  ClassDefinitions.add name definition


let add_special_classes environment =
  (* Add classes for `typing.Optional` and `typing.Undeclared` that are currently not encoded in
     the stubs. *)
  let add_special_class ~name ~bases ~metaclasses ~body =
    let definition =
      let create_base annotation =
        { Expression.Call.Argument.name = None; value = Type.expression annotation }
      in
      let create_metaclass annotation =
        {
          Expression.Call.Argument.name = Some (Node.create_with_default_location "metaclass");
          value = Type.expression annotation;
        }
      in
      {
        Class.name = Reference.create name;
        bases = List.map bases ~f:create_base @ List.map metaclasses ~f:create_metaclass;
        body;
        decorators = [];
        docstring = None;
      }
    in
    set_class_definition
      ast_environment
      ~name
      ~definition:(Node.create_with_default_location definition);
    SharedMemoryClassHierarchyHandler.insert name;
    register_class_metadata environment name
  in
  let t_self_expression = Name (Name.Identifier "TSelf") |> Node.create_with_default_location in
  List.iter
    ~f:(fun (name, bases, metaclasses, body) -> add_special_class ~name ~bases ~metaclasses ~body)
    [ "None", [], [], [];
      "typing.Optional", [], [], [];
      "typing.Undeclared", [], [], [];
      "typing.NoReturn", [], [], [];
      ( "typing.Type",
        [Type.parametric "typing.Generic" (Concrete [Type.variable "typing._T"])],
        [],
        [] );
      ( "typing.GenericMeta",
        [],
        [],
        [ Define
            {
              signature =
                {
                  name = Reference.create "typing.GenericMeta.__getitem__";
                  parameters =
                    [ { Parameter.name = "cls"; value = None; annotation = None }
                      |> Node.create_with_default_location;
                      { Parameter.name = "arg"; value = None; annotation = None }
                      |> Node.create_with_default_location ];
                  decorators = [];
                  docstring = None;
                  return_annotation = None;
                  async = false;
                  parent = Some (Reference.create "typing.GenericMeta");
                };
              body = [];
            }
          |> Node.create_with_default_location ] );
      "typing.Generic", [], [Type.Primitive "typing.GenericMeta"], [];
      ( "TypedDictionary",
        [Type.parametric "typing.Mapping" (Concrete [Type.string; Type.Any])],
        [],
        Type.TypedDictionary.defines ~t_self_expression ~total:true );
      ( "NonTotalTypedDictionary",
        [Type.Primitive "TypedDictionary"],
        [],
        Type.TypedDictionary.defines ~t_self_expression ~total:false ) ]


let add_dummy_modules _ =
  (* Register dummy module for `builtins` and `future.builtins`. *)
  let builtins = Reference.create "builtins" in
  SharedMemory.Modules.add builtins (Ast.Module.create_implicit ~empty_stub:true ());
  let future_builtins = Reference.create "future.builtins" in
  SharedMemory.Modules.add future_builtins (Ast.Module.create_implicit ~empty_stub:true ())


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

let register_module _ ({ Source.qualifier; _ } as source) =
  SharedMemory.Modules.add qualifier (Module.create source)


let register_implicit_submodules _ qualifier =
  let rec register_submodules = function
    | None -> ()
    | Some qualifier ->
        let register () =
          let open SharedMemory in
          match Modules.get qualifier with
          | Some _ -> ()
          | None -> Modules.add qualifier (Module.create_implicit ())
        in
        register ();
        register_submodules (Reference.prefix qualifier)
  in
  register_submodules (Reference.prefix qualifier)


let register_class_definitions environment source =
  let order = (module SharedMemoryClassHierarchyHandler : ClassHierarchy.Handler) in
  let module Visit = Visit.MakeStatementVisitor (struct
    type t = Type.Primitive.Set.t

    let visit_children _ = true

    let statement { Source.qualifier; _ } new_annotations = function
      | { Node.location; value = Class ({ Class.name; _ } as definition) } ->
          let name = Reference.show name in
          let primitive = name in
          SharedMemoryDependencyHandler.add_class_key ~qualifier name;
          set_class_definition environment ~name ~definition:{ Node.location; value = definition };
          if not (ClassHierarchy.contains order primitive) then
            SharedMemoryClassHierarchyHandler.insert primitive;
          Set.add new_annotations primitive
      | _ -> new_annotations
  end)
  in
  Visit.visit Type.Primitive.Set.empty source


let collect_aliases environment { Source.statements; qualifier; _ } =
  let rec visit_statement ~qualifier ?(in_class_body = false) aliases { Node.value; _ } =
    match value with
    | Assign { Assign.target = { Node.value = Name target; _ }; annotation; value; _ }
      when Expression.is_simple_name target -> (
        let target =
          let target = Expression.name_to_reference_exn target |> Reference.sanitize_qualified in
          if in_class_body then target else Reference.combine qualifier target
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
            match Type.Variable.parse_declaration value with
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
    | Class { Class.name; body; _ } ->
        List.fold body ~init:aliases ~f:(visit_statement ~qualifier:name ~in_class_body:true)
    | Import { Import.from = Some _; imports = [{ Import.name; _ }] }
      when Reference.show name = "*" ->
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
            ClassHierarchy.contains
              (module SharedMemoryClassHierarchyHandler)
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


let resolve_alias _ { UnresolvedAlias.qualifier; target; value } =
  let order = (module SharedMemoryClassHierarchyHandler : ClassHierarchy.Handler) in
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
            let module_definition = SharedMemory.Modules.get in
            if Module.from_empty_stub ~reference ~module_definition then
              (), Type.Any
            else if
              ClassHierarchy.contains order primitive
              || SharedMemory.Modules.mem (Reference.create primitive)
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
      | { Node.value = Define _; _ } ->
          (* inner functions are not globals *)
          false
      | _ -> true


    let statement _ _ { Node.value; location } =
      let register ~reference ~annotation =
        SharedMemory.UndecoratedFunctions.add reference annotation
      in
      match value with
      | Class definition -> (
          let annotation =
            AnnotatedClass.create { Node.value = definition; location }
            |> AnnotatedClass.inferred_callable_type ~resolution
          in
          match annotation with
          | Some { Type.Callable.implementation; overloads = []; _ } ->
              register ~reference:definition.Class.name ~annotation:implementation
          | _ -> () )
      | Define ({ Define.signature = { Statement.Define.name; _ }; _ } as define) ->
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
      | { Node.value = Define _; _ } ->
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
       value = Define ({ Statement.Define.signature = { name; parent; _ }; _ } as define);
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
      | { Node.location; value = Class { Class.name; _ } } ->
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
    | { Node.value = If { If.body; orelse; _ }; _ } ->
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


let connect_type_order environment resolution source =
  let module Visit = Visit.MakeStatementVisitor (struct
    type t = unit

    let visit_children _ = true

    let statement _ _ = function
      | { Node.location; value = Class definition } ->
          connect_definition environment ~resolution ~definition:(Node.create ~location definition)
      | _ -> ()
  end)
  in
  Visit.visit () source |> ignore


let register_dependencies _ source =
  let module Visit = Visit.MakeStatementVisitor (struct
    type t = unit

    let visit_children _ = true

    let statement { Source.qualifier; _ } _ = function
      | { Node.value = Import { Import.from; imports }; _ } ->
          let imports =
            let imports =
              match from with
              (* If analyzing from x import y, only add x to the dependencies. Otherwise, add all
                 dependencies. *)
              | None -> imports |> List.map ~f:(fun { Import.name; _ } -> name)
              | Some base_module -> [base_module]
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


let propagate_nested_classes environment source =
  let propagate ~qualifier ({ Statement.Class.name; _ } as definition) successors =
    let nested_class_names { Statement.Class.name; body; _ } =
      let extract_classes = function
        | { Node.value = Class { name = nested_name; _ }; _ } ->
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
    |> List.filter_map ~f:SharedMemory.ClassDefinitions.get
    |> List.map ~f:Node.value
    |> List.concat_map ~f:nested_class_names
    |> List.fold ~f:create_alias ~init:own_nested_classes
    |> ignore
  in
  let module Visit = Visit.MakeStatementVisitor (struct
    type t = unit

    let visit_children _ = true

    let statement { Source.qualifier; _ } _ = function
      | { Node.value = Class ({ Class.name; _ } as definition); _ } ->
          SharedMemory.ClassMetadata.get (Reference.show name)
          |> Option.iter ~f:(fun { GlobalResolution.successors; _ } ->
                 propagate ~qualifier definition successors)
      | _ -> ()
  end)
  in
  Visit.visit () source


let built_in_annotations =
  ["TypedDictionary"; "NonTotalTypedDictionary"] |> Type.Primitive.Set.of_list


let is_module _ = SharedMemory.Modules.mem

let deduplicate_class_hierarchy = SharedMemoryClassHierarchyHandler.deduplicate

let remove_extra_edges_to_object = SharedMemoryClassHierarchyHandler.remove_extra_edges_to_object

let dependency_handler _ = (module SharedMemoryDependencyHandler : Dependencies.Handler)

let transaction _ ?(only_global_keys = false) ~f () =
  let open SharedMemory in
  if only_global_keys then
    GlobalKeys.LocalChanges.push_stack ()
  else (
    Modules.LocalChanges.push_stack ();
    FunctionKeys.LocalChanges.push_stack ();
    ClassKeys.LocalChanges.push_stack ();
    AliasKeys.LocalChanges.push_stack ();
    GlobalKeys.LocalChanges.push_stack ();
    DependentKeys.LocalChanges.push_stack ();
    Dependents.LocalChanges.push_stack ();
    ClassDefinitions.LocalChanges.push_stack ();
    ClassMetadata.LocalChanges.push_stack ();
    Aliases.LocalChanges.push_stack ();
    OrderEdges.LocalChanges.push_stack ();
    OrderBackedges.LocalChanges.push_stack ();
    OrderAnnotations.LocalChanges.push_stack ();
    OrderKeys.LocalChanges.push_stack ();
    OrderIndices.LocalChanges.push_stack ();
    Globals.LocalChanges.push_stack () );
  let result = f () in
  if only_global_keys then
    GlobalKeys.LocalChanges.commit_all ()
  else (
    Modules.LocalChanges.commit_all ();
    FunctionKeys.LocalChanges.commit_all ();
    ClassKeys.LocalChanges.commit_all ();
    AliasKeys.LocalChanges.commit_all ();
    GlobalKeys.LocalChanges.commit_all ();
    DependentKeys.LocalChanges.commit_all ();
    Dependents.LocalChanges.commit_all ();
    ClassDefinitions.LocalChanges.commit_all ();
    ClassMetadata.LocalChanges.commit_all ();
    Globals.LocalChanges.commit_all ();
    Aliases.LocalChanges.commit_all ();
    OrderEdges.LocalChanges.commit_all ();
    OrderBackedges.LocalChanges.commit_all ();
    OrderAnnotations.LocalChanges.commit_all ();
    OrderKeys.LocalChanges.commit_all ();
    OrderIndices.LocalChanges.commit_all () );
  if only_global_keys then
    GlobalKeys.LocalChanges.pop_stack ()
  else (
    Modules.LocalChanges.pop_stack ();
    FunctionKeys.LocalChanges.pop_stack ();
    ClassKeys.LocalChanges.pop_stack ();
    AliasKeys.LocalChanges.pop_stack ();
    GlobalKeys.LocalChanges.pop_stack ();
    DependentKeys.LocalChanges.pop_stack ();
    Dependents.LocalChanges.pop_stack ();
    ClassDefinitions.LocalChanges.pop_stack ();
    ClassMetadata.LocalChanges.pop_stack ();
    Globals.LocalChanges.pop_stack ();
    Aliases.LocalChanges.pop_stack ();
    OrderEdges.LocalChanges.pop_stack ();
    OrderBackedges.LocalChanges.pop_stack ();
    OrderAnnotations.LocalChanges.pop_stack ();
    OrderKeys.LocalChanges.pop_stack ();
    OrderIndices.LocalChanges.pop_stack () );
  result


let fill_shared_memory_with_default_typeorder () =
  let object_primitive = "object" in
  let generic_primitive = "typing.Generic" in
  let integer = "int" in
  let float = "float" in
  let complex = "complex" in
  let default_annotations =
    let singleton annotation = [annotation; object_primitive] in
    [ [object_primitive];
      (* Special forms *)
      singleton "typing.Annotated";
      singleton "typing.Tuple";
      singleton "typing.NamedTuple";
      singleton generic_primitive;
      singleton "typing.GenericMeta";
      singleton "typing.Protocol";
      singleton "typing.Callable";
      singleton "typing.FrozenSet";
      singleton "typing.Optional";
      singleton "typing.TypeVar";
      singleton "typing.Undeclared";
      singleton "typing.Union";
      singleton "typing.NoReturn";
      (* Ensure unittest.mock.Base is there because we check against it. *)
      singleton "unittest.mock.Base";
      singleton "unittest.mock.NonCallableMock";
      singleton "typing.ClassVar";
      singleton "typing.Final";
      singleton "typing_extensions.Final";
      singleton "typing_extensions.Literal";
      ["dict"; "typing.Dict"; object_primitive];
      singleton "None";
      (* Numerical hierarchy. *)
      [integer; float; complex; "numbers.Complex"; "numbers.Number"; object_primitive];
      [integer; "numbers.Integral"; object_primitive];
      [float; "numbers.Rational"; object_primitive];
      [float; "numbers.Real"; object_primitive] ]
  in
  let builtin_types = List.concat default_annotations |> Type.Primitive.Set.of_list in
  let insert = SharedMemoryClassHierarchyHandler.insert in
  let connect = SharedMemoryClassHierarchyHandler.connect in
  Set.iter builtin_types ~f:insert;
  let rec connect_primitive_chain annotations =
    match annotations with
    | predecessor :: successor :: rest ->
        connect ?parameters:None ~predecessor ~successor;
        connect_primitive_chain (successor :: rest)
    | _ -> ()
  in
  List.iter ~f:connect_primitive_chain default_annotations;

  (* Since the builtin type hierarchy is not primitive, it's special cased. *)
  let type_builtin = "type" in
  let type_variable = Type.Variable (Type.Variable.Unary.create "_T") in
  insert type_builtin;
  connect
    ~predecessor:type_builtin
    ~parameters:(Concrete [type_variable])
    ~successor:generic_primitive;
  let typed_dictionary = "TypedDictionary" in
  let non_total_typed_dictionary = "NonTotalTypedDictionary" in
  let typing_mapping = "typing.Mapping" in
  insert non_total_typed_dictionary;
  insert typed_dictionary;
  insert typing_mapping;
  connect ?parameters:None ~predecessor:non_total_typed_dictionary ~successor:typed_dictionary;
  connect
    ~predecessor:typed_dictionary
    ~parameters:(Concrete [Type.string; Type.Any])
    ~successor:typing_mapping


let check_class_hierarchy_integrity () =
  ClassHierarchy.check_integrity
    (module SharedMemoryClassHierarchyHandler)
    ~indices:(SharedMemoryClassHierarchyHandler.keys ())


let purge _ ?(debug = false) (qualifiers : Reference.t list) =
  let {
    SharedMemoryDependencyHandler.aliases;
    modules;
    class_definitions;
    class_metadata;
    undecorated_signatures;
    globals;
    edges;
    backedges;
    indices;
    annotations;
  }
    =
    SharedMemoryDependencyHandler.get_all_dependent_table_keys qualifiers
  in
  let open SharedMemory in
  (* Must disconnect backedges before deleting edges, since this relies on edges *)
  SharedMemoryClassHierarchyHandler.disconnect_backedges (OrderIndices.KeySet.elements indices);

  Globals.remove_batch globals;
  OrderIndices.remove_batch indices;
  OrderAnnotations.remove_batch annotations;
  OrderEdges.remove_batch edges;
  OrderBackedges.remove_batch backedges;
  ClassDefinitions.remove_batch class_definitions;
  ClassMetadata.remove_batch class_metadata;
  Aliases.remove_batch aliases;
  UndecoratedFunctions.remove_batch undecorated_signatures;
  Modules.remove_batch modules;

  SharedMemoryClassHierarchyHandler.remove_keys annotations;

  SharedMemoryDependencyHandler.remove_from_dependency_graph qualifiers;
  SharedMemoryDependencyHandler.clear_keys_batch qualifiers;

  if debug then (* If in debug mode, make sure the ClassHierarchy is still consistent. *)
    check_class_hierarchy_integrity ()


let update_and_compute_dependencies _ qualifiers ~update =
  let old_dependent_table_keys =
    SharedMemoryDependencyHandler.get_all_dependent_table_keys qualifiers
  in
  let {
    SharedMemoryDependencyHandler.aliases;
    modules;
    class_definitions;
    class_metadata;
    undecorated_signatures;
    globals;
    edges;
    backedges;
    indices;
    annotations;
  }
    =
    old_dependent_table_keys
  in
  let open SharedMemory in
  (* Backedges are not tracked *)
  SharedMemoryClassHierarchyHandler.disconnect_backedges (OrderIndices.KeySet.elements indices);
  OrderBackedges.remove_batch backedges;

  SharedMemoryClassHierarchyHandler.remove_keys annotations;

  SharedMemoryDependencyHandler.remove_from_dependency_graph qualifiers;
  SharedMemoryDependencyHandler.clear_keys_batch qualifiers;

  let update, mutation_triggers =
    SharedMemoryKeys.ReferenceDependencyKey.Transaction.empty
    |> Aliases.add_to_transaction ~keys:aliases
    |> Modules.add_to_transaction ~keys:modules
    |> ClassDefinitions.add_to_transaction ~keys:class_definitions
    |> ClassMetadata.add_to_transaction ~keys:class_metadata
    |> UndecoratedFunctions.add_to_transaction ~keys:undecorated_signatures
    |> Globals.add_to_transaction ~keys:globals
    |> OrderEdges.add_to_transaction ~keys:edges
    |> OrderIndices.add_to_transaction ~keys:indices
    |> OrderAnnotations.add_to_transaction ~keys:annotations
    |> SharedMemoryKeys.ReferenceDependencyKey.Transaction.execute ~update
  in
  let {
    SharedMemoryDependencyHandler.aliases;
    modules;
    class_definitions;
    class_metadata;
    undecorated_signatures;
    globals;
    edges;
    backedges = _;
    indices;
    annotations;
  }
    =
    SharedMemoryDependencyHandler.find_added_dependent_table_keys
      qualifiers
      ~old_keys:old_dependent_table_keys
  in
  let mutation_and_addition_triggers =
    [ Aliases.get_all_dependents aliases;
      Modules.get_all_dependents modules;
      ClassDefinitions.get_all_dependents class_definitions;
      ClassMetadata.get_all_dependents class_metadata;
      UndecoratedFunctions.get_all_dependents undecorated_signatures;
      Globals.get_all_dependents globals;
      OrderEdges.get_all_dependents edges;
      OrderIndices.get_all_dependents indices;
      OrderAnnotations.get_all_dependents annotations ]
    |> List.fold ~init:mutation_triggers ~f:SharedMemoryKeys.ReferenceDependencyKey.KeySet.union
  in
  update, mutation_and_addition_triggers


let shared_memory_handler ast_environment = { ast_environment }

let normalize_shared_memory qualifiers =
  (* Since we don't provide an API to the raw order keys in the type order handler, handle it
     inline here. *)
  let open SharedMemory in
  ( match OrderKeys.get Memory.SingletonKey.key with
  | None -> ()
  | Some keys ->
      OrderKeys.remove_batch (OrderKeys.KeySet.singleton Memory.SingletonKey.key);
      List.sort ~compare:Int.compare keys |> OrderKeys.add Memory.SingletonKey.key );
  SharedMemoryDependencyHandler.normalize qualifiers


let shared_memory_hash_to_key_map ~qualifiers () =
  let extend_map map ~new_map =
    Map.merge_skewed map new_map ~combine:(fun ~key:_ value _ -> value)
  in
  let open SharedMemory in
  (* Type order. *)
  let map =
    let order_keys = OrderKeys.find_unsafe Memory.SingletonKey.key in
    let index_keys =
      OrderAnnotations.get_batch (OrderAnnotations.KeySet.of_list order_keys)
      |> OrderAnnotations.KeyMap.values
      |> List.filter_opt
    in
    OrderKeys.compute_hashes_to_keys ~keys:[Memory.SingletonKey.key]
    |> extend_map ~new_map:(OrderAnnotations.compute_hashes_to_keys ~keys:order_keys)
    |> extend_map ~new_map:(OrderIndices.compute_hashes_to_keys ~keys:index_keys)
    |> extend_map ~new_map:(OrderEdges.compute_hashes_to_keys ~keys:order_keys)
    |> extend_map ~new_map:(OrderBackedges.compute_hashes_to_keys ~keys:order_keys)
  in
  let map = extend_map map ~new_map:(Modules.compute_hashes_to_keys ~keys:qualifiers) in
  (* Handle-based keys. *)
  let map =
    map
    |> extend_map ~new_map:(FunctionKeys.compute_hashes_to_keys ~keys:qualifiers)
    |> extend_map ~new_map:(ClassKeys.compute_hashes_to_keys ~keys:qualifiers)
    |> extend_map ~new_map:(GlobalKeys.compute_hashes_to_keys ~keys:qualifiers)
    |> extend_map ~new_map:(AliasKeys.compute_hashes_to_keys ~keys:qualifiers)
    |> extend_map ~new_map:(DependentKeys.compute_hashes_to_keys ~keys:qualifiers)
  in
  (* Class definitions. *)
  let map =
    let keys = List.filter_map qualifiers ~f:ClassKeys.get |> List.concat in
    extend_map map ~new_map:(ClassDefinitions.compute_hashes_to_keys ~keys)
    |> extend_map ~new_map:(ClassMetadata.compute_hashes_to_keys ~keys)
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
  let decode index =
    let annotation = SharedMemoryClassHierarchyHandler.annotations index in
    match annotation with
    | None -> Format.sprintf "Undecodable(%d)" index
    | Some annotation -> annotation
  in
  let decode_target { ClassHierarchy.Target.target; parameters } =
    Format.asprintf "%s[%a]" (decode target) Type.OrderedTypes.pp_concise parameters
  in
  let open SharedMemory in
  match decoded with
  | ClassDefinitions.Decoded (key, value) ->
      let value =
        match value with
        | Some { Node.value = definition; _ } ->
            `Assoc ["class_definition", `String (Ast.Statement.Class.show definition)]
            |> Yojson.to_string
            |> Option.some
        | None -> None
      in
      Some (ClassValue.description, key, value)
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
  | ClassKeys.Decoded (key, value) ->
      Some (ClassKeyValue.description, Reference.show key, value >>| List.to_string ~f:Fn.id)
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
  | OrderIndices.Decoded (key, value) ->
      Some (OrderIndexValue.description, key, value >>| Int.to_string)
  | OrderAnnotations.Decoded (key, value) ->
      Some (OrderAnnotationValue.description, Int.to_string key, value)
  | OrderEdges.Decoded (key, value) ->
      Some (EdgeValue.description, decode key, value >>| List.to_string ~f:decode_target)
  | OrderBackedges.Decoded (key, value) ->
      Some
        ( BackedgeValue.description,
          decode key,
          value >>| ClassHierarchy.Target.Set.Tree.to_list >>| List.to_string ~f:decode_target )
  | OrderKeys.Decoded (key, value) ->
      Some (OrderKeyValue.description, Int.to_string key, value >>| List.to_string ~f:decode)
  | Modules.Decoded (key, value) ->
      Some
        (ModuleValue.description, Reference.show key, value >>| Module.sexp_of_t >>| Sexp.to_string)
  | _ -> None


let decoded_equal first second =
  let open SharedMemory in
  match first, second with
  | ClassDefinitions.Decoded (_, first), ClassDefinitions.Decoded (_, second) ->
      Some (Option.equal (Node.equal Statement.Class.equal) first second)
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
  | ClassKeys.Decoded (_, first), ClassKeys.Decoded (_, second) ->
      Some (Option.equal (List.equal Identifier.equal) first second)
  | GlobalKeys.Decoded (_, first), GlobalKeys.Decoded (_, second) ->
      Some (Option.equal (List.equal Reference.equal) first second)
  | AliasKeys.Decoded (_, first), AliasKeys.Decoded (_, second) ->
      Some (Option.equal (List.equal Identifier.equal) first second)
  | DependentKeys.Decoded (_, first), DependentKeys.Decoded (_, second) ->
      Some (Option.equal (List.equal Reference.equal) first second)
  | OrderIndices.Decoded (_, first), OrderIndices.Decoded (_, second) ->
      Some (Option.equal Int.equal first second)
  | OrderAnnotations.Decoded (_, first), OrderAnnotations.Decoded (_, second) ->
      Some (Option.equal String.equal first second)
  | OrderEdges.Decoded (_, first), OrderEdges.Decoded (_, second) ->
      Some (Option.equal (List.equal ClassHierarchy.Target.equal) first second)
  | OrderBackedges.Decoded (_, first), OrderBackedges.Decoded (_, second) ->
      Some (Option.equal ClassHierarchy.Target.Set.Tree.equal first second)
  | OrderKeys.Decoded (_, first), OrderKeys.Decoded (_, second) ->
      Some (Option.equal (List.equal Int.equal) first second)
  | Modules.Decoded (_, first), Modules.Decoded (_, second) ->
      Some (Option.equal Module.equal first second)
  | _ -> None


let class_hierarchy_dot () =
  ClassHierarchy.to_dot
    (module SharedMemoryClassHierarchyHandler)
    ~indices:(SharedMemoryClassHierarchyHandler.keys ())
