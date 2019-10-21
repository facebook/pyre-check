(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Ast
open Statement
open Expression
open SharedMemoryKeys

type t = { ast_environment: AstEnvironment.ReadOnly.t }

let create ast_environment = { ast_environment }

type unannotated_global =
  | SimpleAssign of {
      explicit_annotation: Expression.t option;
      value: Expression.t;
      target_location: Location.t;
    }
  | TupleAssign of {
      value: Expression.t;
      target_location: Location.t;
      index: int;
      total_length: int;
    }
  | Imported of Reference.t
  | Define of Define.Signature.t Node.t list
[@@deriving compare, show, equal]

module ReadOnly = struct
  type t = {
    ast_environment: AstEnvironment.ReadOnly.t;
    get_class_definition: ?dependency:dependency -> string -> ClassSummary.t Node.t option;
    class_exists: ?dependency:dependency -> string -> bool;
    all_classes: unit -> Type.Primitive.t list;
    all_indices: unit -> IndexTracker.t list;
    all_unannotated_globals: unit -> Reference.t list;
    get_unannotated_global: ?dependency:dependency -> Reference.t -> unannotated_global option;
    hash_to_key_map: unit -> string String.Map.t;
    serialize_decoded: Memory.decodable -> (string * string * string option) option;
    decoded_equal: Memory.decodable -> Memory.decodable -> bool option;
  }

  let ast_environment { ast_environment; _ } = ast_environment

  let get_class_definition { get_class_definition; _ } = get_class_definition

  let class_exists { class_exists; _ } = class_exists

  let all_classes { all_classes; _ } = all_classes ()

  let all_indices { all_indices; _ } = all_indices ()

  let all_unannotated_globals { all_unannotated_globals; _ } = all_unannotated_globals ()

  let get_unannotated_global { get_unannotated_global; _ } = get_unannotated_global

  let hash_to_key_map { hash_to_key_map; _ } = hash_to_key_map ()

  let serialize_decoded { serialize_decoded; _ } = serialize_decoded

  let decoded_equal { decoded_equal; _ } = decoded_equal
end

(* The key tracking is necessary because there is no empirical way to determine which classes exist
   for a given class. This "fan-out" necessitates internal tracking. However, this module need not
   be sealed to ensure write only-ness since we're not dependency tracking this, since it's only
   used internally for doing our dependency analysis, and for all_classes, which is only used for
   all-or-nothing operations like validating the class hierarchy and printing it for debugging
   purposes *)
module KeyTracker = struct
  module ClassKeyValue = struct
    type t = Identifier.t list [@@deriving compare]

    let prefix = Prefix.make ()

    let description = "Class keys"

    let unmarshall value = Marshal.from_string value 0
  end

  module UnannotatedGlobalKeyValue = struct
    type t = Reference.t list [@@deriving compare]

    let prefix = Prefix.make ()

    let description = "Class keys"

    let unmarshall value = Marshal.from_string value 0
  end

  module ClassKeys = Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (ClassKeyValue)
  module UnannotatedGlobalKeys =
    Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (UnannotatedGlobalKeyValue)

  let get_keys keys =
    ClassKeys.KeySet.of_list keys
    |> ClassKeys.get_batch
    |> ClassKeys.KeyMap.values
    |> List.filter_map ~f:Fn.id
    |> List.concat


  let get_unannotated_global_keys keys =
    UnannotatedGlobalKeys.KeySet.of_list keys
    |> UnannotatedGlobalKeys.get_batch
    |> UnannotatedGlobalKeys.KeyMap.values
    |> List.filter_map ~f:Fn.id
    |> List.concat
end

(* We want to ensure that we are only writing to this table in this phase, not creating internal
   dependencies with self-reads. Accordingly read_only should only be called by downstream clients *)
module WriteOnly : sig
  val set_class_definition : name:string -> definition:ClassSummary.t Node.t -> unit

  val add_to_transaction
    :  DependencyKey.Transaction.t ->
    previous_classes_list:string list ->
    previous_unannotated_globals_list:Reference.t list ->
    DependencyKey.Transaction.t

  val get_all_dependents
    :  class_additions:string list ->
    unannotated_global_additions:Reference.t list ->
    DependencyKey.KeySet.t

  val direct_data_purge
    :  previous_classes_list:Type.Primitive.t list ->
    previous_unannotated_globals_list:Reference.t list ->
    unit

  val set_unannotated_global : target:Reference.t -> unannotated_global -> unit

  val read_only : ast_environment:AstEnvironment.ReadOnly.t -> ReadOnly.t
end = struct
  module ClassValue = struct
    type t = ClassSummary.t Node.t

    let prefix = Prefix.make ()

    let description = "Class"

    let unmarshall value = Marshal.from_string value 0

    let compare = Node.compare ClassSummary.compare
  end

  module ClassDefinitions =
    Memory.DependencyTrackedTableWithCache (SharedMemoryKeys.StringKey) (DependencyKey)
      (ClassValue)

  module UnannotatedGlobalValue = struct
    type t = unannotated_global

    let prefix = Prefix.make ()

    let description = "UnannotatedGlobal"

    let unmarshall value = Marshal.from_string value 0

    let compare = compare_unannotated_global
  end

  module UnannotatedGlobals =
    Memory.DependencyTrackedTableWithCache (SharedMemoryKeys.ReferenceKey) (DependencyKey)
      (UnannotatedGlobalValue)

  let set_unannotated_global ~target = UnannotatedGlobals.write_through target

  let set_class_definition ~name ~definition = ClassDefinitions.write_through name definition

  let add_to_transaction txn ~previous_classes_list ~previous_unannotated_globals_list =
    let class_keys = ClassDefinitions.KeySet.of_list previous_classes_list in
    let unannotated_globals_keys =
      UnannotatedGlobals.KeySet.of_list previous_unannotated_globals_list
    in
    ClassDefinitions.add_to_transaction ~keys:class_keys txn
    |> UnannotatedGlobals.add_to_transaction ~keys:unannotated_globals_keys


  let get_all_dependents ~class_additions ~unannotated_global_additions =
    DependencyKey.KeySet.union
      (ClassDefinitions.KeySet.of_list class_additions |> ClassDefinitions.get_all_dependents)
      ( UnannotatedGlobals.KeySet.of_list unannotated_global_additions
      |> UnannotatedGlobals.get_all_dependents )


  let direct_data_purge ~previous_classes_list ~previous_unannotated_globals_list =
    ClassDefinitions.KeySet.of_list previous_classes_list |> ClassDefinitions.remove_batch;
    UnannotatedGlobals.KeySet.of_list previous_unannotated_globals_list
    |> UnannotatedGlobals.remove_batch


  let read_only ~ast_environment =
    let all_classes () =
      AstEnvironment.ReadOnly.all_explicit_modules ast_environment |> KeyTracker.get_keys
    in
    let all_indices () =
      all_classes ()
      |> Type.Primitive.Set.of_list
      |> IndexTracker.indices
      |> IndexTracker.Set.to_list
    in
    let all_unannotated_globals () =
      AstEnvironment.ReadOnly.all_explicit_modules ast_environment
      |> KeyTracker.get_unannotated_global_keys
    in
    let class_exists ?dependency name =
      Option.iter dependency ~f:(ClassDefinitions.add_dependency name);
      ClassDefinitions.mem name
    in
    let hash_to_key_map () =
      Map.merge_skewed
        (ClassDefinitions.compute_hashes_to_keys ~keys:(all_classes ()))
        (UnannotatedGlobals.compute_hashes_to_keys ~keys:(all_unannotated_globals ()))
        ~combine:(fun ~key:_ value _ -> value)
    in
    let serialize_decoded = function
      | ClassDefinitions.Decoded (key, value) ->
          let value = value >>| Node.value >>| ClassSummary.show in
          Some (ClassValue.description, key, value)
      | UnannotatedGlobals.Decoded (key, value) ->
          let value = value >>| show_unannotated_global in
          Some (ClassValue.description, Reference.show key, value)
      | _ -> None
    in
    let decoded_equal first second =
      match first, second with
      | ClassDefinitions.Decoded (_, first), ClassDefinitions.Decoded (_, second) ->
          Some (Option.equal (Node.equal ClassSummary.equal) first second)
      | UnannotatedGlobals.Decoded (_, first), UnannotatedGlobals.Decoded (_, second) ->
          Some (Option.equal equal_unannotated_global first second)
      | _ -> None
    in
    {
      ast_environment;
      ReadOnly.get_class_definition = ClassDefinitions.get;
      all_classes;
      all_indices;
      class_exists;
      get_unannotated_global = UnannotatedGlobals.get;
      all_unannotated_globals;
      hash_to_key_map;
      serialize_decoded;
      decoded_equal;
    }
end

let missing_builtin_classes, missing_typing_classes, missing_typing_extensions_classes =
  let make ?(bases = []) ?(metaclasses = []) ?(body = []) name =
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
    |> Node.create_with_default_location
  in
  let catch_all_generic = [Type.parametric "typing.Generic" Any] in
  let typing_classes =
    [
      make "typing.Optional" ~bases:catch_all_generic;
      make "typing.Undeclared";
      make "typing.NoReturn";
      make "typing.Annotated" ~bases:catch_all_generic;
      make "typing.Protocol" ~bases:catch_all_generic;
      make "typing.Callable" ~bases:catch_all_generic;
      make "typing.FrozenSet" ~bases:catch_all_generic;
      make "typing.ClassVar" ~bases:catch_all_generic;
      make "typing.Final" ~bases:catch_all_generic;
      make "typing.Union" ~bases:catch_all_generic;
      make ~metaclasses:[Primitive "typing.GenericMeta"] "typing.Generic";
    ]
  in
  let typing_extension_classes =
    [
      make "typing_extensions.Final";
      make "typing_extensions.Literal" ~bases:catch_all_generic;
      make "typing_extensions.Annotated" ~bases:catch_all_generic;
    ]
  in
  let builtin_classes =
    let t_self_expression = Name (Name.Identifier "TSelf") |> Node.create_with_default_location in
    [
      make
        ~bases:[Type.parametric "typing.Mapping" (Concrete [Type.string; Type.object_primitive])]
        ~body:(Type.TypedDictionary.defines ~t_self_expression ~total:true)
        "TypedDictionary";
      make
        ~bases:[Type.Primitive "TypedDictionary"]
        ~body:(Type.TypedDictionary.defines ~t_self_expression ~total:false)
        "NonTotalTypedDictionary";
    ]
  in
  builtin_classes, typing_classes, typing_extension_classes


let register_class_definitions ({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source) =
  let module ClassCollector = Visit.MakeStatementVisitor (struct
    type t = Class.t Node.t list

    let visit_children _ = true

    let statement _ sofar = function
      | { Node.location; value = Statement.Class definition } ->
          { Node.location; value = definition } :: sofar
      | _ -> sofar
  end)
  in
  let classes = ClassCollector.visit [] source in
  let classes =
    match Reference.as_list qualifier with
    | [] -> classes @ missing_builtin_classes
    | ["typing"] -> classes @ missing_typing_classes
    | ["typing_extensions"] -> classes @ missing_typing_extensions_classes
    | _ -> classes
  in
  let register new_annotations { Node.location; value = { Class.name; _ } as definition } =
    let primitive = Reference.show name in
    let definition =
      match primitive with
      | "type" ->
          let value =
            Type.expression
              (Type.parametric "typing.Generic" (Concrete [Type.variable "typing._T"]))
          in
          { definition with Class.bases = [{ name = None; value }] }
      | "typing.GenericMeta" ->
          let body =
            [
              Statement.Define
                {
                  signature =
                    {
                      name = Reference.create "typing.GenericMeta.__getitem__";
                      parameters =
                        [
                          { Parameter.name = "cls"; value = None; annotation = None }
                          |> Node.create_with_default_location;
                          { Parameter.name = "arg"; value = None; annotation = None }
                          |> Node.create_with_default_location;
                        ];
                      decorators = [];
                      docstring = None;
                      return_annotation = None;
                      async = false;
                      generator = false;
                      parent = Some (Reference.create "typing.GenericMeta");
                    };
                  body = [];
                }
              |> Node.create_with_default_location;
            ]
          in
          { definition with body }
      | _ -> definition
    in
    WriteOnly.set_class_definition
      ~name:primitive
      ~definition:{ Node.location; value = ClassSummary.create definition };
    Set.add new_annotations primitive
  in
  List.fold classes ~init:Type.Primitive.Set.empty ~f:register
  |> Set.to_list
  |> KeyTracker.ClassKeys.add qualifier


let missing_builtin_globals =
  let assign name annotation =
    ( Reference.create name,
      SimpleAssign
        {
          explicit_annotation = Some (Type.expression annotation);
          target_location = Location.Reference.any;
          value = Node.create_with_default_location Expression.Ellipsis;
        } )
  in
  [assign "None" Type.none; assign "..." Type.Any; assign "__debug__" Type.bool]


let collect_unannotated_globals { Source.statements; source_path = { SourcePath.qualifier; _ }; _ }
  =
  let rec visit_statement ~qualifier globals { Node.value; location } =
    let qualified_name target =
      let target = Expression.name_to_reference_exn target |> Reference.sanitize_qualified in
      Option.some_if (Reference.length target = 1) (Reference.combine qualifier target)
    in
    match value with
    | Statement.Assign
        {
          Assign.target = { Node.value = Name target; location = target_location };
          annotation;
          value;
          _;
        }
      when Expression.is_simple_name target ->
        qualified_name target
        >>| (fun qualified ->
              (qualified, SimpleAssign { explicit_annotation = annotation; value; target_location })
              :: globals)
        |> Option.value ~default:globals
    | Statement.Assign { Assign.target = { Node.value = Tuple elements; _ }; value; _ } ->
        let valid =
          let total_length = List.length elements in
          let is_simple_name index = function
            | { Node.value = Name name; location = target_location }
              when Expression.is_simple_name name ->
                qualified_name name
                >>| fun name -> name, TupleAssign { value; target_location; index; total_length }
            | _ -> None
          in
          List.mapi elements ~f:is_simple_name
        in
        (Option.all valid |> Option.value ~default:[]) @ globals
    | Import { Import.from = Some _; imports = [{ Import.name; _ }] }
      when String.equal (Reference.show name) "*" ->
        (* Don't register x.* as a global when a user writes `from x import *`. *)
        globals
    | Import { Import.from; imports } ->
        let from =
          match from >>| Reference.show with
          | None
          | Some "future.builtins"
          | Some "builtins" ->
              Reference.empty
          | Some from -> Reference.create from
        in
        let import_to_global { Import.name; alias } =
          let qualified_name =
            match alias with
            | None -> Reference.combine qualifier name
            | Some alias -> Reference.combine qualifier alias
          in
          let original_name = Reference.combine from name in
          qualified_name, Imported original_name
        in
        List.rev_append (List.map ~f:import_to_global imports) globals
    | Define { Define.signature = { Define.Signature.name; _ } as signature; _ } ->
        (name, Define [Node.create signature ~location]) :: globals
    | If { If.body; orelse; _ } ->
        (* TODO(T28732125): Properly take an intersection here. *)
        List.fold ~init:globals ~f:(visit_statement ~qualifier) (body @ orelse)
    | _ -> globals
  in
  let write (target, o) =
    WriteOnly.set_unannotated_global ~target o;
    target
  in
  let merge_defines unannotated_globals_alist =
    let not_defines, defines =
      List.partition_map unannotated_globals_alist ~f:(function
          | name, Define defines -> `Snd (name, defines)
          | x -> `Fst x)
    in
    let add_to_map sofar (name, defines) =
      let merge_with_existing to_merge = function
        | None -> Some to_merge
        | Some existing -> Some (to_merge @ existing)
      in
      Map.change sofar name ~f:(merge_with_existing defines)
    in
    List.fold defines ~f:add_to_map ~init:Reference.Map.empty
    |> Reference.Map.map ~f:(fun x -> Define x)
    |> Reference.Map.to_alist
    |> List.append not_defines
  in
  let globals = List.fold ~init:[] ~f:(visit_statement ~qualifier) statements |> merge_defines in
  let globals =
    match Reference.as_list qualifier with
    | [] -> globals @ missing_builtin_globals
    | _ -> globals
  in
  globals |> List.map ~f:write |> KeyTracker.UnannotatedGlobalKeys.add qualifier


module UpdateResult = struct
  type t = {
    current_classes: Type.Primitive.Set.t;
    previous_classes: Type.Primitive.Set.t;
    current_unannotated_globals: Reference.Set.t;
    previous_unannotated_globals: Reference.Set.t;
    triggered_dependencies: DependencyKey.KeySet.t;
    upstream: AstEnvironment.UpdateResult.t;
  }

  let added_unannotated_globals { current_unannotated_globals; previous_unannotated_globals; _ } =
    Reference.Set.diff current_unannotated_globals previous_unannotated_globals


  let added_classes { current_classes; previous_classes; _ } =
    Type.Primitive.Set.diff current_classes previous_classes


  let current_classes { current_classes; _ } = current_classes

  let current_unannotated_globals { current_unannotated_globals; _ } = current_unannotated_globals

  let current_classes_and_removed_classes { current_classes; previous_classes; _ } =
    Type.Primitive.Set.union current_classes previous_classes


  let current_and_previous_unannotated_globals
      { current_unannotated_globals; previous_unannotated_globals; _ }
    =
    Reference.Set.union current_unannotated_globals previous_unannotated_globals


  let locally_triggered_dependencies { triggered_dependencies; _ } = triggered_dependencies

  let upstream { upstream; _ } = upstream

  let all_triggered_dependencies { triggered_dependencies; upstream; _ } =
    [triggered_dependencies; AstEnvironment.UpdateResult.triggered_dependencies upstream]


  let unannotated_global_environment_update_result = Fn.id
end

let update
    { ast_environment; _ }
    ~scheduler
    ~configuration
    ~ast_environment_update_result:upstream
    modified_qualifiers
  =
  let map sources =
    let register qualifier =
      AstEnvironment.ReadOnly.get_source ast_environment qualifier
      >>| (fun source ->
            register_class_definitions source;
            collect_unannotated_globals source)
      |> Option.value ~default:()
    in
    List.iter sources ~f:register
  in
  let modified_qualifiers = Set.to_list modified_qualifiers in
  let update () = Scheduler.iter scheduler ~configuration ~f:map ~inputs:modified_qualifiers in
  let previous_classes_list = KeyTracker.get_keys modified_qualifiers in
  let previous_classes = Type.Primitive.Set.of_list previous_classes_list in
  let previous_unannotated_globals_list =
    KeyTracker.get_unannotated_global_keys modified_qualifiers
  in
  let previous_unannotated_globals = Reference.Set.of_list previous_unannotated_globals_list in
  KeyTracker.ClassKeys.KeySet.of_list modified_qualifiers |> KeyTracker.ClassKeys.remove_batch;
  KeyTracker.UnannotatedGlobalKeys.KeySet.of_list modified_qualifiers
  |> KeyTracker.UnannotatedGlobalKeys.remove_batch;
  match configuration with
  | { incremental_style = FineGrained; _ } ->
      let current_classes, current_unannotated_globals, triggered_dependencies =
        Profiling.track_duration_and_shared_memory
          "TableUpdate(Unannotated globals)"
          ~tags:["phase_name", "Global discovery"]
          ~f:(fun _ ->
            let (), mutation_triggers =
              DependencyKey.Transaction.empty
              |> WriteOnly.add_to_transaction
                   ~previous_classes_list
                   ~previous_unannotated_globals_list
              |> DependencyKey.Transaction.execute ~update
            in
            let current_classes =
              KeyTracker.get_keys modified_qualifiers |> Type.Primitive.Set.of_list
            in
            let current_unannotated_globals =
              KeyTracker.get_unannotated_global_keys modified_qualifiers |> Reference.Set.of_list
            in
            let class_additions = Type.Primitive.Set.diff current_classes previous_classes in
            let unannotated_global_additions =
              Reference.Set.diff current_unannotated_globals previous_unannotated_globals
            in
            let addition_triggers =
              WriteOnly.get_all_dependents
                ~class_additions:(Set.to_list class_additions)
                ~unannotated_global_additions:(Set.to_list unannotated_global_additions)
            in
            ( current_classes,
              current_unannotated_globals,
              DependencyKey.KeySet.union addition_triggers mutation_triggers ))
      in
      {
        UpdateResult.current_classes;
        previous_classes;
        current_unannotated_globals;
        previous_unannotated_globals;
        triggered_dependencies;
        upstream;
      }
  | _ ->
      let current_classes, current_unannotated_globals, triggered_dependencies =
        Profiling.track_duration_and_shared_memory
          "LegacyTableUpdate(Unannotated globals)"
          ~tags:["phase_name", "global discovery"]
          ~f:(fun _ ->
            WriteOnly.direct_data_purge ~previous_classes_list ~previous_unannotated_globals_list;
            update ();
            ( KeyTracker.get_keys modified_qualifiers |> Type.Primitive.Set.of_list,
              KeyTracker.get_unannotated_global_keys modified_qualifiers |> Reference.Set.of_list,
              DependencyKey.KeySet.empty ))
      in
      {
        current_classes;
        previous_classes;
        current_unannotated_globals;
        previous_unannotated_globals;
        triggered_dependencies;
        upstream;
      }


let read_only { ast_environment; _ } = WriteOnly.read_only ~ast_environment
