(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Ast
open Statement
open Expression

type t = { ast_environment: AstEnvironment.ReadOnly.t }

let create ast_environment = { ast_environment }

type unannotated_global =
  | SimpleAssign of {
      explicit_annotation: Expression.t option;
      value: Expression.t;
    }
  | Imported of Reference.t
[@@deriving compare, show]

type dependency =
  | AliasRegister of Reference.t
  | TypeCheckSource of Reference.t
[@@deriving show, compare, sexp]

module DependencyKey = Memory.DependencyKey.Make (struct
  type nonrec t = dependency

  let to_string dependency = sexp_of_dependency dependency |> Sexp.to_string_mach

  let compare = compare_dependency

  type out = dependency

  let from_string string = Sexp.of_string string |> dependency_of_sexp
end)

module ReadOnly = struct
  type t = {
    ast_environment: AstEnvironment.ReadOnly.t;
    get_class_definition: ?dependency:dependency -> string -> Class.t Node.t option;
    class_exists: ?dependency:dependency -> string -> bool;
    all_classes: unit -> Type.Primitive.t list;
    get_unannotated_global: ?dependency:dependency -> Reference.t -> unannotated_global option;
  }

  let ast_environment { ast_environment; _ } = ast_environment

  let get_class_definition { get_class_definition; _ } = get_class_definition

  let class_exists { class_exists; _ } = class_exists

  let all_classes { all_classes; _ } = all_classes ()

  let get_unannotated_global { get_unannotated_global; _ } = get_unannotated_global
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
  val set_class_definition : name:string -> definition:Class.t Node.t -> unit

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
    type t = Class.t Node.t

    let prefix = Prefix.make ()

    let description = "Class"

    let unmarshall value = Marshal.from_string value 0

    let compare = Node.compare Class.compare
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
    let class_exists ?dependency name =
      Option.iter dependency ~f:(ClassDefinitions.add_dependency name);
      ClassDefinitions.mem name
    in
    {
      ast_environment;
      ReadOnly.get_class_definition = ClassDefinitions.get;
      all_classes;
      class_exists;
      get_unannotated_global = UnannotatedGlobals.get;
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
  let typing_classes =
    [
      make "typing.Optional";
      make "typing.Undeclared";
      make "typing.NoReturn";
      make "typing.Annotated";
      make "typing.Protocol";
      make "typing.Callable";
      make "typing.FrozenSet";
      make "typing.ClassVar";
      make "typing.Final";
      make "typing.Union";
      make ~metaclasses:[Primitive "typing.GenericMeta"] "typing.Generic";
    ]
  in
  let typing_extension_classes =
    [
      make "typing_extensions.Final";
      make "typing_extensions.Literal";
      make "typing_extensions.Annotated";
    ]
  in
  let builtin_classes =
    let t_self_expression = Name (Name.Identifier "TSelf") |> Node.create_with_default_location in
    [
      make
        ~bases:[Type.parametric "typing.Mapping" (Concrete [Type.string; Type.Any])]
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
  let classes = source |> ClassCollector.visit [] |> List.rev in
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
      ~definition:{ Node.location; value = definition };
    Set.add new_annotations primitive
  in
  List.fold classes ~init:Type.Primitive.Set.empty ~f:register
  |> Set.to_list
  |> KeyTracker.ClassKeys.add qualifier


let collect_unannotated_globals { Source.statements; source_path = { SourcePath.qualifier; _ }; _ }
  =
  let rec visit_statement ~qualifier globals { Node.value; _ } =
    match value with
    | Statement.Assign { Assign.target = { Node.value = Name target; _ }; annotation; value; _ }
      when Expression.is_simple_name target ->
        let qualified_name =
          let target = Expression.name_to_reference_exn target |> Reference.sanitize_qualified in
          Reference.combine qualifier target
        in
        (qualified_name, SimpleAssign { explicit_annotation = annotation; value }) :: globals
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
    | _ -> globals
  in
  let write (target, o) =
    WriteOnly.set_unannotated_global ~target o;
    target
  in
  List.fold ~init:[] ~f:(visit_statement ~qualifier) statements
  (* Preserve existing first alias wins semantics *)
  |> List.rev
  |> List.map ~f:write
  |> KeyTracker.UnannotatedGlobalKeys.add qualifier


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


  let current_classes { current_classes; _ } = current_classes

  let current_unannotated_globals { current_unannotated_globals; _ } = current_unannotated_globals

  let current_classes_and_removed_classes { current_classes; previous_classes; _ } =
    Type.Primitive.Set.union current_classes previous_classes


  let current_and_previous_unannotated_globals
      { current_unannotated_globals; previous_unannotated_globals; _ }
    =
    Reference.Set.union current_unannotated_globals previous_unannotated_globals


  let triggered_dependencies { triggered_dependencies; _ } = triggered_dependencies
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
      let (), mutation_triggers =
        DependencyKey.Transaction.empty
        |> WriteOnly.add_to_transaction ~previous_classes_list ~previous_unannotated_globals_list
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
      let triggered_dependencies =
        DependencyKey.KeySet.union addition_triggers mutation_triggers
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
      WriteOnly.direct_data_purge ~previous_classes_list ~previous_unannotated_globals_list;
      update ();
      let current_classes =
        KeyTracker.get_keys modified_qualifiers |> Type.Primitive.Set.of_list
      in
      let current_unannotated_globals =
        KeyTracker.get_unannotated_global_keys modified_qualifiers |> Reference.Set.of_list
      in
      let triggered_dependencies = DependencyKey.KeySet.empty in
      {
        current_classes;
        previous_classes;
        current_unannotated_globals;
        previous_unannotated_globals;
        triggered_dependencies;
        upstream;
      }


let read_only { ast_environment; _ } = WriteOnly.read_only ~ast_environment
