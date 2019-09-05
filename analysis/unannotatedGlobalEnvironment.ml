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

module ReadOnly = struct
  type t = {
    ast_environment: AstEnvironment.ReadOnly.t;
    get_class_definition: ?dependency:Reference.t -> string -> Class.t Node.t option;
    class_exists: ?dependency:Reference.t -> string -> bool;
    all_classes: unit -> Type.Primitive.t list;
  }

  let ast_environment { ast_environment; _ } = ast_environment

  let get_class_definition { get_class_definition; _ } = get_class_definition

  let class_exists { class_exists; _ } = class_exists

  let all_classes { all_classes; _ } = all_classes ()
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

  module ClassKeys = Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (ClassKeyValue)

  let get_keys keys =
    ClassKeys.KeySet.of_list keys
    |> ClassKeys.get_batch
    |> ClassKeys.KeyMap.values
    |> List.filter_map ~f:Fn.id
    |> List.concat
end

(* We want to ensure that we are only writing to this table in this phase, not creating internal
   dependencies with self-reads. Accordingly read_only should only be called by downstream clients *)
module WriteOnly : sig
  val set_class_definition : name:string -> definition:Class.t Node.t -> unit

  val add_to_transaction
    :  SharedMemoryKeys.ReferenceDependencyKey.Transaction.t ->
    keys:string list ->
    SharedMemoryKeys.ReferenceDependencyKey.Transaction.t

  val get_all_dependents : string list -> SharedMemoryKeys.ReferenceDependencyKey.KeySet.t

  val direct_data_purge : Type.Primitive.t list -> unit

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
    Memory.DependencyTrackedTableWithCache
      (SharedMemoryKeys.StringKey)
      (SharedMemoryKeys.ReferenceDependencyKey)
      (ClassValue)

  let set_class_definition ~name ~definition = ClassDefinitions.add name definition

  let add_to_transaction txn ~keys =
    let keys = ClassDefinitions.KeySet.of_list keys in
    ClassDefinitions.add_to_transaction ~keys txn


  let get_all_dependents x =
    ClassDefinitions.KeySet.of_list x |> ClassDefinitions.get_all_dependents


  let direct_data_purge keys =
    ClassDefinitions.KeySet.of_list keys |> ClassDefinitions.remove_batch


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
    [ make "typing.Optional";
      make "typing.Undeclared";
      make "typing.NoReturn";
      make "typing.Annotated";
      make "typing.Protocol";
      make "typing.Callable";
      make "typing.FrozenSet";
      make "typing.ClassVar";
      make "typing.Final";
      make "typing.Union";
      make ~metaclasses:[Primitive "typing.GenericMeta"] "typing.Generic" ]
  in
  let typing_extension_classes =
    [make "typing_extensions.Final"; make "typing_extensions.Literal"]
  in
  let builtin_classes =
    let t_self_expression = Name (Name.Identifier "TSelf") |> Node.create_with_default_location in
    [ make
        ~bases:[Type.parametric "typing.Mapping" (Concrete [Type.string; Type.Any])]
        ~body:(Type.TypedDictionary.defines ~t_self_expression ~total:true)
        "TypedDictionary";
      make
        ~bases:[Type.Primitive "TypedDictionary"]
        ~body:(Type.TypedDictionary.defines ~t_self_expression ~total:false)
        "NonTotalTypedDictionary" ]
  in
  builtin_classes, typing_classes, typing_extension_classes


let register_class_definitions ({ Source.qualifier; _ } as source) =
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
            [ Statement.Define
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
              |> Node.create_with_default_location ]
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


module UpdateResult = struct
  type t = {
    current_classes: Type.Primitive.Set.t;
    previous_classes: Type.Primitive.Set.t;
    triggered_dependencies: SharedMemoryKeys.ReferenceDependencyKey.KeySet.t;
  }

  let current_classes { current_classes; _ } = current_classes

  let current_classes_and_removed_classes { current_classes; previous_classes; _ } =
    Type.Primitive.Set.union current_classes previous_classes


  let triggered_dependencies { triggered_dependencies; _ } = triggered_dependencies
end

let update { ast_environment; _ } ~scheduler ~configuration modified_qualifiers =
  let map sources =
    let register qualifier =
      AstEnvironment.ReadOnly.get_source ast_environment qualifier
      >>| register_class_definitions
      |> Option.value ~default:()
    in
    List.iter sources ~f:register
  in
  let modified_qualifiers = Set.to_list modified_qualifiers in
  let update () = Scheduler.iter scheduler ~configuration ~f:map ~inputs:modified_qualifiers in
  let previous_classes_list = KeyTracker.get_keys modified_qualifiers in
  let previous_classes = Type.Primitive.Set.of_list previous_classes_list in
  KeyTracker.ClassKeys.KeySet.of_list modified_qualifiers |> KeyTracker.ClassKeys.remove_batch;
  match configuration with
  | { incremental_style = FineGrained; _ } ->
      let (), mutation_triggers =
        SharedMemoryKeys.ReferenceDependencyKey.Transaction.empty
        |> WriteOnly.add_to_transaction ~keys:previous_classes_list
        |> SharedMemoryKeys.ReferenceDependencyKey.Transaction.execute ~update
      in
      let current_classes =
        KeyTracker.get_keys modified_qualifiers |> Type.Primitive.Set.of_list
      in
      let additions = Type.Primitive.Set.diff current_classes previous_classes in
      let addition_triggers =
        WriteOnly.get_all_dependents (Type.Primitive.Set.to_list additions)
      in
      let triggered_dependencies =
        SharedMemoryKeys.ReferenceDependencyKey.KeySet.union addition_triggers mutation_triggers
      in
      { UpdateResult.current_classes; previous_classes; triggered_dependencies }
  | _ ->
      WriteOnly.direct_data_purge previous_classes_list;
      update ();
      let current_classes =
        KeyTracker.get_keys modified_qualifiers |> Type.Primitive.Set.of_list
      in
      let triggered_dependencies = SharedMemoryKeys.ReferenceDependencyKey.KeySet.empty in
      { current_classes; previous_classes; triggered_dependencies }


let read_only { ast_environment; _ } = WriteOnly.read_only ~ast_environment
