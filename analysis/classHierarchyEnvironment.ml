(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre
open Expression
open Statement
module PreviousEnvironment = AliasEnvironment

type t = { alias_environment: AliasEnvironment.ReadOnly.t }

let create alias_environment = { alias_environment }

let unannotated_global_environment { alias_environment } =
  AliasEnvironment.ReadOnly.unannotated_global_environment alias_environment


module ReadOnly = struct
  type t = {
    get_edges:
      ?dependency:SharedMemoryKeys.dependency ->
      IndexTracker.t ->
      ClassHierarchy.Target.t list option;
    get_undecorated_function:
      ?dependency:SharedMemoryKeys.dependency ->
      Reference.t ->
      Type.t Type.Callable.overload option;
    alias_environment: AliasEnvironment.ReadOnly.t;
  }

  let get_edges { get_edges; _ } = get_edges

  let get_undecorated_function { get_undecorated_function; _ } = get_undecorated_function

  let alias_environment { alias_environment; _ } = alias_environment
end

module HierarchyReadOnly = ReadOnly
module UpdateResult = Environment.UpdateResult.Make (PreviousEnvironment)

module EdgeValue = struct
  type t = ClassHierarchy.Target.t list [@@deriving compare]

  let prefix = Prefix.make ()

  let description = "Edges"

  let unmarshall value = Marshal.from_string value 0
end

module UndecoratedFunctionValue = struct
  type t = Type.t Type.Callable.overload [@@deriving compare]

  let prefix = Prefix.make ()

  let description = "Undecorated functions"

  let unmarshall value = Marshal.from_string value 0
end

let get_parents ({ alias_environment } as environment) name ~track_dependencies =
  let object_index = IndexTracker.index "object" in
  let parse_annotation =
    let dependency = Option.some_if track_dependencies (SharedMemoryKeys.ClassConnect name) in
    AliasEnvironment.ReadOnly.parse_annotation_without_validating_type_parameters
      ?dependency
      alias_environment
  in
  let dependency = Option.some_if track_dependencies (SharedMemoryKeys.ClassConnect name) in
  (* Register normal annotations. *)
  let extract_supertype { Expression.Call.Argument.value; _ } =
    let value = Expression.delocalize value in
    match Node.value value with
    | Call _
    | Name _ -> (
        let supertype, parameters = parse_annotation ~allow_untracked:true value |> Type.split in
        match supertype with
        | Type.Top ->
            Statistics.event
              ~name:"superclass of top"
              ~section:`Environment
              ~normals:["unresolved name", Expression.show value]
              ();
            None
        | Type.Primitive primitive
          when not
                 (UnannotatedGlobalEnvironment.ReadOnly.class_exists
                    ?dependency
                    (unannotated_global_environment environment)
                    primitive) ->
            Log.log ~section:`Environment "Superclass annotation %a is missing" Type.pp supertype;
            None
        | Type.Primitive supertype -> Some (supertype, parameters)
        | _ -> None )
    | _ -> None
  in
  let bases ({ Node.value = { Class.bases; _ }; _ } as definition) =
    let inferred_base = AnnotatedBases.inferred_generic_base definition ~parse_annotation in
    inferred_base @ bases
  in
  let add_special_parents parents =
    let simples = List.map ~f:(fun parent -> parent, Type.OrderedTypes.Concrete []) in
    match parents, name with
    | _, "int" -> simples ["float"; "numbers.Integral"]
    | _, "float" -> simples ["complex"; "numbers.Rational"; "numbers.Real"]
    | _, "complex" -> simples ["numbers.Complex"]
    | _, "numbers.Complex" -> simples ["numbers.Number"]
    | [], _ -> simples ["object"]
    | _ -> parents
  in
  let is_not_primitive_cycle (parent, _) = not (String.equal name parent) in
  let convert_to_targets =
    List.map ~f:(fun (name, parameters) ->
        { ClassHierarchy.Target.target = IndexTracker.index name; parameters })
  in
  let deduplicate targets =
    let deduplicate (visited, sofar) ({ ClassHierarchy.Target.target; _ } as edge) =
      if Set.mem visited target then
        visited, sofar
      else
        Set.add visited target, edge :: sofar
    in
    List.fold targets ~f:deduplicate ~init:(IndexTracker.Set.empty, []) |> snd |> List.rev
  in
  let remove_extra_edges_to_object targets =
    let not_object_edge { ClassHierarchy.Target.target; _ } =
      not (IndexTracker.equal target object_index)
    in
    match List.filter targets ~f:not_object_edge with
    | [] -> targets
    | filtered -> filtered
  in
  UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
    ?dependency
    (unannotated_global_environment environment)
    name
  >>| bases
  (* Don't register metaclass=abc.ABCMeta, etc. superclasses. *)
  >>| List.filter ~f:(fun { Expression.Call.Argument.name; _ } -> Option.is_none name)
  >>| List.filter_map ~f:extract_supertype
  >>| add_special_parents
  >>| List.filter ~f:is_not_primitive_cycle
  >>| convert_to_targets
  >>| deduplicate
  >>| remove_extra_edges_to_object


let produce_undecorated_function ({ alias_environment } as environment) name ~track_dependencies =
  let global =
    let dependency =
      Option.some_if track_dependencies (SharedMemoryKeys.UndecoratedFunction name)
    in
    UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
      ?dependency
      (unannotated_global_environment environment)
      name
  in
  let handle = function
    | UnannotatedGlobalEnvironment.Define defines ->
        let handle define =
          let dependency =
            Option.some_if track_dependencies (SharedMemoryKeys.UndecoratedFunction name)
          in
          let parse_annotation =
            AliasEnvironment.ReadOnly.parse_annotation_without_validating_type_parameters
              ?dependency
              alias_environment
          in
          let parse_as_concatenation =
            AliasEnvironment.ReadOnly.parse_as_concatenation ?dependency alias_environment
          in
          let parse_as_parameter_specification_instance_annotation =
            AliasEnvironment.ReadOnly.parse_as_parameter_specification_instance_annotation
              ?dependency
              alias_environment
          in
          let parser =
            {
              AnnotatedCallable.parse_annotation;
              parse_as_concatenation;
              parse_as_parameter_specification_instance_annotation;
            }
          in
          AnnotatedCallable.create_overload ~parser define
        in
        List.find defines ~f:(fun define -> not (Define.is_overloaded_method (Node.value define)))
        >>| handle
    | _ -> None
  in
  global >>= handle


module Edges = Environment.EnvironmentTable.WithCache (struct
  module PreviousEnvironment = PreviousEnvironment
  module UpdateResult = UpdateResult
  module Key = IndexTracker.IndexKey
  module Value = EdgeValue

  type trigger = string

  let convert_trigger = IndexTracker.index

  type nonrec t = t

  module TriggerSet = Type.Primitive.Set

  let produce_value = get_parents

  let filter_upstream_dependency = function
    | SharedMemoryKeys.ClassConnect name -> Some name
    | _ -> None


  let added_keys upstream_update =
    AliasEnvironment.UpdateResult.upstream upstream_update
    |> UnannotatedGlobalEnvironment.UpdateResult.added_classes


  let current_and_previous_keys upstream_update =
    AliasEnvironment.UpdateResult.upstream upstream_update
    |> UnannotatedGlobalEnvironment.UpdateResult.current_classes_and_removed_classes


  let all_keys environment =
    unannotated_global_environment environment
    |> UnannotatedGlobalEnvironment.ReadOnly.all_classes
    |> List.map ~f:IndexTracker.index


  let decode_target { ClassHierarchy.Target.target; parameters } =
    Format.asprintf
      "%s[%a]"
      (IndexTracker.annotation target)
      Type.OrderedTypes.pp_concise
      parameters


  let serialize_value = List.to_string ~f:decode_target

  let show_key = IndexTracker.annotation

  let equal_value = List.equal ClassHierarchy.Target.equal
end)

module UndecoratedFunctions = Environment.EnvironmentTable.WithCache (struct
  module PreviousEnvironment = PreviousEnvironment
  module UpdateResult = UpdateResult
  module Key = SharedMemoryKeys.ReferenceKey
  module Value = UndecoratedFunctionValue

  type nonrec t = t

  type trigger = Reference.t

  let convert_trigger = Fn.id

  module TriggerSet = Reference.Set

  let produce_value = produce_undecorated_function

  let filter_upstream_dependency = function
    | SharedMemoryKeys.UndecoratedFunction name -> Some name
    | _ -> None


  let added_keys upstream_update =
    AliasEnvironment.UpdateResult.upstream upstream_update
    |> UnannotatedGlobalEnvironment.UpdateResult.added_unannotated_globals


  let current_and_previous_keys upstream_update =
    AliasEnvironment.UpdateResult.upstream upstream_update
    |> UnannotatedGlobalEnvironment.UpdateResult.current_and_previous_unannotated_globals


  let all_keys environment =
    unannotated_global_environment environment
    |> UnannotatedGlobalEnvironment.ReadOnly.all_unannotated_globals


  let serialize_value = Type.Callable.show_overload Type.pp

  let show_key = Reference.show

  let equal_value = Type.Callable.equal_overload Type.equal
end)

let update environment ~scheduler ~configuration upstream_update =
  let edge_result =
    Profiling.track_duration_and_shared_memory "class forward edge" ~f:(fun _ ->
        Edges.update environment ~scheduler ~configuration upstream_update)
  in
  let undecorated_functions_result =
    Profiling.track_duration_and_shared_memory "undecorated functions" ~f:(fun _ ->
        UndecoratedFunctions.update environment ~scheduler ~configuration upstream_update)
  in
  let triggered_dependencies =
    SharedMemoryKeys.DependencyKey.KeySet.union
      (UpdateResult.locally_triggered_dependencies edge_result)
      (UpdateResult.locally_triggered_dependencies undecorated_functions_result)
  in
  UpdateResult.create ~triggered_dependencies ~upstream:upstream_update


let read_only { alias_environment } =
  {
    ReadOnly.alias_environment;
    get_edges = Edges.get;
    get_undecorated_function = UndecoratedFunctions.get;
  }
