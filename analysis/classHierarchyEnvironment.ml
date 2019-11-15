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


module UpdateResult = Environment.UpdateResult.Make (PreviousEnvironment)

module EdgeValue = struct
  type t = ClassHierarchy.Target.t list option [@@deriving compare]

  let prefix = Prefix.make ()

  let description = "Edges"

  let unmarshall value = Marshal.from_string value 0
end

module UndecoratedFunctionValue = struct
  type t = Type.t Type.Callable.overload option [@@deriving compare]

  let prefix = Prefix.make ()

  let description = "Undecorated functions"

  let unmarshall value = Marshal.from_string value 0
end

let get_parents alias_environment name ~track_dependencies =
  let object_index = IndexTracker.index "object" in
  let parse_annotation =
    let dependency = Option.some_if track_dependencies (SharedMemoryKeys.ClassConnect name) in
    AliasEnvironment.ReadOnly.parse_annotation_without_validating_type_parameters
      ?dependency
      alias_environment
  in
  let dependency = Option.some_if track_dependencies (SharedMemoryKeys.ClassConnect name) in
  (* Register normal annotations. *)
  let extract_supertype { Call.Argument.value; _ } =
    let value = delocalize value in
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
                    (unannotated_global_environment { alias_environment })
                    primitive) ->
            Log.log ~section:`Environment "Superclass annotation %a is missing" Type.pp supertype;
            None
        | Type.Primitive supertype -> Some (supertype, parameters)
        | _ -> None )
    | _ -> None
  in
  let bases ({ Node.value = { ClassSummary.bases; _ }; _ } as definition) =
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
    (unannotated_global_environment { alias_environment })
    name
  >>| bases
  (* Don't register metaclass=abc.ABCMeta, etc. superclasses. *)
  >>| List.filter ~f:(fun { Call.Argument.name; _ } -> Option.is_none name)
  >>| List.filter_map ~f:extract_supertype
  >>| add_special_parents
  >>| List.filter ~f:is_not_primitive_cycle
  >>| convert_to_targets
  >>| deduplicate
  >>| remove_extra_edges_to_object


let produce_undecorated_function alias_environment name ~track_dependencies =
  let environment = { alias_environment } in
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
    | UnannotatedGlobalEnvironment.Define signatures ->
        let handle { Node.value = signature; location } =
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
          Node.create signature ~location |> AnnotatedCallable.create_overload ~parser
        in
        List.find signatures ~f:(fun signature ->
            not (Define.Signature.is_overloaded_function (Node.value signature)))
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

  let key_to_trigger = IndexTracker.annotation

  type nonrec t = t

  module TriggerSet = Type.Primitive.Set

  let produce_value = get_parents

  let filter_upstream_dependency = function
    | SharedMemoryKeys.ClassConnect name -> Some name
    | _ -> None


  let legacy_invalidated_keys upstream_update =
    AliasEnvironment.UpdateResult.upstream upstream_update
    |> UnannotatedGlobalEnvironment.UpdateResult.previous_classes


  let all_keys alias_environment =
    unannotated_global_environment { alias_environment }
    |> UnannotatedGlobalEnvironment.ReadOnly.all_classes
    |> List.map ~f:IndexTracker.index


  let decode_target { ClassHierarchy.Target.target; parameters } =
    Format.asprintf
      "%s[%a]"
      (IndexTracker.annotation target)
      Type.OrderedTypes.pp_concise
      parameters


  let serialize_value = function
    | Some targets -> List.to_string targets ~f:decode_target
    | None -> "None"


  let show_key = IndexTracker.annotation

  let equal_value = Option.equal (List.equal ClassHierarchy.Target.equal)
end)

module UndecoratedFunctions = Environment.EnvironmentTable.WithCache (struct
  module PreviousEnvironment = PreviousEnvironment
  module UpdateResult = UpdateResult
  module Key = SharedMemoryKeys.ReferenceKey
  module Value = UndecoratedFunctionValue

  type nonrec t = t

  type trigger = Reference.t

  let convert_trigger = Fn.id

  let key_to_trigger = Fn.id

  module TriggerSet = Reference.Set

  let produce_value = produce_undecorated_function

  let filter_upstream_dependency = function
    | SharedMemoryKeys.UndecoratedFunction name -> Some name
    | _ -> None


  let legacy_invalidated_keys upstream_update =
    AliasEnvironment.UpdateResult.upstream upstream_update
    |> UnannotatedGlobalEnvironment.UpdateResult.previous_unannotated_globals


  let all_keys alias_environment =
    unannotated_global_environment { alias_environment }
    |> UnannotatedGlobalEnvironment.ReadOnly.all_unannotated_globals


  let serialize_value = function
    | Some overload -> Type.Callable.show_overload Type.pp overload
    | None -> "None"


  let show_key = Reference.show

  let equal_value = Option.equal (Type.Callable.equal_overload Type.equal)
end)

let update { alias_environment } ~scheduler ~configuration upstream_update =
  let edge_result = Edges.update alias_environment ~scheduler ~configuration upstream_update in
  let undecorated_functions_result =
    UndecoratedFunctions.update alias_environment ~scheduler ~configuration upstream_update
  in
  let triggered_dependencies =
    SharedMemoryKeys.DependencyKey.KeySet.union
      (UpdateResult.locally_triggered_dependencies edge_result)
      (UpdateResult.locally_triggered_dependencies undecorated_functions_result)
  in
  UpdateResult.create ~triggered_dependencies ~upstream:upstream_update


module ReadOnly = struct
  type t = {
    edges_read_only: Edges.ReadOnly.t;
    undecorated_function_read_only: UndecoratedFunctions.ReadOnly.t;
    alias_environment: AliasEnvironment.ReadOnly.t;
  }

  let get_edges { edges_read_only; _ } = Edges.ReadOnly.get edges_read_only

  let get_undecorated_function { undecorated_function_read_only; _ } =
    UndecoratedFunctions.ReadOnly.get undecorated_function_read_only


  let alias_environment { alias_environment; _ } = alias_environment

  let hash_to_key_map { edges_read_only; undecorated_function_read_only; _ } =
    Map.merge_skewed
      (Edges.ReadOnly.hash_to_key_map edges_read_only)
      (UndecoratedFunctions.ReadOnly.hash_to_key_map undecorated_function_read_only)
      ~combine:(fun ~key:_ value _ -> value)


  let serialize_decoded { edges_read_only; undecorated_function_read_only; _ } decodable =
    match Edges.ReadOnly.serialize_decoded edges_read_only decodable with
    | Some decoded -> Some decoded
    | None ->
        UndecoratedFunctions.ReadOnly.serialize_decoded undecorated_function_read_only decodable


  let decoded_equal { edges_read_only; undecorated_function_read_only; _ } left right =
    match Edges.ReadOnly.decoded_equal edges_read_only left right with
    | Some result -> Some result
    | None -> UndecoratedFunctions.ReadOnly.decoded_equal undecorated_function_read_only left right


  let check_integrity read_only =
    let unannotated_global_environment =
      alias_environment read_only |> AliasEnvironment.ReadOnly.unannotated_global_environment
    in
    let indices =
      unannotated_global_environment |> UnannotatedGlobalEnvironment.ReadOnly.all_indices
    in
    let class_hierarchy =
      ( module struct
        let edges = get_edges read_only ?dependency:None

        let contains key =
          UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
            unannotated_global_environment
            key
          |> Option.is_some
      end : ClassHierarchy.Handler )
    in
    ClassHierarchy.check_integrity class_hierarchy ~indices
end

module HierarchyReadOnly = ReadOnly

let read_only { alias_environment } =
  {
    ReadOnly.edges_read_only = Edges.read_only alias_environment;
    undecorated_function_read_only = UndecoratedFunctions.read_only alias_environment;
    alias_environment;
  }
