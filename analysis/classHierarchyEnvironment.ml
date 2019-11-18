(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre
open Expression
module PreviousEnvironment = AliasEnvironment

let unannotated_global_environment alias_environment =
  AliasEnvironment.ReadOnly.unannotated_global_environment alias_environment


module EdgeValue = struct
  type t = ClassHierarchy.Target.t list option [@@deriving compare]

  let prefix = Prefix.make ()

  let description = "Edges"

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
                    (unannotated_global_environment alias_environment)
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
    (unannotated_global_environment alias_environment)
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


module Edges = Environment.EnvironmentTable.WithCache (struct
  module PreviousEnvironment = PreviousEnvironment
  module Key = IndexTracker.IndexKey
  module Value = EdgeValue

  type trigger = string

  let convert_trigger = IndexTracker.index

  let key_to_trigger = IndexTracker.annotation

  module TriggerSet = Type.Primitive.Set

  let produce_value = get_parents

  let filter_upstream_dependency = function
    | SharedMemoryKeys.ClassConnect name -> Some name
    | _ -> None


  let legacy_invalidated_keys = UnannotatedGlobalEnvironment.UpdateResult.previous_classes

  let all_keys unannotated_global_environment =
    UnannotatedGlobalEnvironment.ReadOnly.all_classes unannotated_global_environment
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

module ReadOnly = struct
  include Edges.ReadOnly

  let alias_environment = upstream_environment

  let get_edges = get

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

let update ~scheduler ~configuration:({ Configuration.Analysis.debug; _ } as configuration) upstream
  =
  let result = Edges.update ~scheduler ~configuration upstream in
  let read_only = Edges.UpdateResult.read_only result in
  if debug then
    ReadOnly.check_integrity read_only;
  result


module HierarchyReadOnly = ReadOnly
module UpdateResult = Edges.UpdateResult
