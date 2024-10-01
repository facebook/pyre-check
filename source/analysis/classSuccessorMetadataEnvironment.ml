(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* ClassSuccessorMetadataEnvironment: layer of the environment stack
 * - upstream: ClassHierarchyEnvironment
 * - downstream: AttributeResolution
 * - key: the name type, as an Identifier.t
 * - value: ClassSuccessorMetadataEnvironment.class_meatadata
 *
 * The ClassSuccessorMetadataEnvironment is a cache for metadata about classes
 * that require the full ancestors (`successors`), which includes
 * - the successors list itself
 * - several flags computed based on inheritance
 *)

open Core
open Ast
open Pyre
module PreviousEnvironment = ClassHierarchyEnvironment

type class_metadata = {
  (* `None` means successor computation for the given class failed (due to, e.g. inconsistent MRO).
     TODO: Decide how `successors = None` would interact with other flags. Should we auto-derive
     some of them based on the value of `successors`? *)
  successors: Type.Primitive.t list option;
  is_test: bool;
  is_mock: bool;
  is_final: bool;
  is_abstract: bool;
  is_protocol: bool;
  is_typed_dictionary: bool;
  extends_enum: bool;
}
[@@deriving compare, show]

module IncomingDataComputation = struct
  module Queries = struct
    type t = {
      get_class_summary: string -> ClassSummary.t Node.t option;
      get_class_hierarchy: unit -> (module ClassHierarchy.Handler);
    }
  end

  let produce_class_metadata Queries.{ get_class_hierarchy; get_class_summary; _ } class_name =
    let add definition =
      let successors annotation =
        let (module Handler) = get_class_hierarchy () in
        match
          ClassHierarchy.method_resolution_order_linearize
            ~get_parents:(ClassHierarchy.parents_of (module Handler))
            annotation
        with
        | Result.Ok (_ :: successors) -> Some successors
        | Result.Ok [] -> Some []
        | Result.Error _ -> None
      in
      let successors = successors class_name in
      let successors_or_empty = Option.value successors ~default:[] in
      let extends_enum =
        not
          (Set.is_empty
             (Set.inter Recognized.enumeration_classes (String.Set.of_list successors_or_empty)))
      in
      let is_final =
        definition
        |> fun { Node.value = definition; _ } ->
        (* Enums with defined members are implicitly final
           https://typing.readthedocs.io/en/latest/spec/enums.html#enum-behaviors *)
        ClassSummary.is_final definition
        || (ClassSummary.has_possible_enum_members definition && extends_enum)
      in
      let is_test =
        List.exists ~f:Type.Primitive.is_unit_test (class_name :: successors_or_empty)
      in
      let is_mock =
        let is_mock_class = function
          | "typing.Any"
          | "unittest.mock.Base"
          | "mock.Base"
          | "unittest.mock.NonCallableMock"
          | "mock.NonCallableMock" ->
              true
          | _ -> false
        in
        List.exists ~f:is_mock_class (class_name :: successors_or_empty)
      in
      let is_protocol = ClassSummary.is_protocol (Node.value definition) in
      let is_abstract = ClassSummary.is_abstract (Node.value definition) in
      let is_typed_dictionary =
        let total_typed_dictionary_name = Type.TypedDictionary.class_name ~total:true in
        let non_total_typed_dictionary_name = Type.TypedDictionary.class_name ~total:false in
        List.exists
          ~f:(fun class_name ->
            Identifier.equal total_typed_dictionary_name class_name
            || Identifier.equal non_total_typed_dictionary_name class_name)
          successors_or_empty
      in
      {
        is_test;
        is_mock;
        successors;
        is_final;
        is_protocol;
        is_abstract;
        is_typed_dictionary;
        extends_enum;
      }
    in
    get_class_summary class_name >>| add
end

module OutgoingDataComputation = struct
  module Queries = struct
    type t = { get_class_metadata: Type.Primitive.t -> class_metadata option }
  end

  let successors { Queries.get_class_metadata; _ } class_name =
    match get_class_metadata class_name with
    | Some { successors = Some successors; _ } -> successors
    | _ -> []


  (* NOTE: This function is not symetric: least_upper_bound(A, B) can return different result from
     least_upper_bound(B, A) when multiple inheritance is involved. *)
  let least_upper_bound { Queries.get_class_metadata; _ } left right =
    match get_class_metadata left, get_class_metadata right with
    | Some { successors = Some left_mro; _ }, Some { successors = Some right_mro; _ } ->
        let right_mro_set = String.Hash_set.of_list (right :: right_mro) in
        List.find (left :: left_mro) ~f:(Hash_set.mem right_mro_set)
    | _, _ -> None


  let has_transitive_successor queries ~successor predecessor =
    let counts_as_extends_target current = [%compare.equal: Type.Primitive.t] current successor in
    let successors_of_predecessor = successors queries predecessor in
    List.exists (predecessor :: successors_of_predecessor) ~f:counts_as_extends_target
end

module ClassMetadataValue = struct
  type t = class_metadata option

  let prefix = Hack_parallel.Std.Prefix.make ()

  let description = "Class metadata"

  let equal = Memory.equal_from_compare (Option.compare compare_class_metadata)
end

module MetadataTable = Environment.EnvironmentTable.WithCache (struct
  module PreviousEnvironment = PreviousEnvironment
  module Key = SharedMemoryKeys.StringKey
  module Value = ClassMetadataValue

  type trigger = string [@@deriving sexp, compare]

  let convert_trigger = Fn.id

  let key_to_trigger = Fn.id

  module TriggerSet = Type.Primitive.Set

  let lazy_incremental = false

  let produce_value class_hierarchy_environment class_name ~dependency =
    let unannotated_global_environment =
      class_hierarchy_environment
      |> ClassHierarchyEnvironment.ReadOnly.alias_environment
      |> TypeAliasEnvironment.ReadOnly.unannotated_global_environment
    in
    let upstream =
      IncomingDataComputation.Queries.
        {
          get_class_summary =
            UnannotatedGlobalEnvironment.ReadOnly.get_class_summary
              ?dependency
              unannotated_global_environment;
          get_class_hierarchy =
            (fun () ->
              ClassHierarchyEnvironment.ReadOnly.class_hierarchy
                class_hierarchy_environment
                ?dependency);
        }
    in
    IncomingDataComputation.produce_class_metadata upstream class_name


  let filter_upstream_dependency = function
    | SharedMemoryKeys.RegisterClassMetadata name -> Some name
    | _ -> None


  let trigger_to_dependency name = SharedMemoryKeys.RegisterClassMetadata name

  let show_key = Fn.id

  let overlay_owns_key source_code_overlay =
    SourceCodeIncrementalApi.Overlay.owns_identifier source_code_overlay


  let equal_value = Option.equal [%compare.equal: class_metadata]
end)

include MetadataTable

module ReadOnly = struct
  include MetadataTable.ReadOnly

  let get_class_metadata = get

  let is_class_typed_dictionary read_only ?dependency class_name =
    get read_only ?dependency class_name
    |> Option.value_map ~default:false ~f:(fun ({ is_typed_dictionary; _ } : class_metadata) ->
           is_typed_dictionary)


  let does_class_extend_enum read_only ?dependency class_name =
    get read_only ?dependency class_name
    |> Option.value_map ~default:false ~f:(fun ({ extends_enum; _ } : class_metadata) ->
           extends_enum)


  let class_hierarchy_environment = upstream_environment

  let unannotated_global_environment read_only =
    class_hierarchy_environment read_only
    |> ClassHierarchyEnvironment.ReadOnly.unannotated_global_environment


  let from_pure_logic ?dependency read_only f =
    f
      OutgoingDataComputation.Queries.
        { get_class_metadata = get_class_metadata ?dependency read_only }


  let successors read_only ?dependency =
    from_pure_logic ?dependency read_only OutgoingDataComputation.successors


  let has_transitive_successor read_only ?dependency =
    from_pure_logic ?dependency read_only OutgoingDataComputation.has_transitive_successor


  let least_upper_bound read_only ?dependency =
    from_pure_logic ?dependency read_only OutgoingDataComputation.least_upper_bound
end

module MetadataReadOnly = ReadOnly
