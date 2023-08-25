(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* ClassMetadataEnvironment: layer of the environment stack
 * - upstream: ClassHierarchyEnvironment
 * - downstream: AttributeResolution
 * - key: the name type, as an Identifier.t
 * - value: ClassMetadataEnvironment.class_meatadata
 *
 * The ClassMetadataEnvironment is a cache for metadata about classes
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
}
[@@deriving compare, show]

module ClassMetadataValue = struct
  type t = class_metadata option

  let prefix = Hack_parallel.Std.Prefix.make ()

  let description = "Class metadata"

  let equal = Memory.equal_from_compare (Option.compare compare_class_metadata)
end

let produce_class_metadata class_hierarchy_environment class_name ~dependency =
  let alias_environment =
    ClassHierarchyEnvironment.ReadOnly.alias_environment class_hierarchy_environment
  in
  let unannotated_global_environment =
    alias_environment |> AliasEnvironment.ReadOnly.unannotated_global_environment
  in
  let add definition =
    let successors annotation =
      let (module Handler) =
        ClassHierarchyEnvironment.ReadOnly.class_hierarchy class_hierarchy_environment ?dependency
      in
      match
        ClassHierarchy.method_resolution_order_linearize
          ~get_successors:(ClassHierarchy.parents_of (module Handler))
          annotation
      with
      | Result.Ok (_ :: successors) -> Some successors
      | Result.Ok [] -> Some []
      | Result.Error _ -> None
    in
    let successors = successors class_name in
    let is_final =
      definition |> fun { Node.value = definition; _ } -> ClassSummary.is_final definition
    in
    let is_test =
      List.exists ~f:Type.Primitive.is_unit_test (class_name :: Option.value successors ~default:[])
    in
    let is_mock =
      let is_mock_class = function
        | "unittest.mock.Base"
        | "mock.Base"
        | "unittest.mock.NonCallableMock"
        | "mock.NonCallableMock" ->
            true
        | _ -> false
      in
      List.exists ~f:is_mock_class (class_name :: Option.value successors ~default:[])
    in
    let is_protocol = ClassSummary.is_protocol (Node.value definition) in
    let is_abstract = ClassSummary.is_abstract (Node.value definition) in
    let is_typed_dictionary =
      let total_typed_dictionary_name = Type.TypedDictionary.class_name ~total:true in
      List.exists
        ~f:([%compare.equal: Type.Primitive.t] total_typed_dictionary_name)
        (Option.value successors ~default:[])
    in
    { is_test; is_mock; successors; is_final; is_protocol; is_abstract; is_typed_dictionary }
  in
  UnannotatedGlobalEnvironment.ReadOnly.get_class_summary
    unannotated_global_environment
    class_name
    ?dependency
  >>| add


module MetadataTable = Environment.EnvironmentTable.WithCache (struct
  module PreviousEnvironment = PreviousEnvironment
  module Key = SharedMemoryKeys.StringKey
  module Value = ClassMetadataValue

  type trigger = string [@@deriving sexp, compare]

  let convert_trigger = Fn.id

  let key_to_trigger = Fn.id

  module TriggerSet = Type.Primitive.Set

  let lazy_incremental = false

  let produce_value = produce_class_metadata

  let filter_upstream_dependency = function
    | SharedMemoryKeys.RegisterClassMetadata name -> Some name
    | _ -> None


  let trigger_to_dependency name = SharedMemoryKeys.RegisterClassMetadata name

  let show_key = Fn.id

  let overlay_owns_key module_tracker_overlay =
    ModuleTracker.Overlay.owns_identifier module_tracker_overlay


  let equal_value = Option.equal [%compare.equal: class_metadata]
end)

include MetadataTable

module ReadOnly = struct
  include MetadataTable.ReadOnly

  let get_class_metadata = get

  let is_typed_dictionary read_only ?dependency class_name =
    get read_only ?dependency class_name
    |> Option.value_map ~default:false ~f:(fun ({ is_typed_dictionary; _ } : class_metadata) ->
           is_typed_dictionary)


  let class_hierarchy_environment = upstream_environment

  let successors read_only ?dependency class_name =
    match get_class_metadata read_only ?dependency class_name with
    | Some { successors = Some successors; _ } -> successors
    | _ -> []


  let is_transitive_successor read_only ?dependency ~placeholder_subclass_extends_all ~target source
    =
    let class_hierarcy =
      ClassHierarchyEnvironment.ReadOnly.class_hierarchy
        ?dependency
        (class_hierarchy_environment read_only)
    in
    let extends_placeholder_stub = ClassHierarchy.extends_placeholder_stub class_hierarcy in
    let counts_as_extends_target current =
      [%compare.equal: Type.Primitive.t] current target
      || (placeholder_subclass_extends_all && extends_placeholder_stub current)
    in
    let successors_of_source = successors read_only ?dependency source in
    List.exists (source :: successors_of_source) ~f:counts_as_extends_target
end

module MetadataReadOnly = ReadOnly
