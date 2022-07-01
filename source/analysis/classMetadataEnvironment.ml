(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre
module PreviousEnvironment = ClassHierarchyEnvironment

type class_metadata = {
  successors: Type.Primitive.t list;
  is_test: bool;
  is_final: bool;
  extends_placeholder_stub_class: bool;
  is_abstract: bool;
  is_protocol: bool;
  is_typed_dictionary: bool;
}
[@@deriving compare, show]

module ClassMetadataValue = struct
  type t = class_metadata option

  let prefix = Prefix.make ()

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
      let linearization =
        ClassHierarchy.method_resolution_order_linearize
          ~get_successors:
            (ClassHierarchyEnvironment.ReadOnly.get_edges class_hierarchy_environment ?dependency)
          annotation
      in
      match linearization with
      | _ :: successors -> successors
      | [] -> []
    in
    let successors = successors class_name in
    let is_final =
      definition |> fun { Node.value = definition; _ } -> ClassSummary.is_final definition
    in
    let in_test = List.exists ~f:Type.Primitive.is_unit_test (class_name :: successors) in
    let extends_placeholder_stub_class =
      let empty_stub_environment =
        AliasEnvironment.ReadOnly.empty_stub_environment alias_environment
      in
      definition
      |> AnnotatedBases.extends_placeholder_stub_class
           ~aliases:(AliasEnvironment.ReadOnly.get_alias alias_environment ?dependency)
           ~from_empty_stub:
             (EmptyStubEnvironment.ReadOnly.from_empty_stub empty_stub_environment ?dependency)
    in
    let is_protocol = ClassSummary.is_protocol (Node.value definition) in
    let is_abstract = ClassSummary.is_abstract (Node.value definition) in
    let class_hierarchy =
      ClassHierarchyEnvironment.ReadOnly.class_hierarchy ?dependency class_hierarchy_environment
    in
    let is_typed_dictionary =
      ClassHierarchy.is_typed_dictionary_subclass ~class_hierarchy class_name
    in
    {
      is_test = in_test;
      successors;
      is_final;
      extends_placeholder_stub_class;
      is_protocol;
      is_abstract;
      is_typed_dictionary;
    }
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
    get_class_metadata read_only ?dependency class_name
    >>| (fun { successors; _ } -> successors)
    |> Option.value ~default:[]
end

module MetadataReadOnly = ReadOnly
