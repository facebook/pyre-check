(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre
module PreviousEnvironment = UndecoratedFunctionEnvironment

type class_metadata = {
  successors: Type.Primitive.t list;
  is_test: bool;
  is_final: bool;
  extends_placeholder_stub_class: bool;
  is_abstract: bool;
  is_protocol: bool;
}
[@@deriving eq, compare, show]

module ClassMetadataValue = struct
  type t = class_metadata option

  let prefix = Prefix.make ()

  let description = "Class metadata"

  let unmarshall value = Marshal.from_string value 0

  let compare = Option.compare compare_class_metadata
end

let produce_class_metadata undecorated_function_environment class_name ~track_dependencies =
  let unannotated_global_environment_dependency =
    Option.some_if track_dependencies (SharedMemoryKeys.RegisterClassMetadata class_name)
  in
  let class_hierarchy_environment =
    UndecoratedFunctionEnvironment.ReadOnly.class_hierarchy_environment
      undecorated_function_environment
  in
  let alias_environment =
    ClassHierarchyEnvironment.ReadOnly.alias_environment class_hierarchy_environment
  in
  let unannotated_global_environment =
    alias_environment |> AliasEnvironment.ReadOnly.unannotated_global_environment
  in
  let add definition =
    let successors annotation =
      let linearization =
        let dependency =
          Option.some_if track_dependencies (SharedMemoryKeys.RegisterClassMetadata class_name)
        in
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
      let dependency =
        Option.some_if track_dependencies (SharedMemoryKeys.RegisterClassMetadata class_name)
      in
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
    {
      is_test = in_test;
      successors;
      is_final;
      extends_placeholder_stub_class;
      is_protocol;
      is_abstract;
    }
  in
  UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
    unannotated_global_environment
    class_name
    ?dependency:unannotated_global_environment_dependency
  >>| add


module MetadataTable = Environment.EnvironmentTable.WithCache (struct
  module PreviousEnvironment = PreviousEnvironment
  module Key = SharedMemoryKeys.StringKey
  module Value = ClassMetadataValue

  type trigger = string

  let convert_trigger = Fn.id

  let key_to_trigger = Fn.id

  module TriggerSet = Type.Primitive.Set

  let lazy_incremental = false

  let produce_value = produce_class_metadata

  let filter_upstream_dependency = function
    | SharedMemoryKeys.RegisterClassMetadata name -> Some name
    | _ -> None


  let legacy_invalidated_keys = UnannotatedGlobalEnvironment.UpdateResult.previous_classes

  let all_keys = UnannotatedGlobalEnvironment.ReadOnly.all_classes

  let serialize_value = function
    | Some
        { successors; is_test; is_final; extends_placeholder_stub_class; is_protocol; is_abstract }
      ->
        `Assoc
          [
            "successors", `String (List.to_string ~f:Type.Primitive.show successors);
            "is_test", `Bool is_test;
            "is_final", `Bool is_final;
            "extends_placeholder_stub_class", `Bool extends_placeholder_stub_class;
            "is_abstract", `Bool is_abstract;
            "is_protocol", `Bool is_protocol;
          ]
        |> Yojson.to_string
    | None -> "None"


  let show_key = Fn.id

  let equal_value = Option.equal equal_class_metadata
end)

include MetadataTable

module ReadOnly = struct
  include MetadataTable.ReadOnly

  let get_class_metadata = get

  let undecorated_function_environment = upstream_environment

  let class_hierarchy_environment read_only =
    upstream_environment read_only
    |> UndecoratedFunctionEnvironment.ReadOnly.class_hierarchy_environment


  let successors read_only ?dependency class_name =
    get_class_metadata read_only ?dependency class_name
    >>| (fun { successors; _ } -> successors)
    |> Option.value ~default:[]
end

module MetadataReadOnly = ReadOnly
