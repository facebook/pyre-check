(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre

type t = { class_metadata_environment: ClassMetadataEnvironment.ReadOnly.t }

let class_metadata_environment { class_metadata_environment } = class_metadata_environment

let class_hierarchy_environment environment =
  class_metadata_environment environment
  |> ClassMetadataEnvironment.ReadOnly.class_hierarchy_environment


let alias_environment environment =
  class_hierarchy_environment environment |> ClassHierarchyEnvironment.ReadOnly.alias_environment


let unannotated_global_environment environment =
  alias_environment environment |> AliasEnvironment.ReadOnly.unannotated_global_environment


let ast_environment environment =
  unannotated_global_environment environment
  |> UnannotatedGlobalEnvironment.ReadOnly.ast_environment


let resolution_implementation ?dependency environment () =
  let class_metadata_environment = class_metadata_environment environment in
  let class_metadata_dependency =
    dependency >>| fun dependency -> ClassMetadataEnvironment.TypeCheckSource dependency
  in
  let dependency =
    dependency >>| fun dependency -> AnnotatedGlobalEnvironment.TypeCheckSource dependency
  in
  let annotated_global_environment =
    AnnotatedGlobalEnvironment.create class_metadata_environment
    |> AnnotatedGlobalEnvironment.read_only
  in
  GlobalResolution.create
    ?dependency:class_metadata_dependency
    ~class_metadata_environment
    ~global:
      (AnnotatedGlobalEnvironment.ReadOnly.get_global ?dependency annotated_global_environment)
    (module Annotated.Class)


let resolution = resolution_implementation ?dependency:None

let dependency_tracked_resolution environment ~dependency () =
  resolution_implementation ~dependency environment ()


let is_module environment = AstEnvironment.ReadOnly.is_module (ast_environment environment)

let update_and_compute_dependencies environment ~scheduler ~configuration update_result =
  let annotated_global_environment =
    class_metadata_environment environment |> AnnotatedGlobalEnvironment.create
  in
  AnnotatedGlobalEnvironment.update
    annotated_global_environment
    ~scheduler
    ~configuration
    update_result
  |> AnnotatedGlobalEnvironment.UpdateResult.triggered_dependencies
  |> AnnotatedGlobalEnvironment.DependencyKey.KeySet.elements
  |> List.map ~f:(fun (AnnotatedGlobalEnvironment.TypeCheckSource dependency) -> dependency)
  |> SharedMemoryKeys.ReferenceDependencyKey.KeySet.of_list


let shared_memory_handler class_metadata_environment = { class_metadata_environment }

let hash_to_key_map environment =
  let annotated_global_environment =
    class_metadata_environment environment
    |> AnnotatedGlobalEnvironment.create
    |> AnnotatedGlobalEnvironment.read_only
  in
  AnnotatedGlobalEnvironment.ReadOnly.hash_to_key_map annotated_global_environment


let serialize_decoded environment =
  let annotated_global_environment =
    class_metadata_environment environment
    |> AnnotatedGlobalEnvironment.create
    |> AnnotatedGlobalEnvironment.read_only
  in
  AnnotatedGlobalEnvironment.ReadOnly.serialize_decoded annotated_global_environment


let decoded_equal environment =
  let annotated_global_environment =
    class_metadata_environment environment
    |> AnnotatedGlobalEnvironment.create
    |> AnnotatedGlobalEnvironment.read_only
  in
  AnnotatedGlobalEnvironment.ReadOnly.decoded_equal annotated_global_environment
