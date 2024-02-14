(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val raw_module_path_of_artifact_path
  :  configuration:Configuration.Analysis.t ->
  ArtifactPath.t ->
  Ast.ModulePath.Raw.t option

val module_path_of_artifact_path
  :  configuration:Configuration.Analysis.t ->
  ArtifactPath.t ->
  Ast.ModulePath.t option

val artifact_path_of_raw_module_path
  :  configuration:Configuration.Analysis.t ->
  Ast.ModulePath.Raw.t ->
  ArtifactPath.t

val artifact_path_of_module_path
  :  configuration:Configuration.Analysis.t ->
  Ast.ModulePath.t ->
  ArtifactPath.t

val artifact_path_of_qualifier
  :  source_code_api:SourceCodeApi.t ->
  Ast.Reference.t ->
  ArtifactPath.t option

val tracked_module_path_of_artifact_path
  :  source_code_api:SourceCodeApi.t ->
  ArtifactPath.t ->
  Ast.ModulePath.t option

(* TODO(T178992543): This is currently exposed for Pysa only and should eventually be removed *)
val is_internal_path : configuration:Configuration.Analysis.t -> ArtifactPath.t -> bool
