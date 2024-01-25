(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val artifact_path_of_qualifier
  :  source_code_api:SourceCodeApi.t ->
  Ast.Reference.t ->
  ArtifactPath.t option

val module_path_of_artifact_path
  :  source_code_api:SourceCodeApi.t ->
  ArtifactPath.t ->
  Ast.ModulePath.t option
