(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val artifact_path_of_qualifier
  :  module_tracker:ModuleTracker.ReadOnly.t ->
  Ast.Reference.t ->
  ArtifactPath.t option

val module_path_of_artifact_path
  :  module_tracker:ModuleTracker.ReadOnly.t ->
  ArtifactPath.t ->
  Ast.ModulePath.t option
