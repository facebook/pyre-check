(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Given an artifact path, return the corresponding module name for that path, or [None] if the
    path is not tracked. *)
val module_of_path
  :  module_tracker:Analysis.ModuleTracker.ReadOnly.t ->
  ArtifactPath.t ->
  Ast.Reference.t option

(** Given a source path, return the corresponding module names for that path. This API will take
    into account any potential path translation done by [lookup_artifact].*)
val modules_of_source_path
  :  lookup_artifact:(SourcePath.t -> ArtifactPath.t list) ->
  module_tracker:Analysis.ModuleTracker.ReadOnly.t ->
  SourcePath.t ->
  Ast.Reference.t list

(** Given a source path, return the corresponding module names for that path. This API will take
    into account any potential path translation done by the {!BuildSystem.t}.*)
val modules_of_source_path_with_build_system
  :  build_system:BuildSystem.t ->
  module_tracker:Analysis.ModuleTracker.ReadOnly.t ->
  SourcePath.t ->
  Ast.Reference.t list

(** Given a Python module name, Return path to the corresponding Python source file as a string.
    This API will take into account any potential path translation done by [lookup_source]. *)
val instantiate_path
  :  lookup_source:(ArtifactPath.t -> SourcePath.t option) ->
  module_tracker:Analysis.ModuleTracker.ReadOnly.t ->
  Ast.Reference.t ->
  string option

(** Given a Python module name, Return path to the corresponding Python source file as a string.
    This API will take into account any potential path translation done by the {!BuildSystem.t}. *)
val instantiate_path_with_build_system
  :  build_system:BuildSystem.t ->
  module_tracker:Analysis.ModuleTracker.ReadOnly.t ->
  Ast.Reference.t ->
  string option
