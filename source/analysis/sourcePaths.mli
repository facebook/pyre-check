(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Given a source path, return the corresponding module names for that path. This API will take
    into account any potential path translation done by [lookup_artifact].*)
val qualifiers_of_source_path
  :  lookup_artifact:(SourcePath.t -> ArtifactPath.t list) ->
  source_code_api:SourceCodeApi.t ->
  SourcePath.t ->
  Ast.Reference.t list

(** Given a Python module name, Return path to the corresponding Python source file as a string.
    This API will take into account any potential path translation done by [lookup_source]. *)
val absolute_source_path_of_qualifier
  :  lookup_source:(ArtifactPath.t -> SourcePath.t option) ->
  source_code_api:SourceCodeApi.t ->
  Ast.Reference.t ->
  string option
