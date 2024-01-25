(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

(* This module provides conversions between analysis representations of a module name (ModulePath /
   qualifier) and ArtifactPaths (i.e. paths in the code Pyre looks at directly, which sometimes is
   the source tree but might also be the output of buck). You should always convert the
   ArtifactPaths to SourcePaths using a BuildSystem before exposing to users. *)

let artifact_path_of_qualifier ~source_code_api qualifier =
  let configuration = SourceCodeApi.controls source_code_api |> EnvironmentControls.configuration in
  SourceCodeApi.module_path_of_qualifier source_code_api qualifier
  |> Option.map ~f:(Ast.ModulePath.full_path ~configuration)


let module_path_of_artifact_path ~source_code_api path =
  let configuration = SourceCodeApi.controls source_code_api |> EnvironmentControls.configuration in
  let open Option in
  Ast.ModulePath.create ~configuration path
  >>= fun { Ast.ModulePath.raw; qualifier; _ } ->
  SourceCodeApi.module_path_of_qualifier source_code_api qualifier
  >>= fun ({ Ast.ModulePath.raw = tracked_raw; _ } as module_path) ->
  Option.some_if (Ast.ModulePath.Raw.equal raw tracked_raw) module_path
