(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

(* This module provides conversions between analysis representations of a module name (ModulePath /
   qualifier) and SourcePaths (i.e. paths in the user codebase, which may have been translated
   through a BuildSystem such as buck before being analyzed by Pyre *)

let qualifiers_of_source_path ~lookup_artifact ~source_code_api path =
  lookup_artifact path
  |> List.filter_map ~f:(ArtifactPaths.tracked_module_path_of_artifact_path ~source_code_api)
  |> List.map ~f:Ast.ModulePath.qualifier


let absolute_source_path_of_qualifier ~lookup_source ~source_code_api qualifier =
  match ArtifactPaths.artifact_path_of_qualifier ~source_code_api qualifier with
  | None -> None
  | Some analysis_path ->
      let path =
        match lookup_source analysis_path with
        | Some source_path -> source_path |> SourcePath.raw
        | None ->
            (* NOTE (grievejia): This means the path is under the search roots but is not tracked by
               Buck. Showing the original path here is a compromise: ideally we should instead look
               into configuring Buck-built project in such a way that all source files are tracked
               by Buck. *)
            ArtifactPath.raw analysis_path
      in
      Some (PyrePath.absolute path)
