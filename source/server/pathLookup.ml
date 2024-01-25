(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The PathLookup module provides helpers for converting between `SourcePath.t` and qualifiers
   (`Reference.t`).

   This always requires converting between `ArtifactPath.t` and `SourcePath.t` (the two are
   different because we need to account for builds that might remap the paths; for example under
   buck the `ArtifactPath.t` is the result of a buck build and the `SourcePath.t` is the user-facing
   path in the original source tree).

   The conversion functions have two forms, one that uses dependency injection for converting and
   another that takes a `BuildSystem.t` as input. *)

open Base

let qualifiers_of_source_path ~lookup_artifact ~source_code_api path =
  lookup_artifact path
  |> List.filter_map ~f:(Analysis.ArtifactPaths.module_path_of_artifact_path ~source_code_api)
  |> List.map ~f:Ast.ModulePath.qualifier


let qualifiers_of_source_path_with_build_system ~build_system ~source_code_api path =
  qualifiers_of_source_path
    ~lookup_artifact:(BuildSystem.lookup_artifact build_system)
    ~source_code_api
    path


let absolute_source_path_of_qualifier ~lookup_source ~source_code_api qualifier =
  match Analysis.ArtifactPaths.artifact_path_of_qualifier ~source_code_api qualifier with
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


let absolute_source_path_of_qualifier_with_build_system ~build_system ~source_code_api qualifier =
  absolute_source_path_of_qualifier
    ~lookup_source:(BuildSystem.lookup_source build_system)
    ~source_code_api
    qualifier
