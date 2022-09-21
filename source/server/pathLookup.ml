(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Base

let module_of_path ~module_tracker path =
  match Analysis.ModuleTracker.ReadOnly.lookup_path module_tracker path with
  | Analysis.ModuleTracker.PathLookup.Found { Ast.ModulePath.qualifier; _ } -> Some qualifier
  | ShadowedBy _
  | NotFound ->
      None


let modules_of_source_path ~lookup_artifact ~module_tracker path =
  lookup_artifact path |> List.filter_map ~f:(module_of_path ~module_tracker)


let modules_of_source_path_with_build_system ~build_system ~module_tracker path =
  modules_of_source_path
    ~lookup_artifact:(BuildSystem.lookup_artifact build_system)
    ~module_tracker
    path


let instantiate_path ~lookup_source ~module_tracker qualifier =
  match Analysis.ModuleTracker.ReadOnly.lookup_full_path module_tracker qualifier with
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


let instantiate_path_with_build_system ~build_system ~module_tracker qualifier =
  instantiate_path ~lookup_source:(BuildSystem.lookup_source build_system) ~module_tracker qualifier
