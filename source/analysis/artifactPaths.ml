(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast

(* This module provides conversions between analysis representations of a module name (ModulePath /
   qualifier) and ArtifactPaths (i.e. paths in the code Pyre looks at directly, which sometimes is
   the source tree but might also be the output of buck). You should always convert the
   ArtifactPaths to SourcePaths using a BuildSystem before exposing to users. *)

let raw_module_path_of_artifact_path ~configuration path =
  let open Option.Monad_infix in
  let search_paths = Configuration.Analysis.search_paths configuration in
  SearchPath.search_for_path ~search_paths path
  >>| fun SearchPath.{ relative_path; priority } ->
  { ModulePath.Raw.relative = relative_path; priority }


let is_internal_path
    ~configuration:{ Configuration.Analysis.filter_directories; ignore_all_errors; _ }
    path
  =
  let original_raw_path =
    let raw_path = ArtifactPath.raw path in
    (* NOTE(grievejia): Symlink are generally not followed by the type checker. This usage comes
       from legacy code and should not be replicated elsewhere. *)
    PyrePath.follow_symbolic_link raw_path |> Option.value ~default:raw_path
  in
  let source_path_is_covered item =
    PyrePath.equal item original_raw_path
    || PyrePath.directory_contains ~directory:item original_raw_path
  in
  let filter_directories = Option.value filter_directories ~default:[] in
  let ignore_all_errors = Option.value ignore_all_errors ~default:[] in
  List.exists filter_directories ~f:source_path_is_covered
  && not (List.exists ignore_all_errors ~f:source_path_is_covered)


let should_type_check_path
    ~configuration:({ Configuration.Analysis.analyze_external_sources; _ } as configuration)
    path
  =
  analyze_external_sources || is_internal_path ~configuration path


let module_path_of_artifact_path
    ~configuration:({ Configuration.Analysis.excludes; _ } as configuration)
    path
  =
  let open Option.Monad_infix in
  let absolute_path = ArtifactPath.raw path |> PyrePath.absolute in
  if List.exists excludes ~f:(fun regexp -> Str.string_match regexp absolute_path 0) then
    None
  else
    raw_module_path_of_artifact_path ~configuration path
    >>= fun ({ ModulePath.Raw.relative; _ } as raw) ->
    let qualifier =
      match Configuration.Analysis.find_extension configuration path with
      | Some { Configuration.Extension.include_suffix_in_module_qualifier; _ }
        when include_suffix_in_module_qualifier ->
          (* Ensure extension is not stripped when creating qualifier *)
          ModulePath.qualifier_from_relative_path (relative ^ ".py")
      | _ -> ModulePath.qualifier_from_relative_path relative
    in
    Some
      { ModulePath.raw; qualifier; should_type_check = should_type_check_path ~configuration path }


let artifact_path_of_raw_module_path ~configuration { ModulePath.Raw.relative; priority; _ } =
  let root =
    Configuration.Analysis.search_paths configuration
    |> fun search_paths -> List.nth_exn search_paths priority |> SearchPath.get_root
  in
  PyrePath.create_relative ~root ~relative |> ArtifactPath.create


let artifact_path_of_module_path ~configuration { ModulePath.raw; _ } =
  artifact_path_of_raw_module_path ~configuration raw


let artifact_path_of_qualifier ~source_code_api qualifier =
  let configuration = SourceCodeApi.controls source_code_api |> EnvironmentControls.configuration in
  SourceCodeApi.module_path_of_qualifier source_code_api qualifier
  |> Option.map ~f:(artifact_path_of_module_path ~configuration)


let tracked_module_path_of_artifact_path ~source_code_api path =
  let configuration = SourceCodeApi.controls source_code_api |> EnvironmentControls.configuration in
  let open Option in
  module_path_of_artifact_path ~configuration path
  >>= fun { Ast.ModulePath.raw; qualifier; _ } ->
  SourceCodeApi.module_path_of_qualifier source_code_api qualifier
  >>= fun ({ Ast.ModulePath.raw = tracked_raw; _ } as module_path) ->
  Option.some_if (Ast.ModulePath.Raw.equal raw tracked_raw) module_path
