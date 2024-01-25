(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* locationBasedLookupProcessor is a wrapper around LocationBasedLookup.create_of_module that finds
   the path and creates a lookup. It is used in both hover and expression-level coverage. *)

open Core
open Ast
open Analysis

type error_reason = FileNotFound [@@deriving sexp, show, compare, to_yojson]

type types_by_location = ((Location.t * Type.t) list, error_reason) Result.t

type coverage_by_location = (LocationBasedLookup.coverage_for_path, error_reason) Result.t

type module_path = (Ast.ModulePath.t, error_reason) Result.t

let get_module_path ~type_environment ~build_system path =
  let full_path =
    let { Configuration.Analysis.local_root = root; _ } =
      TypeEnvironment.ReadOnly.controls type_environment |> EnvironmentControls.configuration
    in
    PyrePath.create_relative ~root ~relative:path |> SourcePath.create
  in
  match BuildSystem.lookup_artifact build_system full_path with
  | [] -> Result.Error FileNotFound
  | analysis_path :: _ -> (
      (* If a source path corresponds to multiple artifacts, randomly pick an artifact and compute
         results for it. *)
      let source_code_api =
        TypeEnvironment.ReadOnly.get_untracked_source_code_api type_environment
      in
      match ArtifactPaths.module_path_of_artifact_path ~source_code_api analysis_path with
      | Some module_path -> Result.Ok module_path
      | None -> Result.Error FileNotFound)


let get_lookup ~build_system ~type_environment path =
  let module_path = get_module_path ~type_environment ~build_system path in
  let generate_lookup_for_existent_path { ModulePath.qualifier; _ } =
    let timer = Timer.start () in
    let lookup = LocationBasedLookup.create_of_module type_environment qualifier in
    Log.log
      ~section:`Performance
      "locationBasedLookupProcessor: create_of_module: %d"
      (Timer.stop_in_ms timer);
    Result.Ok lookup
  in
  match module_path with
  | Result.Ok path -> generate_lookup_for_existent_path path
  | Result.Error error_reason -> Result.Error error_reason


let find_all_resolved_types_for_path ~type_environment ~build_system path =
  let open Result in
  get_lookup ~type_environment ~build_system path
  >>| LocationBasedLookup.get_all_nodes_and_coverage_data
  >>| List.map ~f:(fun (location, { LocationBasedLookup.type_; expression = _ }) -> location, type_)
  >>| List.sort ~compare:[%compare: Location.t * Type.t]


let find_expression_level_coverage_for_path ~type_environment ~build_system path =
  let open Result in
  get_lookup ~type_environment ~build_system path
  >>| LocationBasedLookup.get_expression_level_coverage
