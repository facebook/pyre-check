(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis

type error_reason =
  | StubShadowing
  | FileNotFound
[@@deriving sexp, show, compare, to_yojson]

type types_by_location = ((Location.t * Type.t) list, error_reason) Result.t

type coverage_by_location = (LocationBasedLookup.coverage_for_path, error_reason) Result.t

let get_lookup ~build_system ~environment path =
  let generate_lookup_for_existent_path { ModulePath.qualifier; _ } =
    let timer = Timer.start () in
    let lookup = LocationBasedLookup.create_of_module environment qualifier in
    Log.log
      ~section:`Performance
      "locationBasedLookupProcessor: create_of_module: %d"
      (Timer.stop_in_ms timer);
    Result.Ok lookup
  in
  let generate_lookup_for_nonexistent_path error_reason = Result.Error error_reason in
  let full_path =
    let { Configuration.Analysis.local_root = root; _ } =
      TypeEnvironment.ReadOnly.controls environment |> EnvironmentControls.configuration
    in
    PyrePath.create_relative ~root ~relative:path |> SourcePath.create
  in
  match BuildSystem.lookup_artifact build_system full_path with
  | [] -> generate_lookup_for_nonexistent_path FileNotFound
  | analysis_path :: _ -> (
      (* If a source path corresponds to multiple artifacts, randomly pick an artifact and compute
         results for it. *)
      let module_tracker = TypeEnvironment.ReadOnly.module_tracker environment in
      match ModuleTracker.ReadOnly.lookup_path module_tracker analysis_path with
      | ModuleTracker.PathLookup.Found module_path -> generate_lookup_for_existent_path module_path
      | ModuleTracker.PathLookup.ShadowedBy _ -> generate_lookup_for_nonexistent_path StubShadowing
      | ModuleTracker.PathLookup.NotFound -> generate_lookup_for_nonexistent_path FileNotFound)


let find_all_resolved_types_for_path ~environment ~build_system path =
  let open Result in
  get_lookup ~environment ~build_system path
  >>| LocationBasedLookup.get_all_nodes_and_coverage_data
  >>| List.map ~f:(fun (location, { LocationBasedLookup.type_; expression = _ }) -> location, type_)
  >>| List.sort ~compare:[%compare: Location.t * Type.t]


let find_expression_level_coverage_for_path ~environment ~build_system path =
  let open Result in
  get_lookup ~environment ~build_system path >>| LocationBasedLookup.get_expression_level_coverage
