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

type types_by_location = ((Location.t * Type.t) list, error_reason) Result.t

let get_lookup ~configuration ~build_system ~environment path =
  let generate_lookup_for_existent_path { SourcePath.qualifier; _ } =
    let timer = Timer.start () in
    let lookup =
      LocationBasedLookup.create_of_module (TypeEnvironment.read_only environment) qualifier
    in
    Log.log
      ~section:`Performance
      "locationBasedLookupProcessor: create_of_module: %d"
      (Timer.stop_in_ms timer);
    Result.Ok lookup
  in
  let generate_lookup_for_nonexistent_path error_reason = Result.Error error_reason in
  let full_path =
    let { Configuration.Analysis.local_root = root; _ } = configuration in
    PyrePath.create_relative ~root ~relative:path
  in
  match BuildSystem.lookup_artifact build_system full_path with
  | [] -> generate_lookup_for_nonexistent_path FileNotFound
  | artifact_path :: _ -> (
      (* If a source path corresponds to multiple artifacts, randomly pick an artifact and compute
         results for it. *)
      let module_tracker = TypeEnvironment.module_tracker environment |> ModuleTracker.read_only in
      match ModuleTracker.ReadOnly.lookup_path module_tracker artifact_path with
      | ModuleTracker.PathLookup.Found source_path -> generate_lookup_for_existent_path source_path
      | ModuleTracker.PathLookup.ShadowedBy _ -> generate_lookup_for_nonexistent_path StubShadowing
      | ModuleTracker.PathLookup.NotFound -> generate_lookup_for_nonexistent_path FileNotFound)


let find_all_resolved_types_for_path ~environment ~build_system ~configuration path =
  let open Result in
  get_lookup ~configuration ~environment ~build_system path
  >>| LocationBasedLookup.get_all_resolved_types
  >>| List.sort ~compare:[%compare: Location.t * Type.t]
