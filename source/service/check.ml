(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Analysis

let check
    ~scheduler
    ~configuration:
      ({ Configuration.Analysis.project_root; source_paths; search_paths; _ } as configuration)
    ~populate_call_graph
  =
  (* Sanity check environment. *)
  let check_directory_exists directory =
    if not (PyrePath.is_directory directory) then
      raise (Invalid_argument (Format.asprintf "`%a` is not a directory" PyrePath.pp directory))
  in
  let check_path_exists path =
    if not (PyrePath.file_exists path) then
      raise (Invalid_argument (Format.asprintf "`%a` is not a valid path" PyrePath.pp path))
  in
  let check_search_path_exists search_path =
    match search_path with
    | SearchPath.Root _
    | SearchPath.Subdirectory _ ->
        check_directory_exists (SearchPath.to_path search_path)
    | SearchPath.Submodule _ -> check_path_exists (SearchPath.to_path search_path)
  in
  source_paths |> List.map ~f:SearchPath.to_path |> List.iter ~f:check_directory_exists;
  check_directory_exists project_root;
  search_paths |> List.iter ~f:check_search_path_exists;
  (* Profiling helper *)
  Profiling.track_shared_memory_usage ~name:"Before module tracking" ();
  Log.info "Building type environment...";

  let timer = Timer.start () in
  let environment =
    EnvironmentControls.create ~populate_call_graph configuration |> ErrorsEnvironment.create
  in
  Statistics.performance ~name:"full environment built" ~timer ();

  (* Run type check and then postprocessing - this is not as efficient as it could be, but we need
     to tune the map-reduce parameters before we can safely combine them into a single step *)
  let () =
    ErrorsEnvironment.type_environment environment
    |> TypeEnvironment.populate_for_project_modules ~scheduler
  in
  let () = ErrorsEnvironment.populate_all_errors ~scheduler environment in
  Profiling.track_shared_memory_usage ();

  (* Only destroy the scheduler if the check command created it. *)
  environment
