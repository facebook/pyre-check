(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Analysis

let check ~scheduler ~configuration ~populate_call_graph =
  Configuration.Analysis.validate_paths configuration;
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
