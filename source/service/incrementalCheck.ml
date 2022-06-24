(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Analysis
open Pyre

type errors = Analysis.AnalysisError.t list [@@deriving show]

let recheck ~configuration ~scheduler ~environment artifact_paths =
  let timer = Timer.start () in
  Scheduler.once_per_worker scheduler ~configuration ~f:SharedMemory.invalidate_caches;
  SharedMemory.invalidate_caches ();
  SharedMemory.collect `aggressive;
  (* Repopulate the environment. *)
  Log.info "Repopulating the environment...";

  let errors_environment_update_result =
    ErrorsEnvironment.update_this_and_all_preceding_environments
      environment
      ~scheduler
      artifact_paths
  in

  (* Log updates *)
  Statistics.event
    ~section:`Memory
    ~name:"shared memory size"
    ~integers:["size", Memory.heap_size ()]
    ();
  let new_errors =
    ErrorsEnvironment.read_only environment |> ErrorsEnvironment.ReadOnly.get_all_errors
  in
  Log.info "Number of new errors = %d" (List.length new_errors);
  let {
    ErrorsEnvironment.UpdateStatistics.module_updates_count;
    invalidated_modules_count;
    rechecked_functions_count;
    rechecked_modules_count;
  }
    =
    ErrorsEnvironment.UpdateStatistics.count_updates errors_environment_update_result
  in
  Statistics.performance
    ~name:"incremental check"
    ~timer
    ~integers:
      [
        "number of changed files", List.length artifact_paths;
        "number of module tracker updates", module_updates_count;
        "number of parser updates", invalidated_modules_count;
        "number of rechecked modules", rechecked_modules_count;
        "number of re-checked functions", rechecked_functions_count;
      ]
    ();
  ()
