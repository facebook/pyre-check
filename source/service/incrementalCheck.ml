(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
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

  let rechecked_functions_count, rechecked_modules_count =
    let rechecked_functions, rechecked_modules =
      let filter_union sofar keyset =
        let collect_unique registered (sofar_functions, sofar_modules) =
          match SharedMemoryKeys.DependencyKey.get_key registered with
          | SharedMemoryKeys.TypeCheckDefine name -> Set.add sofar_functions name, sofar_modules
          | SharedMemoryKeys.CreateModuleErrors name -> sofar_functions, Set.add sofar_modules name
          | _ -> sofar_functions, sofar_modules
        in
        SharedMemoryKeys.DependencyKey.RegisteredSet.fold collect_unique keyset sofar
      in
      ErrorsEnvironment.UpdateResult.all_triggered_dependencies errors_environment_update_result
      |> List.fold ~init:(Reference.Set.empty, Reference.Set.empty) ~f:filter_union
    in
    Reference.Set.length rechecked_functions, Reference.Set.length rechecked_modules
  in
  let module_updates, invalidated_modules =
    let unannotated_global_environment_update_result =
      ErrorsEnvironment.UpdateResult.unannotated_global_environment_update_result
        errors_environment_update_result
    in
    ( UnannotatedGlobalEnvironment.UpdateResult.module_updates
        unannotated_global_environment_update_result,
      UnannotatedGlobalEnvironment.UpdateResult.invalidated_modules
        unannotated_global_environment_update_result )
  in
  Statistics.performance
    ~name:"incremental check"
    ~timer
    ~integers:
      [
        "number of changed files", List.length artifact_paths;
        "number of module tracker updates", List.length module_updates;
        "number of parser updates", List.length invalidated_modules;
        "number of rechecked modules", rechecked_modules_count;
        "number of re-checked functions", rechecked_functions_count;
      ]
    ();
  ()
