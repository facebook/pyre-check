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

let recheck ~configuration ~scheduler ~environment ~errors artifact_paths =
  let timer = Timer.start () in
  Scheduler.once_per_worker scheduler ~configuration ~f:SharedMemory.invalidate_caches;
  SharedMemory.invalidate_caches ();
  SharedMemory.collect `aggressive;
  (* Repopulate the environment. *)
  Log.info "Repopulating the environment...";

  let type_environment_update_result =
    TypeEnvironment.update_this_and_all_preceding_environments environment ~scheduler artifact_paths
  in
  let invalidated_modules =
    TypeEnvironment.UpdateResult.invalidated_modules type_environment_update_result
  in
  let unannotated_global_environment_update_result =
    TypeEnvironment.UpdateResult.unannotated_global_environment_update_result
      type_environment_update_result
  in
  let triggered_dependency_function_names =
    let filter_union sofar keyset =
      let filter registered sofar =
        match SharedMemoryKeys.DependencyKey.get_key registered with
        | SharedMemoryKeys.TypeCheckDefine name -> Reference.Set.add sofar name
        | _ -> sofar
      in
      SharedMemoryKeys.DependencyKey.RegisteredSet.fold filter keyset sofar
    in
    TypeEnvironment.UpdateResult.all_triggered_dependencies type_environment_update_result
    |> List.fold ~init:Reference.Set.empty ~f:filter_union
  in
  (* Rerun postprocessing for triggered modules. *)
  let recheck_modules =
    (* For each rechecked function, its containing module needs to be included in postprocessing *)
    List.fold
      ~init:(Reference.Set.of_list invalidated_modules)
      (Reference.Set.to_list triggered_dependency_function_names)
      ~f:(fun sofar define_name ->
        let unannotated_global_environment =
          TypeEnvironment.read_only environment
          |> TypeEnvironment.ReadOnly.unannotated_global_environment
        in
        match
          UnannotatedGlobalEnvironment.ReadOnly.get_function_definition
            unannotated_global_environment
            define_name
        with
        | None -> sofar
        | Some { FunctionDefinition.qualifier; _ } -> Set.add sofar qualifier)
    |> Set.to_list
  in

  let new_errors =
    Analysis.Postprocessing.run
      ~scheduler
      ~environment:(Analysis.TypeEnvironment.read_only environment)
      recheck_modules
  in
  let rechecked_functions_count = Set.length triggered_dependency_function_names in
  Statistics.event
    ~section:`Memory
    ~name:"shared memory size"
    ~integers:["size", Memory.heap_size ()]
    ();

  (* Kill all previous errors for new files we just checked *)
  List.iter ~f:(Hashtbl.remove errors) recheck_modules;

  (* Associate the new errors with new files *)
  Log.info "Number of new errors = %d" (List.length new_errors);
  List.iter new_errors ~f:(fun error ->
      let key = AnalysisError.module_reference error in
      Hashtbl.add_multi errors ~key ~data:error);

  let module_updates =
    UnannotatedGlobalEnvironment.UpdateResult.module_updates
      unannotated_global_environment_update_result
  in
  Statistics.performance
    ~name:"incremental check"
    ~timer
    ~integers:
      [
        "number of changed files", List.length artifact_paths;
        "number of module tracker updates", List.length module_updates;
        "number of parser updates", List.length invalidated_modules;
        "number of rechecked modules", List.length recheck_modules;
        "number of re-checked functions", rechecked_functions_count;
      ]
    ();
  recheck_modules, new_errors
