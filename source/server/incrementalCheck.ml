(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis
open Configuration.Analysis
open Pyre

type errors = Analysis.AnalysisError.t list [@@deriving show]

let recheck
    ~configuration:({ incremental_style; _ } as configuration)
    ~scheduler
    ~environment
    ~errors
    paths
  =
  let timer = Timer.start () in
  let ast_environment = TypeEnvironment.ast_environment environment in
  let module_tracker = AstEnvironment.module_tracker ast_environment in
  let module_updates = ModuleTracker.update module_tracker ~configuration ~paths in
  Scheduler.once_per_worker scheduler ~configuration ~f:SharedMem.invalidate_caches;
  SharedMem.invalidate_caches ();
  SharedMem.collect `aggressive;
  (* Repopulate the environment. *)
  Log.info "Repopulating the environment...";

  let annotated_global_environment_update_result =
    let annotated_global_environment = AnnotatedGlobalEnvironment.create ast_environment in
    AnnotatedGlobalEnvironment.update_this_and_all_preceding_environments
      annotated_global_environment
      ~configuration
      ~scheduler
      (Update module_updates)
  in
  let invalidated_modules =
    AnnotatedGlobalEnvironment.UpdateResult.ast_environment_update_result
      annotated_global_environment_update_result
    |> AstEnvironment.UpdateResult.invalidated_modules
  in
  let recheck_modules, new_errors, total_rechecked_functions =
    match incremental_style with
    | FineGrained ->
        let unannotated_global_environment_update_result =
          AnnotatedGlobalEnvironment.UpdateResult.unannotated_global_environment_update_result
            annotated_global_environment_update_result
        in
        let function_triggers =
          let filter_union sofar keyset =
            let filter registered sofar =
              match SharedMemoryKeys.DependencyKey.get_key registered with
              | SharedMemoryKeys.TypeCheckDefine name -> (
                  match Reference.Map.add sofar ~key:name ~data:registered with
                  | `Duplicate -> sofar
                  | `Ok updated -> updated)
              | _ -> sofar
            in
            SharedMemoryKeys.DependencyKey.RegisteredSet.fold filter keyset sofar
          in
          AnnotatedGlobalEnvironment.UpdateResult.all_triggered_dependencies
            annotated_global_environment_update_result
          |> List.fold ~init:Reference.Map.empty ~f:filter_union
        in
        let recheck_functions =
          let register_and_add sofar trigger =
            let register = function
              | Some existing -> existing
              | None ->
                  SharedMemoryKeys.DependencyKey.Registry.register
                    (SharedMemoryKeys.TypeCheckDefine trigger)
            in
            Reference.Map.update sofar trigger ~f:register
          in
          UnannotatedGlobalEnvironment.UpdateResult.define_additions
            unannotated_global_environment_update_result
          |> Set.fold ~init:function_triggers ~f:register_and_add
        in
        let recheck_functions_list = Map.to_alist recheck_functions in
        let recheck_function_names = List.map recheck_functions_list ~f:fst in

        (* Rerun type checking for triggered functions. *)
        TypeEnvironment.invalidate environment recheck_function_names;
        recheck_functions_list
        |> List.map ~f:(fun (define, registered) -> define, Some registered)
        |> TypeCheck.run_on_defines ~scheduler ~configuration ~environment;

        (* Rerun postprocessing for triggered modules. *)
        let recheck_modules =
          (* For each rechecked function, its containing module needs to be included in
             postprocessing *)
          List.fold
            ~init:(Reference.Set.of_list invalidated_modules)
            (Reference.Map.keys function_triggers)
            ~f:(fun sofar define_name ->
              let unannotated_global_environment =
                UnannotatedGlobalEnvironment.UpdateResult.read_only
                  unannotated_global_environment_update_result
              in
              match
                UnannotatedGlobalEnvironment.ReadOnly.get_define
                  unannotated_global_environment
                  define_name
              with
              | None -> sofar
              | Some { FunctionDefinition.qualifier; _ } -> Set.add sofar qualifier)
          |> Set.to_list
        in

        let errors =
          Analysis.Postprocessing.run
            ~scheduler
            ~configuration
            ~environment:(Analysis.TypeEnvironment.read_only environment)
            recheck_modules
        in

        recheck_modules, errors, Map.length recheck_functions
    | _ ->
        let total_rechecked_functions =
          let unannotated_global_environment_update_result =
            AnnotatedGlobalEnvironment.UpdateResult.unannotated_global_environment_update_result
              annotated_global_environment_update_result
          in
          let previous_defines =
            UnannotatedGlobalEnvironment.UpdateResult.previous_defines
              unannotated_global_environment_update_result
            |> Set.to_list
          in
          let current_defines =
            let unannotated_global_environment =
              UnannotatedGlobalEnvironment.UpdateResult.read_only
                unannotated_global_environment_update_result
            in
            List.concat_map
              invalidated_modules
              ~f:
                (UnannotatedGlobalEnvironment.ReadOnly.all_defines_in_module
                   unannotated_global_environment)
          in
          TypeEnvironment.invalidate environment previous_defines;
          TypeEnvironment.invalidate environment current_defines;

          List.length current_defines
        in
        let errors =
          Analysis.TypeCheck.legacy_run_on_modules
            ~scheduler
            ~configuration
            ~environment
            invalidated_modules;
          Analysis.Postprocessing.run
            ~scheduler
            ~configuration
            ~environment:(Analysis.TypeEnvironment.read_only environment)
            invalidated_modules
        in
        invalidated_modules, errors, total_rechecked_functions
  in
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
      let key = AnalysisError.path error in
      Hashtbl.add_multi errors ~key ~data:error);

  Statistics.performance
    ~name:"incremental check"
    ~timer
    ~integers:
      [
        "number of changed files", List.length paths;
        "number of module tracker updates", List.length module_updates;
        "number of parser updates", List.length invalidated_modules;
        "number of rechecked modules", List.length recheck_modules;
        "number of re-checked functions", total_rechecked_functions;
      ]
    ();
  recheck_modules, new_errors


let recheck_with_state
    ~state:{ State.environment; errors; scheduler; connections; _ }
    ~configuration
    paths
  =
  StatusUpdate.write
    ~message:"Incremental recheck in progress..."
    ~connections
    ~message_type:WarningMessage;
  let _, new_errors = recheck ~configuration ~scheduler ~environment ~errors paths in
  StatusUpdate.write ~message:"Done recheck." ~connections ~message_type:InfoMessage;
  new_errors
