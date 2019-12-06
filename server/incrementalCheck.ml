(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
module ModuleTracker = Analysis.ModuleTracker
open Ast
open Analysis
open State
open Configuration.Analysis
open Pyre

type errors = State.Error.t list [@@deriving show]

let recheck
    ~module_tracker
    ~ast_environment
    ~errors
    ~scheduler
    ~connections
    ~lookups
    ~configuration:({ incremental_style; _ } as configuration)
    paths
  =
  let timer = Timer.start () in
  let module_updates = ModuleTracker.update module_tracker ~configuration ~paths in
  let scheduler =
    Scheduler.with_parallel scheduler ~is_parallel:(List.length module_updates > 10)
  in
  Scheduler.once_per_worker scheduler ~configuration ~f:SharedMem.invalidate_caches;
  SharedMem.invalidate_caches ();
  Log.info "Parsing %d updated modules..." (List.length module_updates);
  StatusUpdate.write
    ~message:"Reparsing updated modules..."
    ~short_message:(Some "[Reparsing]")
    ~connections
    ~message_type:WarningMessage;
  Log.log
    ~section:`Server
    "Incremental Module Update %a"
    Sexp.pp
    [%message (module_updates : ModuleTracker.IncrementalUpdate.t list)];
  let ast_environment_update_result =
    AstEnvironment.update ~configuration ~scheduler ast_environment (Update module_updates)
  in
  let reparsed_sources = AstEnvironment.UpdateResult.reparsed ast_environment_update_result in
  Log.log
    ~section:`Server
    "Incremental Parser Update %s"
    (List.to_string ~f:Reference.show reparsed_sources);
  let legacy_dependency_tracker = Dependencies.create (AstEnvironment.read_only ast_environment) in
  (* Repopulate the environment. *)
  let invalidated_environment_qualifiers =
    match incremental_style with
    | FineGrained
    | Shallow ->
        Reference.Set.of_list reparsed_sources
    | Transitive ->
        Dependencies.transitive_of_list legacy_dependency_tracker ~modules:reparsed_sources
        |> Reference.Set.union (Reference.Set.of_list reparsed_sources)
  in
  Log.info
    "Repopulating the environment for %d modules."
    (Set.length invalidated_environment_qualifiers);
  StatusUpdate.write
    ~message:"Repopulating the environment"
    ~short_message:(Some "[Repopulating]")
    ~connections
    ~message_type:WarningMessage;
  let ast_environment = AstEnvironment.read_only ast_environment in

  let annotated_global_environment_update_result =
    AnnotatedGlobalEnvironment.update_this_and_all_preceding_environments
      ast_environment
      ~configuration
      ~scheduler
      ~ast_environment_update_result
      invalidated_environment_qualifiers
  in
  let environment =
    TypeEnvironment.create
      (AnnotatedGlobalEnvironment.UpdateResult.read_only annotated_global_environment_update_result)
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
            SharedMemoryKeys.DependencyKey.KeySet.elements keyset
            |> List.filter_map ~f:(function
                   | SharedMemoryKeys.TypeCheckDefine name -> Some name
                   | _ -> None)
            |> List.fold ~init:sofar ~f:Reference.Set.add
          in
          AnnotatedGlobalEnvironment.UpdateResult.all_triggered_dependencies
            annotated_global_environment_update_result
          |> List.fold ~init:Reference.Set.empty ~f:filter_union
        in
        let recheck_functions =
          Set.union
            function_triggers
            (UnannotatedGlobalEnvironment.UpdateResult.define_additions
               unannotated_global_environment_update_result)
          |> Set.to_list
        in
        Log.log
          ~section:`Server
          "Rechecked functions %s"
          (List.to_string ~f:Reference.show recheck_functions);

        (* Rerun type checking for triggered functions. *)
        TypeEnvironment.invalidate environment recheck_functions;
        TypeCheck.run_on_defines ~scheduler ~configuration ~environment recheck_functions;

        (* Rerun postprocessing for triggered modules. *)
        let recheck_modules =
          (* For each rechecked function, its containing module needs to be included in
             postprocessing *)
          List.fold
            ~init:invalidated_environment_qualifiers
            (Set.to_list function_triggers)
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
              | Some { UnannotatedGlobalEnvironment.FunctionDefinition.qualifier; _ } ->
                  Set.add sofar qualifier)
          |> Set.to_list
        in
        Log.log
          ~section:`Server
          "Repostprocessed modules %s"
          (List.to_string ~f:Reference.show recheck_modules);

        let errors =
          Analysis.Postprocessing.run
            ~scheduler
            ~configuration
            ~environment:(Analysis.TypeEnvironment.read_only environment)
            recheck_modules
        in

        recheck_modules, errors, List.length recheck_functions
    | _ ->
        let invalidated_environment_qualifiers = Set.to_list invalidated_environment_qualifiers in
        let () =
          Dependencies.purge legacy_dependency_tracker invalidated_environment_qualifiers;
          let re_environment_build_sources =
            List.filter_map
              invalidated_environment_qualifiers
              ~f:(AstEnvironment.ReadOnly.get_source ast_environment)
          in
          Dependencies.register_all_dependencies
            legacy_dependency_tracker
            re_environment_build_sources
        in
        Log.log
          ~section:`Server
          "(Old) Incremental Environment Builder Update %s"
          (List.to_string ~f:Reference.show invalidated_environment_qualifiers);

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
              invalidated_environment_qualifiers
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
            invalidated_environment_qualifiers;
          Analysis.Postprocessing.run
            ~scheduler
            ~configuration
            ~environment:(Analysis.TypeEnvironment.read_only environment)
            invalidated_environment_qualifiers
        in
        invalidated_environment_qualifiers, errors, total_rechecked_functions
  in
  Statistics.event
    ~section:`Memory
    ~name:"shared memory size"
    ~integers:["size", Memory.heap_size ()]
    ();

  (* Clean up all lookup data related to updated files. *)
  List.iter recheck_modules ~f:(LookupCache.evict ~lookups);
  (* Kill all previous errors for new files we just checked *)
  List.iter ~f:(Hashtbl.remove errors) recheck_modules;

  (* Associate the new errors with new files *)
  List.iter new_errors ~f:(fun error ->
      let key = Error.path error in
      Hashtbl.add_multi errors ~key ~data:error);

  Statistics.performance
    ~name:"incremental check"
    ~timer
    ~integers:
      [
        "number of changed files", List.length paths;
        "number of module tracker updates", List.length module_updates;
        "number of parser updates", List.length reparsed_sources;
        "number of environment builder updates", Set.length invalidated_environment_qualifiers;
        "number of rechecked modules", List.length recheck_modules;
        "number of re-checked functions", total_rechecked_functions;
      ]
    ();
  StatusUpdate.write
    ~message:"Done recheck."
    ~short_message:(Some "Done recheck.")
    ~connections
    ~message_type:InfoMessage;
  environment, new_errors


let recheck_with_state
    ~state:
      ( { State.module_tracker; ast_environment; errors; scheduler; connections; lookups; _ } as
      state )
    ~configuration
    paths
  =
  let _, new_errors =
    recheck
      ~module_tracker
      ~ast_environment
      ~errors
      ~scheduler
      ~connections
      ~lookups
      ~configuration
      paths
  in
  state, new_errors
