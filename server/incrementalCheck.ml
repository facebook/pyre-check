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

let direct_parser_update ~configuration ~scheduler ~ast_environment module_updates =
  let directly_changed_source_paths, removed_modules =
    let categorize = function
      | ModuleTracker.IncrementalUpdate.New source_path -> `Fst source_path
      | ModuleTracker.IncrementalUpdate.Delete qualifier -> `Snd qualifier
    in
    List.partition_map module_updates ~f:categorize
  in
  let directly_changed_modules =
    List.map directly_changed_source_paths ~f:(fun { SourcePath.qualifier; _ } -> qualifier)
  in
  AstEnvironment.remove_sources
    ast_environment
    (List.append removed_modules directly_changed_modules);
  let { Service.Parser.parsed; _ } =
    Service.Parser.parse_sources
      ~configuration
      ~scheduler
      ~preprocessing_state:None
      ~ast_environment
      directly_changed_source_paths
  in
  parsed


let recheck
    ~state:( {
               State.module_tracker;
               ast_environment;
               environment;
               errors;
               scheduler;
               open_documents;
               _;
             } as state )
    ~configuration:({ debug; ignore_dependencies; incremental_style; _ } as configuration)
    paths
  =
  let timer = Timer.start () in
  Annotated.Class.AttributeCache.clear ();
  Module.Cache.clear ();
  let module_updates = ModuleTracker.update module_tracker ~configuration ~paths in
  let scheduler =
    Scheduler.with_parallel scheduler ~is_parallel:(List.length module_updates > 10)
  in
  Log.info "Parsing %d updated modules..." (List.length module_updates);
  StatusUpdate.warning
    ~message:"Reparsing updated modules..."
    ~short_message:(Some "[Reparsing]")
    ~state;
  Log.log
    ~section:`Server
    "Incremental Module Update %a"
    Sexp.pp
    [%message (module_updates : ModuleTracker.IncrementalUpdate.t list)];
  let invalidated_environment_qualifiers =
    if ignore_dependencies then
      direct_parser_update ~configuration ~scheduler ~ast_environment module_updates
    else
      Service.Parser.update ~configuration ~scheduler ~ast_environment module_updates
  in
  Log.log
    ~section:`Server
    "Incremental Parser Update %s"
    (List.to_string ~f:Reference.show invalidated_environment_qualifiers);

  (* Repopulate the environment. *)
  let invalidated_environment_qualifiers =
    match incremental_style with
    | FineGrained -> invalidated_environment_qualifiers
    | Shallow ->
        Dependencies.of_list
          ~modules:invalidated_environment_qualifiers
          ~get_dependencies:(Environment.dependencies environment)
        |> Reference.Set.union (Reference.Set.of_list invalidated_environment_qualifiers)
        |> Reference.Set.to_list
    | Transitive ->
        Dependencies.transitive_of_list
          ~modules:invalidated_environment_qualifiers
          ~get_dependencies:(Environment.dependencies environment)
        |> Reference.Set.union (Reference.Set.of_list invalidated_environment_qualifiers)
        |> Reference.Set.to_list
  in
  Log.info
    "Repopulating the environment for %d modules."
    (List.length invalidated_environment_qualifiers);
  StatusUpdate.warning
    ~message:"Repopulating the environment"
    ~short_message:(Some "[Repopulating]")
    ~state;
  let re_environment_build_sources =
    List.filter_map
      invalidated_environment_qualifiers
      ~f:(AstEnvironment.get_source ast_environment)
  in
  let recheck_modules, recheck_sources =
    match incremental_style with
    | FineGrained ->
        let (), invalidated_type_checking_keys =
          let update () =
            Service.Environment.populate
              ~configuration
              ~scheduler
              environment
              re_environment_build_sources;
            if debug then
              Analysis.Environment.check_class_hierarchy_integrity ()
          in
          Analysis.Environment.update_and_compute_dependencies
            environment
            invalidated_environment_qualifiers
            ~update
        in
        let invalidated_type_checking_keys =
          List.fold
            invalidated_environment_qualifiers
            ~init:invalidated_type_checking_keys
            ~f:(fun sofar key -> SharedMemoryKeys.ReferenceDependencyKey.KeySet.add key sofar)
        in
        let invalidated_type_checking_keys =
          SharedMemoryKeys.ReferenceDependencyKey.KeySet.elements invalidated_type_checking_keys
        in
        let recheck_modules = invalidated_type_checking_keys in
        let recheck_sources =
          List.filter_map recheck_modules ~f:(AstEnvironment.get_source ast_environment)
        in
        Log.log
          ~section:`Server
          "Incremental Environment Builder Update %s"
          (List.to_string ~f:Reference.show invalidated_type_checking_keys);
        recheck_modules, recheck_sources
    | _ ->
        let () =
          Analysis.Environment.purge environment ~debug invalidated_environment_qualifiers;
          Service.Environment.populate
            ~configuration
            ~scheduler
            environment
            re_environment_build_sources
        in
        invalidated_environment_qualifiers, re_environment_build_sources
  in
  Statistics.event
    ~section:`Memory
    ~name:"shared memory size"
    ~integers:["size", Memory.heap_size ()]
    ();

  (* Compute new set of errors. *)
  (* Clear all type resolution info from shared memory for all affected sources. *)
  ResolutionSharedMemory.remove recheck_modules;
  Coverage.SharedMemory.remove_batch (Coverage.SharedMemory.KeySet.of_list recheck_modules);

  (* Clean up all lookup data related to updated files. *)
  List.iter recheck_modules ~f:(LookupCache.evict ~state);
  let new_errors =
    Service.Check.analyze_sources
      ~open_documents:(Reference.Table.mem open_documents)
      ~scheduler
      ~configuration
      ~environment
      recheck_sources
  in
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
      [ "number of changed files", List.length paths;
        "number of module tracker updates", List.length module_updates;
        "number of parser updates", List.length invalidated_environment_qualifiers;
        "number of environment builder updates", List.length recheck_sources;
        ( "number of re-checked functions",
          List.sum (module Int) ~f:Preprocessing.count_defines recheck_sources ) ]
    ();
  StatusUpdate.information ~message:"Done recheck." ~short_message:(Some "Done recheck.") ~state;
  state, new_errors
