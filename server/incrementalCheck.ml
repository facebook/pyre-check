(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
module ServerDependencies = Dependencies
module ModuleTracker = Analysis.ModuleTracker
open Ast
open Analysis
open State
open Configuration.Analysis
open Pyre

type errors = State.Error.t list [@@deriving show]

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
  if not (List.is_empty removed_modules) then
    List.map removed_modules ~f:Reference.show
    |> String.concat ~sep:", "
    |> Log.info "Removing type information for `%s`";
  let scheduler =
    Scheduler.with_parallel scheduler ~is_parallel:(List.length directly_changed_source_paths > 5)
  in
  (* Repopulate the environment. *)
  Log.info "Repopulating the environment.";
  StatusUpdate.warning
    ~message:"Repopulating the environment"
    ~short_message:(Some "[Repopulating]")
    ~state;
  let re_environment_build_sources, invalidated_environment_qualifiers =
    (* Also recheck dependencies of the changed files. *)
    let re_environment_build_modules =
      if ignore_dependencies then
        directly_changed_modules
      else
        Set.union
          (Reference.Set.of_list directly_changed_modules)
          (ServerDependencies.compute_dependencies
             directly_changed_source_paths
             ~state
             ~configuration
             ~removed_modules)
        |> Set.to_list
    in
    let re_environment_build_source_paths =
      List.filter_map re_environment_build_modules ~f:(ModuleTracker.lookup module_tracker)
    in
    (* Clean up all data related to updated files. *)
    let qualifiers = List.append removed_modules re_environment_build_modules in
    AstEnvironment.remove_sources ast_environment qualifiers;
    List.iter qualifiers ~f:(LookupCache.evict ~state);
    Log.info "Parsing %d updated sources..." (List.length re_environment_build_source_paths);
    StatusUpdate.warning
      ~message:
        (Format.asprintf
           "Parsing %d updated sources..."
           (List.length re_environment_build_source_paths))
      ~short_message:(Some "[Parsing sources]")
      ~state;
    let { Service.Parser.parsed; syntax_error; system_error } =
      Service.Parser.parse_sources
        ~configuration
        ~scheduler
        ~preprocessing_state:None
        ~ast_environment
        re_environment_build_source_paths
    in
    let unparsed = List.concat [syntax_error; system_error] in
    if not (List.is_empty unparsed) then
      Log.warning
        "Unable to parse `%s`."
        ( List.map unparsed ~f:(fun { SourcePath.relative; _ } -> relative)
        |> String.concat ~sep:", " );
    let parsed_sources = List.filter_map parsed ~f:(AstEnvironment.get_source ast_environment) in
    let parsed_paths = List.map parsed_sources ~f:(fun { Source.relative; _ } -> relative) in
    Log.log
      ~section:`Debug
      "Repopulating the environment with %a"
      Sexp.pp
      [%message (parsed_paths : string list)];
    Log.info "Updating the type environment for %d files." (List.length parsed_sources);
    parsed_sources, qualifiers
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
            directly_changed_modules
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
  let new_errors =
    Service.Check.analyze_sources
      ~open_documents:(Reference.Table.mem open_documents)
      ~scheduler
      ~configuration
      ~environment
      recheck_sources
  in
  (* Kill all previous errors for new files we just checked *)
  List.iter ~f:(Hashtbl.remove errors) (removed_modules @ recheck_modules);

  (* Associate the new errors with new files *)
  List.iter new_errors ~f:(fun error ->
      let key = Error.path error in
      Hashtbl.add_multi errors ~key ~data:error);

  Statistics.performance
    ~name:"incremental check"
    ~timer
    ~integers:
      [ "number of direct files", List.length paths;
        "number of files", List.length recheck_sources;
        "number of functions", List.sum (module Int) ~f:Preprocessing.count_defines recheck_sources
      ]
    ();
  StatusUpdate.information ~message:"Done recheck." ~short_message:(Some "Done recheck.") ~state;
  state, new_errors
