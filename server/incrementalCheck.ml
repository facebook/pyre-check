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
    ~configuration:({ debug; ignore_dependencies; _ } as configuration)
    paths
  =
  let timer = Timer.start () in
  Annotated.Class.AttributeCache.clear ();
  Module.Cache.clear ();
  let module_updates = ModuleTracker.update module_tracker ~configuration ~paths in
  let recheck_source_paths, removed =
    let categorize = function
      | ModuleTracker.IncrementalUpdate.New source_path -> `Fst source_path
      | ModuleTracker.IncrementalUpdate.Delete qualifier -> `Snd qualifier
    in
    List.partition_map module_updates ~f:categorize
  in
  let recheck_modules =
    List.map recheck_source_paths ~f:(fun { SourcePath.qualifier; _ } -> qualifier)
  in
  if not (List.is_empty removed) then
    List.map removed ~f:Reference.show
    |> String.concat ~sep:", "
    |> Log.info "Removing type information for `%s`";
  let scheduler =
    Scheduler.with_parallel scheduler ~is_parallel:(List.length recheck_source_paths > 5)
  in
  (* Also recheck dependencies of the changed files. *)
  let recheck =
    if ignore_dependencies then
      recheck_modules
    else
      Set.union
        (Reference.Set.of_list recheck_modules)
        (ServerDependencies.compute_dependencies recheck_source_paths ~state ~configuration)
      |> Set.to_list
  in
  (* Repopulate the environment. *)
  Log.info "Repopulating the environment.";
  StatusUpdate.warning
    ~message:"Repopulating the environment"
    ~short_message:(Some "[Repopulating]")
    ~state;
  let () =
    let timer = Timer.start () in
    (* Clean up all data related to updated files. *)
    let qualifiers = List.append removed recheck_modules in
    Ast.SharedMemory.Sources.remove qualifiers;
    Analysis.Environment.purge environment ~debug qualifiers;
    List.iter qualifiers ~f:(LookupCache.evict ~state);
    Statistics.performance
      ~name:"purged old environment"
      ~timer
      ~integers:["number of files", List.length qualifiers]
      ();
    Log.info "Parsing %d updated sources..." (List.length recheck);
    StatusUpdate.warning
      ~message:(Format.asprintf "Parsing %d updated sources..." (List.length recheck))
      ~short_message:(Some "[Parsing sources]")
      ~state;
    let { Service.Parser.parsed; syntax_error; system_error } =
      Service.Parser.parse_sources
        ~configuration
        ~scheduler
        ~preprocessing_state:None
        ~ast_environment
        recheck_source_paths
    in
    let unparsed = List.concat [syntax_error; system_error] in
    if not (List.is_empty unparsed) then
      Log.warning
        "Unable to parse `%s`."
        ( List.map unparsed ~f:(fun { SourcePath.relative; _ } -> relative)
        |> String.concat ~sep:", " );
    let parsed_paths = List.map parsed ~f:(fun { SourcePath.relative; _ } -> relative) in
    Log.log
      ~section:`Debug
      "Repopulating the environment with %a"
      Sexp.pp
      [%message (parsed_paths : string list)];
    Log.info "Updating the type environment for %d files." (List.length parsed)
  in
  List.filter_map ~f:Ast.SharedMemory.Sources.get recheck_modules
  |> Service.Environment.populate ~configuration ~scheduler environment;
  Statistics.event
    ~section:`Memory
    ~name:"shared memory size"
    ~integers:["size", Ast.SharedMemory.heap_size ()]
    ();
  Service.Postprocess.register_ignores ~configuration scheduler recheck_source_paths;

  (* Compute new set of errors. *)
  (* Clear all type resolution info from shared memory for all affected sources. *)
  ResolutionSharedMemory.remove recheck_modules;
  Coverage.SharedMemory.remove_batch (Coverage.SharedMemory.KeySet.of_list recheck_modules);
  let new_errors =
    Service.Check.analyze_sources
      ~open_documents
      ~scheduler
      ~configuration
      ~environment
      recheck_source_paths
  in
  (* Kill all previous errors for new files we just checked *)
  List.iter ~f:(Hashtbl.remove errors) (removed @ recheck_modules);

  (* Associate the new errors with new files *)
  List.iter new_errors ~f:(fun error ->
      let key = Error.path error |> Ast.SourcePath.qualifier_of_relative in
      Hashtbl.add_multi errors ~key ~data:error);

  Statistics.performance
    ~name:"incremental check"
    ~timer
    ~integers:["number of direct files", List.length paths; "number of files", List.length recheck]
    ();
  state, new_errors
