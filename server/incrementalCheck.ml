(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
module ServerDependencies = Dependencies
module ModuleTracker = Service.ModuleTracker
open Ast
open Analysis
open State
open Configuration.Analysis
open Pyre

type errors = State.Error.t list [@@deriving show]

let recheck_deprecated
    ~state:({ State.environment; errors; scheduler; open_documents; _ } as state)
    ~configuration:({ debug; ignore_dependencies; _ } as configuration)
    ~files
  =
  let timer = Timer.start () in
  Annotated.Class.AttributeCache.clear ();
  Module.Cache.clear ();
  Resolution.Cache.clear ();
  let recheck, removed_handles =
    let update_handle_state (updated, removed) file =
      match File.handle ~configuration file with
      | exception File.NonexistentHandle _ ->
          Log.warning "`%s` not found in search path." (Path.absolute (File.path file));
          updated, removed
      | handle when not (Path.file_exists (File.path file)) -> updated, handle :: removed
      | handle -> (
        match Service.EnvironmentSharedMemory.Modules.get (Source.qualifier ~handle) with
        | Some existing ->
            let existing_handle = Module.handle existing |> Option.value ~default:handle in
            if File.Handle.equal existing_handle handle then
              file :: updated, removed
            else if File.Handle.is_stub handle && not (File.Handle.is_stub existing_handle) then
              (* Stubs take priority over existing handles. *)
              file :: updated, existing_handle :: removed
            else
              updated, removed
        | _ -> file :: updated, removed )
    in
    List.fold files ~f:update_handle_state ~init:([], [])
  in
  if not (List.is_empty removed_handles) then
    List.map removed_handles ~f:File.Handle.show
    |> String.concat ~sep:", "
    |> Log.info "Removing type information for `%s`";
  let (module Handler : Environment.Handler) = environment in
  let scheduler = Scheduler.with_parallel scheduler ~is_parallel:(List.length recheck > 5) in
  (* Also recheck dependencies of the changed files. *)
  let recheck =
    if ignore_dependencies then
      recheck
    else
      let recheck_dependencies =
        let to_file qualifier =
          Ast.SharedMemory.Sources.get qualifier
          >>= (fun { Ast.Source.handle; _ } -> File.Handle.to_path ~configuration handle)
          >>| File.create
        in
        ServerDependencies.compute_dependencies recheck ~state ~configuration
        |> File.Set.filter_map ~f:to_file
      in
      Set.union (File.Set.of_list recheck) recheck_dependencies |> Set.to_list
  in
  (* Repopulate the environment. *)
  Log.info "Repopulating the environment.";
  StatusUpdate.warning
    ~message:"Repopulating the environment"
    ~short_message:(Some "[Repopulating]")
    ~state;
  let repopulate_handles =
    (* Clean up all data related to updated files. *)
    let timer = Timer.start () in
    let handles, recheck =
      let keep_file_with_handle file =
        try Some (File.handle ~configuration file, file) with
        | File.NonexistentHandle _ as nonexistent_handle ->
            Statistics.log_exception
              nonexistent_handle
              ~fatal:false
              ~origin:"Rechecking dependencies";
            None
      in
      List.filter_map recheck ~f:keep_file_with_handle |> List.unzip
    in
    (* Watchman only notifies Pyre that a file has been updated, we have to detect removals
       manually and update our handle set. *)
    Ast.SharedMemory.HandleKeys.remove ~handles:removed_handles;
    let () =
      let qualifiers =
        List.map (handles @ removed_handles) ~f:(fun handle -> Source.qualifier ~handle)
      in
      Ast.SharedMemory.Sources.remove qualifiers;
      Handler.purge ~debug qualifiers
    in
    List.iter handles ~f:(fun handle -> Source.qualifier ~handle |> LookupCache.evict ~state);
    Statistics.performance
      ~name:"purged old environment"
      ~timer
      ~integers:["number of files", List.length (handles @ removed_handles)]
      ();
    let stubs, sources =
      let is_stub file = file |> File.path |> Path.absolute |> String.is_suffix ~suffix:".pyi" in
      List.partition_tf ~f:is_stub recheck
    in
    Log.info "Parsing %d updated stubs..." (List.length stubs);
    StatusUpdate.warning
      ~message:(Format.asprintf "Parsing %d updated stubs..." (List.length stubs))
      ~short_message:(Some "[Parsing stubs]")
      ~state;
    let { Service.Parser.parsed = stubs;
          syntax_error = stub_syntax_errors;
          system_error = stub_system_errors
        }
      =
      Service.Parser.parse_sources ~configuration ~scheduler ~preprocessing_state:None ~files:stubs
    in
    let sources =
      let stub_qualifiers =
        List.map stubs ~f:(fun handle -> Source.qualifier ~handle) |> Reference.Hash_set.of_list
      in
      let keep file =
        match File.handle ~configuration file with
        | exception File.NonexistentHandle _ -> false
        | handle ->
            let qualifier = Source.qualifier ~handle in
            if Hash_set.mem stub_qualifiers qualifier then
              false
            else
              Service.EnvironmentSharedMemory.Modules.get qualifier
              >>= Module.handle
              >>| (fun existing_handle -> File.Handle.equal handle existing_handle)
              |> Option.value ~default:true
      in
      List.filter sources ~f:keep
    in
    Log.info "Parsing %d updated sources..." (List.length sources);
    StatusUpdate.warning
      ~message:(Format.asprintf "Parsing %d updated sources..." (List.length sources))
      ~short_message:(Some "[Parsing sources]")
      ~state;
    let { Service.Parser.parsed = sources;
          syntax_error = source_syntax_errors;
          system_error = source_system_errors
        }
      =
      Service.Parser.parse_sources
        ~configuration
        ~scheduler
        ~preprocessing_state:None
        ~files:sources
    in
    let unparsed =
      List.concat
        [stub_syntax_errors; stub_system_errors; source_syntax_errors; source_system_errors]
    in
    if not (List.is_empty unparsed) then
      Log.warning
        "Unable to parse `%s`."
        (List.map unparsed ~f:File.Handle.show |> String.concat ~sep:", ");
    stubs @ sources
  in
  Log.log
    ~section:`Debug
    "Repopulating the environment with %a"
    Sexp.pp
    [%message (repopulate_handles : File.Handle.t list)];
  Log.info "Updating the type environment for %d files." (List.length repopulate_handles);
  let repopulated_qualifiers =
    List.map repopulate_handles ~f:(fun handle -> Source.qualifier ~handle)
  in
  List.filter_map ~f:Ast.SharedMemory.Sources.get repopulated_qualifiers
  |> Service.Environment.populate ~configuration ~scheduler environment;
  Statistics.event
    ~section:`Memory
    ~name:"shared memory size"
    ~integers:["size", Ast.SharedMemory.heap_size ()]
    ();
  Service.Postprocess.register_ignores ~configuration scheduler repopulate_handles;

  (* Compute new set of errors. *)
  (* Clear all type resolution info from shared memory for all affected sources. *)
  ResolutionSharedMemory.remove repopulated_qualifiers;
  Coverage.SharedMemory.remove_batch (Coverage.SharedMemory.KeySet.of_list repopulated_qualifiers);
  let new_errors =
    Service.Check.analyze_sources
      ~open_documents
      ~scheduler
      ~configuration
      ~environment
      ~handles:repopulate_handles
      ()
  in
  (* Kill all previous errors for new files we just checked *)
  List.iter
    ~f:(fun handle -> Hashtbl.remove errors (Source.qualifier ~handle))
    (removed_handles @ repopulate_handles);

  (* Associate the new errors with new files *)
  List.iter new_errors ~f:(fun error ->
      let key = Error.path error |> Ast.SourcePath.qualifier_of_relative in
      Hashtbl.add_multi errors ~key ~data:error);

  Statistics.performance
    ~name:"incremental check"
    ~timer
    ~integers:["number of direct files", List.length files; "number of files", List.length recheck]
    ();
  state, new_errors


let recheck
    ~state:({ State.module_tracker; environment; errors; scheduler; open_documents; _ } as state)
    ~configuration:({ debug; ignore_dependencies; _ } as configuration)
    ~files
  =
  let timer = Timer.start () in
  Annotated.Class.AttributeCache.clear ();
  Module.Cache.clear ();
  Resolution.Cache.clear ();
  let module_updates =
    let paths = List.map files ~f:File.path in
    ModuleTracker.update module_tracker ~configuration ~paths
  in
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
  let recheck_files =
    List.map recheck_source_paths ~f:(fun { SourcePath.relative_path; _ } ->
        File.create (Path.Relative relative_path))
  in
  if not (List.is_empty removed) then
    List.map removed ~f:Reference.show
    |> String.concat ~sep:", "
    |> Log.info "Removing type information for `%s`";
  let (module Handler : Environment.Handler) = environment in
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
        (ServerDependencies.compute_dependencies recheck_files ~state ~configuration)
      |> Set.to_list
  in
  (* Repopulate the environment. *)
  Log.info "Repopulating the environment.";
  StatusUpdate.warning
    ~message:"Repopulating the environment"
    ~short_message:(Some "[Repopulating]")
    ~state;
  let repopulate_handles =
    let timer = Timer.start () in
    (* Clean up all data related to updated files. *)
    let qualifiers = List.append removed recheck_modules in
    Ast.SharedMemory.Sources.remove qualifiers;
    Handler.purge ~debug qualifiers;
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
        ~files:recheck_files
    in
    let unparsed = List.concat [syntax_error; system_error] in
    if not (List.is_empty unparsed) then
      Log.warning
        "Unable to parse `%s`."
        (List.map unparsed ~f:File.Handle.show |> String.concat ~sep:", ");
    parsed
  in
  Log.log
    ~section:`Debug
    "Repopulating the environment with %a"
    Sexp.pp
    [%message (repopulate_handles : File.Handle.t list)];
  Log.info "Updating the type environment for %d files." (List.length repopulate_handles);
  let repopulated = List.map repopulate_handles ~f:(fun handle -> Source.qualifier ~handle) in
  List.filter_map ~f:Ast.SharedMemory.Sources.get repopulated
  |> Service.Environment.populate ~configuration ~scheduler environment;
  Statistics.event
    ~section:`Memory
    ~name:"shared memory size"
    ~integers:["size", Ast.SharedMemory.heap_size ()]
    ();
  Service.Postprocess.register_ignores ~configuration scheduler repopulate_handles;

  (* Compute new set of errors. *)
  (* Clear all type resolution info from shared memory for all affected sources. *)
  ResolutionSharedMemory.remove repopulated;
  Coverage.SharedMemory.remove_batch (Coverage.SharedMemory.KeySet.of_list repopulated);
  let new_errors =
    Service.Check.analyze_sources
      ~open_documents
      ~scheduler
      ~configuration
      ~environment
      ~handles:repopulate_handles
      ()
  in
  (* Kill all previous errors for new files we just checked *)
  List.iter ~f:(Hashtbl.remove errors) (removed @ repopulated);

  (* Associate the new errors with new files *)
  List.iter new_errors ~f:(fun error ->
      let key = Error.path error |> Ast.SourcePath.qualifier_of_relative in
      Hashtbl.add_multi errors ~key ~data:error);

  Statistics.performance
    ~name:"incremental check"
    ~timer
    ~integers:["number of direct files", List.length files; "number of files", List.length recheck]
    ();
  state, new_errors
