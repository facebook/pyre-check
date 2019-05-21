(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

module ServerDependencies = Dependencies
open Ast
open Analysis

open State
open Configuration.Analysis

open Pyre

type errors = (File.Handle.t * State.Error.t list) list

let build_file_to_error_map ?(checked_files = None) ~state:{ State.errors; _ } error_list =
  let initial_files = Option.value ~default:(Hashtbl.keys errors) checked_files in
  let error_file error = File.Handle.create (Error.path error) in
  List.fold
    ~init:File.Handle.Map.empty
    ~f:(fun map key -> Map.set map ~key ~data:[])
    initial_files
  |> (fun map ->
      List.fold
        ~init:map
        ~f:(fun map error -> Map.add_multi map ~key:(error_file error) ~data:error)
        error_list)
  |> Map.to_alist


let recheck
    ~state:({
        State.environment;
        errors;
        scheduler;
        deferred_state;
        _ } as state)
    ~configuration:({ debug; _ } as configuration)
    ~files
    ~should_analyze_dependencies =

  Annotated.Class.AttributeCache.clear ();
  Module.Cache.clear ();
  Resolution.Cache.clear ();
  let removed_handles, update_environment_with, check =
    let update_handle_state (updated, removed) file =
      match File.handle ~configuration file with
      | exception (File.NonexistentHandle _) ->
          Log.warning "`%s` not found in search path." (Path.absolute (File.path file));
          updated, removed
      | handle when (not (Path.file_exists (File.path file))) ->
          updated, handle :: removed
      | handle ->
          begin
            match Ast.SharedMemory.Modules.get ~qualifier:(Source.qualifier ~handle) with
            | Some existing ->
                let existing_handle =
                  Module.handle existing
                  |> Option.value ~default:handle
                in
                if File.Handle.equal existing_handle handle then
                  (file :: updated), removed
                else if
                  (File.Handle.is_stub handle) &&
                  (not (File.Handle.is_stub existing_handle)) then
                  (* Stubs take priority over existing handles. *)
                  file :: updated, existing_handle :: removed
                else
                  updated, removed
            | _  ->
                file :: updated, removed
          end
    in
    let update_environment_with, removed_handles =
      List.fold files ~f:update_handle_state ~init:([], [])
    in
    let check = List.filter update_environment_with ~f:(fun file -> not (File.is_stub file)) in
    removed_handles,
    update_environment_with,
    check
  in
  if not (List.is_empty removed_handles) then
    List.map removed_handles ~f:File.Handle.show
    |> String.concat ~sep:", "
    |> Log.info "Removing type information for `%s`";

  let (module Handler: Environment.Handler) = environment in
  let scheduler = Scheduler.with_parallel scheduler ~is_parallel:(List.length check > 5) in

  (* Compute requests we do not serve immediately. *)
  let deferred_state =
    if should_analyze_dependencies then
      ServerDependencies.compute_dependencies
        update_environment_with
        ~state
        ~configuration
      |> fun files -> Deferred.add deferred_state ~files
    else
      deferred_state
  in

  (* Repopulate the environment. *)
  Log.info "Repopulating the environment.";
  let repopulate_handles =
    (* Clean up all data related to updated files. *)
    let timer = Timer.start () in
    let handle file =
      try
        Some (File.handle ~configuration file)
      with File.NonexistentHandle _ ->
        None
    in
    (* Watchman only notifies Pyre that a file has been updated, we have to detect
       removals manually and update our handle set. *)
    Ast.SharedMemory.HandleKeys.remove ~handles:removed_handles;
    let targets =
      let find_target file = Path.readlink (File.path file) in
      List.filter_map update_environment_with ~f:find_target
    in
    Ast.SharedMemory.SymlinksToPaths.remove ~targets;
    let handles = List.filter_map update_environment_with ~f:handle in
    Ast.SharedMemory.Sources.remove ~handles:(handles @ removed_handles);
    Handler.purge ~debug (handles @ removed_handles);
    List.iter update_environment_with ~f:(LookupCache.evict ~state ~configuration);
    Statistics.performance
      ~name:"purged old environment"
      ~timer
      ~integers:[
        "number of files", (List.length (handles @ removed_handles))
      ]
      ();

    let stubs, sources =
      let is_stub file =
        file
        |> File.path
        |> Path.absolute
        |> String.is_suffix ~suffix:".pyi"
      in
      List.partition_tf ~f:is_stub update_environment_with
    in
    Log.info "Parsing %d updated stubs..." (List.length stubs);
    let {
      Service.Parser.parsed = stubs;
      syntax_error = stub_syntax_errors;
      system_error = stub_system_errors;
    } =
      Service.Parser.parse_sources
        ~configuration
        ~scheduler
        ~preprocessing_state:None
        ~files:stubs
    in
    let sources =
      let stub_qualifiers =
        List.map stubs ~f:(fun handle -> Source.qualifier ~handle)
        |> Reference.Hash_set.of_list
      in
      let keep file =
        match handle file with
        | None ->
            false
        | Some handle ->
            let qualifier = Source.qualifier ~handle in
            if Hash_set.mem stub_qualifiers qualifier then
              false
            else
              Handler.module_definition qualifier
              >>= Module.handle
              >>| (fun existing_handle -> File.Handle.equal handle existing_handle)
              |> Option.value ~default:true
      in
      List.filter sources ~f:keep
    in
    Log.info "Parsing %d updated sources..." (List.length sources);
    let {
      Service.Parser.parsed = sources;
      syntax_error = source_syntax_errors;
      system_error = source_system_errors;
    } =
      Service.Parser.parse_sources
        ~configuration
        ~scheduler
        ~preprocessing_state:None
        ~files:sources
    in
    let unparsed =
      List.concat [
        stub_syntax_errors;
        stub_system_errors;
        source_syntax_errors;
        source_system_errors;
      ]
    in
    if not (List.is_empty unparsed) then
      Log.warning
        "Unable to parse `%s`."
        (List.map unparsed ~f:File.Handle.show
         |> String.concat ~sep:", ");
    stubs @ sources
  in
  Log.log
    ~section:`Debug
    "Repopulating the environment with %a"
    Sexp.pp [%message (repopulate_handles: File.Handle.t list)];
  Log.info "Updating the type environment for %d files." (List.length repopulate_handles);
  List.filter_map ~f:Ast.SharedMemory.Sources.get repopulate_handles
  |> Service.Environment.populate ~configuration ~scheduler environment;
  Statistics.event
    ~section:`Memory
    ~name:"shared memory size"
    ~integers:["size", Service.EnvironmentSharedMemory.heap_size ()]
    ();
  Service.Postprocess.register_ignores ~configuration scheduler repopulate_handles;

  (* Compute new set of errors. *)
 (* Clear all type resolution info from shared memory for all affected sources. *)
  ResolutionSharedMemory.remove repopulate_handles;
  Coverage.SharedMemory.remove_batch (Coverage.SharedMemory.KeySet.of_list repopulate_handles);

  let new_errors =
    Service.Check.analyze_sources
      ~scheduler
      ~configuration
      ~environment
      ~handles:repopulate_handles
  in
  (* Kill all previous errors for new files we just checked *)
  List.iter ~f:(Hashtbl.remove errors) (removed_handles @ repopulate_handles);
  (* Associate the new errors with new files *)
  List.iter
    new_errors
    ~f:(fun error ->
        Hashtbl.add_multi errors ~key:(File.Handle.create (Error.path error)) ~data:error);
  let checked_files =
    List.filter_map
      ~f:(fun file -> try
             Some (File.handle ~configuration file)
           with File.NonexistentHandle _ ->
             Log.warning
               "Could not create a handle for %s. It will be excluded from the type-check response."
               (Path.absolute (File.path file));
             None
         )
      check
    |> Option.some
  in
  { state with deferred_state },
  build_file_to_error_map ~checked_files ~state new_errors
