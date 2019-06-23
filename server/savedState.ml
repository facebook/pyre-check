(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Service

exception IncompatibleState of string

(* Return symlinks for files for a pyre project that are links to a separate project root. *)
let restore_symbolic_links ~changed_paths ~local_root ~get_old_link_path =
  let new_paths, removed_paths = List.partition_tf ~f:Path.file_exists changed_paths in
  (* We need to get the deleted paths from shared memory, as the version of the server launched
     from the saved state will have references to this files, whereas they won't be present in the
     new project root, meaning that we need to clean up the environment from them. *)
  let removed_paths = List.filter_map removed_paths ~f:get_old_link_path in
  (* Any member of new_paths might not have existed when the saved state was being created. *)
  let new_paths =
    let local_root_links =
      let file_filter file =
        Filename.check_suffix file ".py" || Filename.check_suffix file ".pyi"
      in
      Path.list ~file_filter ~root:local_root () |> fun links -> Path.build_symlink_map ~links
    in
    List.filter_map new_paths ~f:(Map.find local_root_links)
  in
  new_paths @ removed_paths


(* If we're analyzing generated code, Watchman will be blind to any changes to said code. In order
   to be safe, compute hashes for all files that a fresh Pyre run would analyze. *)
let compute_locally_changed_files
    ~scheduler
    ~configuration:({ Configuration.Analysis.local_root; _ } as configuration)
  =
  Log.info "Computing files that changed since the saved state was created.";
  let timer = Timer.start () in
  let stubs_and_sources = Service.Parser.find_stubs_and_sources configuration in
  let changed_files changed new_paths =
    let changed_file path =
      try
        let file = File.create path in
        let handle = File.handle ~configuration file in
        let old_hash = Ast.SharedMemory.Sources.get handle >>| Ast.Source.hash in
        let current_hash = File.hash file in
        if Option.equal Int.equal old_hash current_hash then
          None
        else
          Some file
      with
      | File.NonexistentHandle _ -> None
    in
    changed @ List.filter_map new_paths ~f:changed_file
  in
  let changed_paths =
    Scheduler.map_reduce
      scheduler
      ~configuration
      ~initial:[]
      ~map:changed_files
      ~reduce:( @ )
      ~inputs:stubs_and_sources
      ()
  in
  let removed_paths =
    let new_handles =
      let handle path =
        try File.create path |> File.handle ~configuration |> Option.some with
        | File.NonexistentHandle _ -> None
      in
      List.filter_map stubs_and_sources ~f:handle |> File.Handle.Set.Tree.of_list
    in
    let old_handles = Ast.SharedMemory.HandleKeys.get () in
    (* If a handle was present in the saved state creation (i.e. in old_handles) but is missing
       from new_handles, it was either shadowed by a stub file or removed. In either case, it does
       not exist in the Pyre server's eyes. *)
    let removed_handles =
      File.Handle.Set.Tree.diff old_handles new_handles |> File.Handle.Set.Tree.to_list
    in
    (* This is a bit of a hack: In general, it doesn't make sense to give a path to a removed
       handle, as it might've originated anywhere in the search path. However, the server's
       equipped to handle paths which no longer exist, so by assuming that the removed path lived
       in the local root, we're able to express the idea that the handle needs to be removed and
       its dependencies reanalyzed. *)
    let to_file handle =
      Path.create_relative ~root:local_root ~relative:(File.Handle.show handle) |> File.create
    in
    List.map removed_handles ~f:to_file
  in
  Statistics.performance ~name:"computed files to reanalyze" ~timer ();
  changed_paths @ removed_paths


let load
    ~server_configuration:{ Configuration.Server.configuration =
                              { Configuration.Analysis.expected_version;
                                project_root;
                                local_root;
                                configuration_file_hash;
                                _
                              } as configuration;
                            saved_state_action;
                            _
                          }
    ~connections
  =
  Log.info "Initializing server from saved state...";
  let shared_memory_path, changed_paths =
    match saved_state_action with
    | Some (Load (LoadFromFiles parameters)) ->
        let { Configuration.Server.shared_memory_path; changed_files_path } = parameters in
        let files =
          let to_path serialized = Path.create_absolute ~follow_symbolic_links:false serialized in
          changed_files_path
          >>| File.create
          >>= File.content
          >>| String.split_lines
          >>| List.map ~f:to_path
        in
        shared_memory_path, files
    | Some (Load (LoadFromProject { project_name; metadata })) -> (
        if Option.is_none expected_version then (
          Log.warning "An expected version must be passed in in order to load from saved states.";
          raise (IncompatibleState "version mismatch") );
        Log.log ~section:`Server "Loading from saved state project `%s`..." project_name;
        let target_path = Constants.Server.saved_state_path configuration in
        Log.log ~section:`Server "Loading saved state to `%s`..." (Path.absolute target_path);
        let loaded_state =
          Path.search_upwards ~target:".watchmanconfig" ~root:project_root
          >>= fun watchman_root ->
          FetchSavedState.load
            ~watchman_root
            ~project_name
            ~project_metadata:metadata
            ~configuration_file_hash
            ~version:(Option.value_exn expected_version)
            ~target_path
        in
        match loaded_state with
        | Some { FetchSavedState.saved_state_path; changed_files } ->
            (* Heuristic: If saved states with metadata are enabled, we need to deal with
               non-checked in generated code, so ignore watchman's response. *)
            saved_state_path, changed_files
        | None -> raise (IncompatibleState "unable to fetch state") )
    | _ -> raise (IncompatibleState "unexpected saved state parameters")
  in
  let scheduler = Scheduler.create ~configuration () in
  let environment = (module Environment.SharedHandler : Analysis.Environment.Handler) in
  Memory.load_shared_memory ~path:(Path.absolute shared_memory_path);
  let old_configuration =
    EnvironmentSharedMemory.StoredConfiguration.find_unsafe "configuration"
  in
  if not (Configuration.Analysis.equal old_configuration configuration) then
    raise (IncompatibleState "configuration mismatch");
  let changed_files =
    match changed_paths with
    | Some changed_paths ->
        restore_symbolic_links ~changed_paths ~local_root ~get_old_link_path:(fun path ->
            Ast.SharedMemory.SymlinksToPaths.get (Path.absolute path))
        |> List.map ~f:File.create
    | None -> compute_locally_changed_files ~scheduler ~configuration
  in
  let errors =
    EnvironmentSharedMemory.ServerErrors.find_unsafe "errors" |> File.Handle.Table.of_alist_exn
  in
  let state =
    { State.environment;
      errors;
      scheduler;
      last_request_time = Unix.time ();
      last_integrity_check = Unix.time ();
      connections;
      lookups = String.Table.create ();
      open_documents = Path.Set.empty
    }
  in
  Log.info "Reanalyzing %d files and their dependencies." (List.length changed_files);
  let state, _ = IncrementalCheck.recheck ~state ~configuration ~files:changed_files in
  state


let save ~configuration ~errors ~saved_state_path =
  Log.info "Saving server state to %s" saved_state_path;
  Memory.SharedMemory.collect `aggressive;
  EnvironmentSharedMemory.StoredConfiguration.add "configuration" configuration;
  EnvironmentSharedMemory.ServerErrors.add "errors" (Hashtbl.to_alist errors);
  Memory.save_shared_memory ~path:saved_state_path
