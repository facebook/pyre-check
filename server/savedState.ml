(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open Core

open Pyre

open Service


exception IncompatibleState of string

(* Return symlinks for files for a pyre project that are links to a separate project root. *)
let restore_symbolic_links ~changed_paths ~local_root ~get_old_link_path =
  let new_paths, removed_paths = List.partition_tf ~f:Path.file_exists changed_paths in
  (* We need to get the deleted paths from shared memory, as the version of the server launched from
     the saved state will have references to this files, whereas they won't be present in the
     new project root, meaning that we need to clean up the environment from them. *)
  let removed_paths = List.filter_map removed_paths ~f:get_old_link_path in
  (* Any member of new_paths might not have existed when the saved state was being created. *)
  let new_paths =
    let local_root_links =
      let file_filter file =
        Filename.check_suffix file ".py" ||
        Filename.check_suffix file ".pyi"
      in
      Path.list ~file_filter ~root:local_root ()
      |> fun links -> Path.build_symlink_map ~links
    in
    List.filter_map
      new_paths
      ~f:(Map.find local_root_links)
  in
  new_paths @ removed_paths


let load
    ~server_configuration:{
    Configuration.Server.configuration = ({
        Configuration.Analysis.expected_version;
        project_root;
        local_root;
        configuration_file_hash;
        _;
      } as configuration);
    saved_state_action;
    _;
  }
    ~lock
    ~connections =
  Log.info "Initializing server from saved state...";
  let shared_memory_path, changed_paths =
    match saved_state_action with
    | Some (Load (LoadFromFiles parameters)) ->
        let { Configuration.Server.shared_memory_path; changed_files_path } = parameters in
        let files =
          let to_path serialized =
            Path.create_absolute serialized
          in
          File.content (File.create changed_files_path)
          >>| String.split_lines
          >>| List.map ~f:to_path
          |> Option.value ~default:[]
        in
        shared_memory_path, files

    | Some (Load (LoadFromProject project_name)) ->
        if Option.is_none expected_version then
          begin
            Log.warning "An expected version must be passed in in order to load from saved states.";
            raise (IncompatibleState "version mismatch")
          end;

        Log.log ~section:`Server "Loading from saved state project `%s`..." project_name;
        let target_path = Constants.Server.saved_state_path configuration in
        Log.log ~section:`Server "Loading saved state to `%s`..." (Path.absolute target_path);
        let loaded_state =
          Path.search_upwards ~target:".watchmanconfig" ~root:project_root
          >>= fun watchman_root ->
          FetchSavedState.load
            ~watchman_root
            ~project_name
            ~configuration_file_hash
            ~version:(Option.value_exn expected_version)
            ~target_path
        in
        begin
          match loaded_state with
          | Some { FetchSavedState.saved_state_path; changed_files } ->
              saved_state_path, changed_files
          | None ->
              raise (IncompatibleState "unable to fetch state")
        end
    | _ ->
        raise (IncompatibleState "unexpected saved state parameters")
  in

  let scheduler = Scheduler.create ~configuration () in

  let environment = (module Environment.SharedHandler: Analysis.Environment.Handler) in

  Memory.load_shared_memory ~path:(Path.absolute shared_memory_path);
  let old_configuration = EnvironmentSharedMemory.StoredConfiguration.find_unsafe "configuration" in
  if not (Configuration.Analysis.equal old_configuration configuration) then
    raise (IncompatibleState "configuration mismatch");

  let changed_files =
    restore_symbolic_links
      ~changed_paths
      ~local_root
      ~get_old_link_path:(fun path -> Ast.SharedMemory.SymlinksToPaths.get (Path.absolute path))
    |> List.map ~f:File.create
  in
  Log.info "Reanalyzing %d files which have been modified." (List.length changed_files);
  let errors =
    EnvironmentSharedMemory.ServerErrors.find_unsafe "errors"
    |> File.Handle.Table.of_alist_exn
  in
  {
    State.deferred_state = State.Deferred.of_list changed_files;
    environment;
    errors;
    scheduler;
    lock;
    last_request_time = Unix.time ();
    last_integrity_check = Unix.time ();
    connections;
    lookups = String.Table.create ();
  }


let save ~configuration ~errors ~saved_state_path =
  Log.info "Saving server state to %s" saved_state_path;
  Memory.collect `aggressive;
  EnvironmentSharedMemory.StoredConfiguration.add "configuration" configuration;
  EnvironmentSharedMemory.ServerErrors.add "errors" (Hashtbl.to_alist errors);
  Memory.save_shared_memory ~path:saved_state_path
