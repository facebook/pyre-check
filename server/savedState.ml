(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open Core

open Pyre

open Service


exception IncompatibleState of string


let load
    ~server_configuration:{
    Configuration.Server.configuration = ({
        Configuration.Analysis.expected_version;
        project_root;
        _;
      } as configuration);
    saved_state_action;
    _;
  }
    ~lock
    ~connections =
  Log.info "Initializing server from saved state...";
  let shared_memory_path, changed_files =
    match saved_state_action with
    | Some (Load (LoadFromFiles parameters)) ->
        let { Configuration.Server.shared_memory_path; changed_files_path } = parameters in
        let files =
          let to_file serialized =
            Path.create_absolute serialized
            |> File.create
          in
          File.content (File.create changed_files_path)
          >>| String.split_lines
          >>| List.map ~f:to_file
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
            ~version:(Option.value_exn expected_version)
            ~target_path
        in
        begin
          match loaded_state with
          | Some { FetchSavedState.saved_state_path; changed_files } ->
              saved_state_path, List.map changed_files ~f:File.create
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

  Log.info "Reanalyzing %d files which have been modified." (List.length changed_files);
  let deferred_requests =
    [
      Protocol.Request.TypeCheckRequest
        (Protocol.TypeCheckRequest.create
           ~update_environment_with:changed_files
           ~check:changed_files
           ());
    ]
  in
  let errors =
    EnvironmentSharedMemory.ServerErrors.find_unsafe "errors"
    |> File.Handle.Table.of_alist_exn
  in
  {
    State.deferred_requests;
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
