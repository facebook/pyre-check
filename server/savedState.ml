(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open Core

open Pyre
open Service


exception IncompatibleState


let load
    ~server_configuration:{
    ServerConfiguration.configuration;
    saved_state;
    _;
  }
    ~lock
    ~connections =
  let { ServerConfiguration.shared_memory_path; changed_files_path } =
    match saved_state with
    | Some (Load parameters) ->
        parameters
    | _ ->
        raise IncompatibleState
  in
  Log.info "Initializing server from saved state at %s" (Path.absolute shared_memory_path);

  let scheduler = Scheduler.create ~configuration () in

  let environment = (module Environment.SharedHandler: Analysis.Environment.Handler) in

  Memory.load_shared_memory ~path:(Path.absolute shared_memory_path);
  let old_configuration = EnvironmentSharedMemory.StoredConfiguration.find_unsafe "configuration" in
  if not (Configuration.equal old_configuration configuration) then
    raise IncompatibleState;

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
  Log.info "Reanalyzing %d files which have been modified." (List.length files);
  let deferred_requests =
    [
      Protocol.Request.TypeCheckRequest
        (Protocol.TypeCheckRequest.create ~update_environment_with:files ~check:files ());
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
