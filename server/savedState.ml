(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open Core

open Pyre
open Service


let load
    ~server_configuration:{ ServerConfiguration.configuration; load_state_from; _ }
    ~lock
    ~connections =
  let saved_state_path = Option.value_exn load_state_from in
  Log.info "Initializing server from saved state at %s" saved_state_path;
  (* TODO(T33300184): We need to validate the configuration against the passed in config. *)
  let scheduler = Scheduler.create ~configuration () in

  let environment = (module Environment.SharedHandler: Analysis.Environment.Handler) in

  Memory.load_shared_memory ~path:saved_state_path;

  (* TODO(T33300361): We need to invalidate changed/removed files and reanalyze them here. *)
  let files =
    let handles = Ast.SharedMemory.HandleKeys.get () in
    let file handle =
      Ast.SharedMemory.Sources.get handle
      >>= (fun { Ast.Source.path; _ } -> path)
      >>| File.create
    in
    List.filter_map handles ~f:file
  in
  Log.info "Reanalyzing %d files which have been modified." (List.length files);
  let deferred_requests =
    [
      Protocol.Request.TypeCheckRequest
        (Protocol.TypeCheckRequest.create ~check:files ());
    ]
  in
  {
    State.deferred_requests;
    environment;
    errors = File.Handle.Table.create ();
    scheduler;
    lock;
    last_request_time = Unix.time ();
    last_integrity_check = Unix.time ();
    connections;
    lookups = String.Table.create ();
  }



let save ~saved_state_path =
  Log.info "Saving server state to %s" saved_state_path;
  Memory.collect `aggressive;
  Memory.save_shared_memory ~path:saved_state_path
