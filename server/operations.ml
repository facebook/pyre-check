(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre
open Network
open ServerConfiguration
open State
open Service
open Constants


type version_mismatch = {
  server_version: string;
  expected_version: string;
}
[@@deriving show]


exception ConnectionFailure
exception VersionMismatch of version_mismatch


let start_from_scratch ?old_state ~lock ~connections ~configuration () =
  Log.log ~section:`Server  "Initializing server...";

  let scheduler =
    match old_state with
    | Some { scheduler; _ } -> scheduler
    | None -> Scheduler.create ~configuration ()
  in
  SharedMem.collect `aggressive;

  let timer = Timer.start () in
  let { TypeCheck.handles; environment; errors } =
    TypeCheck.check
      ~scheduler:(Some scheduler)
      ~configuration
  in
  Ast.SharedMemory.HandleKeys.add ~handles;
  Statistics.performance ~name:"initialization" ~timer ~normals:[] ();
  Log.log ~section:`Server "Server initialized";
  Memory.init_done ();

  let errors =
    let table = File.Handle.Table.create () in
    let add_error error =
      Hashtbl.add_multi
        table
        ~key:(File.Handle.create (Error.path error))
        ~data:error
    in
    List.iter errors ~f:add_error;
    table
  in
  {
    deferred_requests = [];
    environment;
    errors;
    scheduler;
    lock;
    last_request_time = Unix.time ();
    last_integrity_check = Unix.time ();
    connections;
    lookups = String.Table.create ();
  }


let start
    ?old_state
    ~lock
    ~connections
    ~configuration:({ configuration; save_state_to; load_state_from; _ } as server_configuration)
    () =
  let ({ State.errors; _ } as state) =
    match load_state_from with
    | Some saved_state_path ->
        Log.info "Loading from saved state at %s" saved_state_path;
        SavedState.load ~server_configuration ~lock ~connections
    | _ ->
        start_from_scratch ?old_state ~lock ~connections ~configuration ()
  in
  begin
    match save_state_to with
    | Some saved_state_path ->
        SavedState.save ~configuration ~errors ~saved_state_path
    | _ ->
        ()
  end;
  state


let stop
    ~reason
    ~configuration:{ configuration; lock_path; socket_path; pid_path; socket_link; _ }
    ~socket =
  Statistics.event ~flush:true ~name:"stop server" ~normals:["reason", reason] ();
  let watchman_pid =
    try
      Watchman.pid_path configuration
      |> Path.absolute
      |> Sys_utils.cat
      |> Int.of_string
      |> Pid.of_int
      |> fun pid -> Some (`Pid pid)
    with Sys_error _ ->
      None
  in
  watchman_pid >>| Signal.send_i Signal.int  |> ignore;

  Path.absolute lock_path
  |> Lock.release
  |> ignore;

  (* Cleanup server files. *)
  Path.remove socket_path;
  Path.remove socket_link;
  Path.remove pid_path;

  Unix.close socket;
  Worker.killall ();
  exit 0


let connect ~retries ~configuration:({ Configuration.expected_version; _ } as configuration) =
  let rec connect attempt =
    if attempt >= retries then begin
      Log.error "Could not connect to server after %d retries" attempt;
      raise ConnectionFailure
    end;
    (* The socket path is computed in each iteration because the server might set up a symlink
       after a connection attempt - in that case, we want to avoid using the stale file. *)
    try
      let socket_path = ServerConfiguration.socket_path configuration in
      if Path.file_exists socket_path then
        begin
          match
            (Unix.handle_unix_error
               (fun () -> Socket.open_connection (ServerConfiguration.socket_path configuration)))
          with
          | `Success socket ->
              Log.info "Connected to server";
              socket
          | `Failure ->
              Log.info "Client could not connect.";
              Unix.sleep 1;
              connect (attempt + 1)
        end
      else
        begin
          Log.info "No valid socket found.";
          Unix.sleep 1;
          connect (attempt + 1)
        end
    with
    | ServerNotRunning ->
        Log.info "Waiting for server...";
        Unix.sleep 1;
        connect (attempt + 1)
  in
  let socket =
    let in_channel, _ = connect 0 in
    Unix.descr_of_in_channel in_channel
  in
  Log.debug "Waiting for server response...";
  Socket.read socket
  |> fun (Handshake.ServerConnected server_version) ->
  Socket.write socket Handshake.ClientConnected;
  match expected_version with
  | Some version when version = server_version ->
      socket
  | None ->
      socket
  | Some expected_version ->
      Unix.close socket;
      raise (VersionMismatch { server_version; expected_version })
