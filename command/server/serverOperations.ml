(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Configuration
open Pyre
open Service
open ServerConfiguration
open ServerState

module Check = CommandCheck
module Handshake = CommandHandshake
module WatchmanConstants = CommandWatchmanConstants

type version_mismatch = {
  server_version: string;
  client_version: string;
}
[@@deriving show]

exception ConnectionFailure
exception VersionMismatch of version_mismatch


let initialize ?old_state lock connections { configuration; _ } =
  Log.log ~section:`Server  "Initializing server...";
  let scheduler =
    match old_state with
    | Some { scheduler; _ } -> scheduler
    | None -> Scheduler.create ~is_parallel:configuration.parallel ()
  in
  SharedMem.collect `aggressive;
  let timer = Timer.start () in
  let { Check.handles; environment; errors = initial_errors } =
    Check.check configuration (Some scheduler) () in
  Statistics.performance ~name:"initialization" ~timer ~configuration ~normals:[] ();
  Log.log ~section:`Server "Server initialized";
  let handles = File.Handle.Set.of_list handles in
  let errors =
    let errors = File.Handle.Table.create () in
    List.iter
      initial_errors
      ~f:(fun error ->
          let { Ast.Location.path; _ } = Error.location error in
          Hashtbl.add_multi
            errors
            ~key:(File.Handle.create path)
            ~data:error);
    errors
  in
  {
    deferred_requests = [];
    environment;
    initial_errors = Error.Hash_set.of_list initial_errors;
    errors;
    handles;
    scheduler;
    lock;
    last_request_time = Unix.time ();
    connections;
    lookups = String.Table.create ();
  }


let remove_server_files { lock_path; socket_path; pid_path; socket_link; _ } =
  Path.remove lock_path;
  Path.remove socket_path;
  Path.remove socket_link;
  Path.remove pid_path


let stop_server ~reason ({ configuration; _ } as server_configuration) socket =
  Statistics.event ~flush:true ~name:"stop server" ~configuration ~normals:["reason", reason] ();
  let watchman_pid =
    try
      WatchmanConstants.pid_path configuration
      |> Path.absolute
      |> Sys_utils.cat
      |> Int.of_string
      |> Pid.of_int
      |> fun pid -> Some (`Pid pid)
    with Sys_error _ ->
      None
  in
  watchman_pid >>| Signal.send_i Signal.int  |> ignore;
  remove_server_files server_configuration;
  Unix.close socket;
  Worker.killall ();
  exit 0


let connect ~retries ~configuration:({ version; _ } as configuration) =
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
  match version with
  | Some version when version = server_version ->
      socket
  | None ->
      socket
  | Some client_version ->
      Unix.close socket;
      raise (VersionMismatch { server_version; client_version })
