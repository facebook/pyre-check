(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Network
open Server
open Protocol
module Time = Core_kernel.Time_ns.Span

exception NotRunning

let stop ~local_root =
  let configuration =
    Configuration.Analysis.create ~local_root:(Path.create_absolute local_root) ()
  in
  try
    let in_channel, _ =
      match Socket.open_connection (Operations.socket_path configuration) with
      | `Success socket -> socket
      | `Failure -> raise NotRunning
    in
    let socket = Unix.descr_of_in_channel in_channel in
    let readable =
      Unix.select ~read:[socket] ~write:[] ~except:[] ~timeout:(`After (Time.of_int_sec 5)) ()
      |> fun { Unix.Select_fds.read; _ } -> read
    in
    (* The timeout here is due to a race between the server's socket closing and the socket file
     * being read. If a connection to the socket is opened after the server closes its socket but
     * before the socket file is destroyed, the Socket.read will never get an EOF. *)
    if readable = [] then
      raise NotRunning;
    Socket.read socket
    |> fun (Handshake.ServerConnected _) ->
    Socket.write socket Handshake.ClientConnected;
    Socket.write socket Protocol.Request.StopRequest;
    ( match Socket.read socket with
    | StopResponse -> Log.info "Server stopped, polling for deletion of socket."
    | _ -> Log.error "Invalid response from server to stop request" );
    let poll_for_deletion paths =
      let start_time = Unix.time () in
      let timeout = 3.0 in
      let rec poll () =
        if Unix.time () -. start_time >=. timeout then (
          Log.warning "Timed out while polling for server to stop.";
          1 )
        else if List.exists ~f:Path.file_exists paths then (
          Unix.nanosleep 0.1 |> ignore;
          poll () )
        else (
          Log.info "Server successfully stopped.";
          0 )
      in
      poll ()
    in
    try
      let socket_paths =
        [ Operations.socket_path configuration;
          Operations.socket_path ~name:"json_server" configuration ]
      in
      match poll_for_deletion socket_paths with
      | exit_code -> exit_code
      | exception Operations.ServerNotRunning ->
          (* Our job is done if the server is not running. *)
          0
    with
    | Operations.ServerNotRunning ->
        (* The initial call to socket_path might also fail if the server's cleaned up already. *)
        0
  with
  | NotRunning
  | Unix.Unix_error _
  | Operations.ServerNotRunning ->
      Log.warning "No servers running";
      1


let run local_root () =
  let code = stop ~local_root in
  exit code


let command =
  Command.basic_spec
    ~summary:"Stop the server"
    Command.Spec.(empty +> anon (maybe_with_default "." ("source-root" %: string)))
    run
