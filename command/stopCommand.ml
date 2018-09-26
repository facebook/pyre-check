(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open Core

open Pyre

open Network
open Server
open Protocol

module Time = Core_kernel.Time_ns.Span

exception NotRunning


let run_stop graceful local_root () =
  let configuration = Configuration.create ~local_root:(Path.create_absolute local_root) () in
  (* Force an immediate process exit from this function only if
     performing a non-graceful shutdown. *)
  let exit_strategy = if graceful then ignore else exit in
  try
    let in_channel, _ =
      match Socket.open_connection (ServerConfiguration.socket_path configuration) with
      | `Success socket -> socket
      | `Failure -> raise NotRunning
    in
    let socket = Unix.descr_of_in_channel in_channel in
    let readable =
      Unix.select
        ~read:[socket]
        ~write:[]
        ~except:[]
        ~timeout:(`After (Time.of_int_sec 5))
        ()
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
    begin
      match Socket.read socket with
      | StopResponse -> Log.info "Server stopped, polling for deletion of socket."
      | _ -> Log.error "Invalid response from server to stop request"
    end;
    let poll_for_deletion path =
      let start_time = Unix.time () in
      let timeout = 3.0 in
      let rec poll () =
        if Unix.time () -. start_time >=. timeout then
          begin
            Log.warning "Timed out while polling for server to stop.";
            1
          end
        else if Path.file_exists path then
          (Unix.nanosleep 0.1 |> ignore; poll ())
        else
          begin
            Log.info "Server successfully stopped.";
            0
          end
      in
      poll ()
    in
    match poll_for_deletion (ServerConfiguration.socket_path configuration) with
    | exit_code ->
        exit_strategy exit_code
    | exception ServerConfiguration.ServerNotRunning ->
        (* Our job is done if the server is not running. *)
        exit_strategy 0
  with
  | NotRunning
  | Unix.Unix_error _
  | ServerConfiguration.ServerNotRunning ->
      Log.warning "No servers running";
      exit_strategy 1

let run ?(graceful=false) local_root () =
  run_stop graceful local_root ()


let command =
  Command.basic_spec
    ~summary:"Stop the server"
    Command.Spec.(
      empty
      +> flag "-graceful" no_arg ~doc:"Perform a graceful exit"
      +> anon (maybe_with_default "." ("source-root" %: string)))
    run_stop
