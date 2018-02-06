(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open Core
open Hack_parallel.Std


open Pyre

open Configuration
open ServerConfiguration
open Protocol

module Time = Core_kernel.Time_ns.Span
module Check = CommandCheck
module Handshake = CommandHandshake
module Socket = CommandSocket
module Request = ServerRequest
module State = ServerState

type version_mismatch = {
  server_version: string;
  client_version: string;
}
[@@deriving show]

exception AlreadyRunning
exception NotRunning
exception ConnectionFailure
exception VersionMismatch of version_mismatch

open State


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


let computation_thread request_queue configuration state =
  let rec loop ({ configuration = { source_root; _ }; _ } as configuration) state =
    let errors_to_lsp_responses error_map =
      let diagnostic_to_response = function
        | Ok diagnostic_error -> [
            diagnostic_error
            |> LanguageServer.Protocol.PublishDiagnostics.to_yojson
            |> Yojson.Safe.to_string
            |> fun serialized_diagnostic -> LanguageServerProtocolResponse serialized_diagnostic
          ]
        | Error _ -> []
      in
      error_map
      |> List.map
        ~f:(fun (handle, errors) ->
            LanguageServer.Protocol.PublishDiagnostics.of_errors ~root:source_root handle errors)
      |> List.concat_map ~f:diagnostic_to_response
    in
    (* Decides what to broadcast to persistent clients after a request is processed. *)
    let broadcast_response lock persistent_clients response =
      match response with
      | TypeCheckResponse error_map ->
          let responses = errors_to_lsp_responses error_map in
          let write_or_mark_failure responses ~key:socket ~data:{ failures } =
            try
              List.iter ~f:(Socket.write socket) responses;
              { failures }
            with
            | (Unix.Unix_error (kind, name, description)) ->
                (match kind with
                 | Unix.EPIPE ->
                     Log.warning "Got an EPIPE while broadcasting to a persistent client";
                     { failures = failures + 1 }
                 | _ -> raise (Unix.Unix_error (kind, name, description)))
          in
          Mutex.critical_section lock ~f:(fun () ->
              Hashtbl.mapi_inplace ~f:(write_or_mark_failure responses) persistent_clients;
              Hashtbl.filter_inplace
                ~f:(fun { failures } -> failures < State.failure_threshold)
                persistent_clients)
      | _ -> ()
    in
    let handle_request state ~request:(origin, request) =
      let process_request socket state configuration request =
        try
          Request.process_request socket state configuration request
        with
        | Request.InvalidRequest ->
            Log.error "Exiting due to invalid request";
            Mutex.critical_section
              state.lock
              ~f:(fun () ->
                  ServerOperations.stop_server
                    configuration
                    !(state.connections).socket);
            state, None
      in
      match origin with
      | Protocol.Request.PersistentSocket socket ->
          let write_or_forget socket responses =
            try
              List.iter ~f:(Socket.write socket) responses
            with
            | Unix.Unix_error (kind, _, _) ->
                (match kind with
                 | Unix.EPIPE ->
                     Log.warning "EPIPE while writing to a persistent client, removing.";
                     Mutex.critical_section state.lock ~f:(fun () ->
                         let { persistent_clients; _ } = !(state.connections) in
                         match Hashtbl.find persistent_clients socket with
                         | Some { failures } ->
                             if failures < State.failure_threshold then
                               Hashtbl.set
                                 persistent_clients
                                 ~key:socket
                                 ~data:{ failures = failures + 1 }
                             else
                               Hashtbl.remove persistent_clients socket
                         | None ->
                             ())
                 | _ ->
                     ())
          in
          let state, response = process_request socket state configuration request in
          (match response with
           | Some (LanguageServerProtocolResponse _)
           | Some (ClientExitResponse Persistent) ->
               response
               >>| (fun response -> write_or_forget socket [response])
               |> ignore
           | Some (TypeCheckResponse error_map) ->
               let responses = errors_to_lsp_responses error_map in
               write_or_forget socket responses
           | Some _ -> Log.error "Unexpected response for persistent client request"
           | None -> ());
          state
      | Protocol.Request.FileNotifier
      | Protocol.Request.Background ->
          let { socket; persistent_clients; _ } =
            Mutex.critical_section state.lock ~f:(fun () -> !(state.connections))
          in
          let state, response = process_request socket state configuration request in
          (match response with
           | Some response ->
               broadcast_response state.lock persistent_clients response
           | None ->
               ());
          state
      | Protocol.Request.NewConnectionSocket socket ->
          let { persistent_clients; _ } =
            Mutex.critical_section state.lock ~f:(fun () -> !(state.connections))
          in
          let state, response = process_request socket state configuration request in
          (match response with
           | Some response ->
               Socket.write_ignoring_epipe socket response;
               broadcast_response state.lock persistent_clients response
           | None -> ());
          state
    in
    let state =
      match Squeue.length request_queue, List.is_empty state.deferred_requests with
      | 0, false ->
          let state =
            {
              state with
              deferred_requests = Protocol.Request.flatten state.deferred_requests;
              last_request_time = Unix.time ();
            }
          in
          (match state.deferred_requests with
           | [] ->
               state
           | request :: requests ->
               let state = { state with deferred_requests = requests } in
               handle_request state ~request:(Protocol.Request.Background, request))
      | 0, true ->
          let current_time = Unix.time () in
          if current_time -. state.last_request_time > State.stop_after_idle_for then
            begin
              Statistics.event
                ~name:"stop idle"
                ~configuration:configuration.configuration
                ();
              Mutex.critical_section
                state.lock
                ~f:(fun () ->
                    ServerOperations.stop_server
                      configuration
                      !(state.connections).socket)
            end;
          (* This sleep is necessary because OCaml threads aren't pre-emptively scheduled. *)
          Unix.nanosleep 0.1 |> ignore;
          state
      | _ ->
          let state = { state with last_request_time = Unix.time () } in
          handle_request state ~request:(Squeue.pop request_queue)
    in
    loop configuration state
  in
  loop configuration state


let request_handler_thread (
    { configuration = { version; _ }; _ } as server_configuration,
    lock,
    connections,
    request_queue) =
  let get_readable_sockets () =
    Mutex.critical_section lock ~f:(fun () -> !connections)
  in
  let queue_request ~origin request =
    match request, origin with
    | Protocol.Request.StopRequest, Protocol.Request.NewConnectionSocket socket ->
        Socket.write socket Protocol.StopResponse;
        ServerOperations.stop_server server_configuration !(connections).socket
    | Protocol.Request.StopRequest, _ ->
        ServerOperations.stop_server server_configuration !(connections).socket
    | Protocol.Request.ClientConnectionRequest client,
      Protocol.Request.NewConnectionSocket socket ->
        Log.log ~section:`Server "Adding %s client" (Protocol.show_client client);
        Mutex.critical_section
          lock
          ~f:(fun () ->
              let { persistent_clients; file_notifiers; _ } = !connections in
              Socket.write socket (Protocol.ClientConnectionResponse client);
              connections :=
                (match client with
                 | Persistent ->
                     Hashtbl.set persistent_clients ~key:socket ~data:{ failures = 0 };
                     !connections
                 | FileNotifier ->
                     { !connections with file_notifiers = socket::file_notifiers }))
    | Protocol.Request.ClientConnectionRequest _, _ ->
        Log.error
          "Unexpected request origin %s for connection request"
          (Protocol.Request.origin_name origin)
    | Protocol.Request.ReinitializeStateRequest, _ ->
        Squeue.clear request_queue;
        Squeue.push_or_drop request_queue (origin, request) |> ignore
    | _ ->
        Squeue.push_or_drop request_queue (origin, request) |> ignore;
  in
  let handle_readable_persistent socket =
    try
      Log.log ~section:`Server "A persistent client socket is readable.";
      let request = Socket.read socket in
      queue_request ~origin:(Protocol.Request.PersistentSocket socket) request
    with
    | End_of_file ->
        Log.log ~section:`Server "Persistent client disconnected";
        Mutex.critical_section lock
          ~f:(fun () ->
              let persistent_clients = !(connections).persistent_clients in
              Hashtbl.remove persistent_clients socket)
  in
  let handle_readable_file_notifier socket =
    try
      Log.log ~section:`Server "A file notifier is readable.";
      let request = Socket.read socket in
      queue_request ~origin:Protocol.Request.FileNotifier request
    with
    | End_of_file ->
        Log.log ~section:`Server "File notifier disconnected";
        Mutex.critical_section lock
          ~f:(fun () ->
              let file_notifiers =
                List.concat_map
                  ~f:(fun file_notifier_socket ->
                      if socket = file_notifier_socket then
                        begin
                          Log.log ~section:`Server "Removing file notifier";
                          Unix.close socket;
                          []
                        end
                      else
                        [file_notifier_socket])
                  !(connections).file_notifiers
              in
              connections := { !connections with file_notifiers });
  in
  let rec loop () =
    let { socket = server_socket; persistent_clients; file_notifiers } = get_readable_sockets () in
    let readable =
      Unix.select
        ~read:(server_socket :: (Hashtbl.keys persistent_clients) @ file_notifiers )
        ~write:[]
        ~except:[]
        ~timeout:(`After (Time.of_sec 5.0))
        ()
      |> fun { Unix.Select_fds.read; _ } -> read
    in
    let handle_socket socket =
      if socket = server_socket then
        begin
          let new_socket, _ =
            Log.log ~section:`Server "New client connection";
            Unix.accept server_socket
          in
          Socket.write new_socket (Handshake.ServerConnected (Option.value ~default:"-1" version));
          try
            Socket.read new_socket
            |> fun Handshake.ClientConnected ->
            let request = Socket.read new_socket in
            queue_request ~origin:(Protocol.Request.NewConnectionSocket new_socket) request
          with
          | End_of_file ->
              Log.warning "New client socket unreadable"
        end
      else if Mutex.critical_section lock ~f:(fun () -> Hashtbl.mem persistent_clients socket) then
        handle_readable_persistent socket
      else
        handle_readable_file_notifier socket
    in
    List.iter ~f:handle_socket readable;
    loop ()
  in
  loop ()


(** Main server either as a daemon or in terminal *)
let serve (socket, server_configuration) =
  let ({ Configuration.verbose; sections; _ } as configuration) =
    server_configuration.configuration
  in
  Log.initialize ~verbose ~sections;
  Log.log ~section:`Server "Starting daemon server loop...";
  let request_queue = Squeue.create 25 in
  let lock = Mutex.create () in
  let connections = ref {
      socket;
      persistent_clients = Unix.File_descr.Table.create ();
      file_notifiers = [];
    }
  in
  Thread.create
    request_handler_thread
    (server_configuration, lock, connections, request_queue)
  |> ignore;

  let state =
    ServerOperations.initialize lock connections server_configuration
  in
  Signal.Expert.handle
    Signal.int
    (fun _ -> ServerOperations.stop_server server_configuration socket);
  Signal.Expert.handle
    Signal.pipe
    (fun _ -> ());
  try
    computation_thread request_queue server_configuration state
  with _ ->
    let backtrace = Printexc.get_backtrace () in
    Statistics.event
      ~flush:true
      ~name:"uncaught exception"
      ~configuration
      ~integers:[]
      ~normals:["exception backtrace", backtrace]
      ();
    ServerOperations.stop_server server_configuration socket


(* Create lock file and pid file. Used for both daemon mode and in-terminal *)
let setup { lock_path; pid_path; _ } =
  let pid = Unix.getpid () |> Pid.to_int in
  if Lock_file.create ~unlink_on_exit:true (Path.absolute lock_path) <> true then
    raise AlreadyRunning;

  Out_channel.with_file
    (Path.absolute pid_path)
    ~f:(fun out_channel ->
        Format.fprintf (Format.formatter_of_out_channel out_channel) "%d%!" pid)

(** Daemon forking code *)
type run_server_daemon_entry =
  ((Socket.t * ServerConfiguration.t), unit Daemon.in_channel, unit Daemon.out_channel) Daemon.entry


(** When spawned, a child is passed input/output channels that can communicate
    with the parent process. We don't currently use these, so close them after
    spawning. *)
let run_server_daemon_entry : run_server_daemon_entry =
  Daemon.register_entry_point
    "server_daemon"
    (fun (socket, server_configuration) (parent_in_channel, parent_out_channel)  ->
       Daemon.close_in parent_in_channel;
       Daemon.close_out parent_out_channel;
       (* Detach the from a controlling terminal *)
       Unix.Terminal_io.setsid () |> ignore;
       setup server_configuration;
       serve (socket, server_configuration))


let start ({
    lock_path;
    socket_path;
    log_path;
    daemonize;
    configuration;
    _;
  } as server_configuration) =
  try
    Log.initialize
      ~verbose:configuration.Configuration.verbose
      ~sections:configuration.Configuration.sections;

    Log.info "Starting up server...";

    if Lock_file.is_locked (Path.absolute lock_path) then
      raise AlreadyRunning;

    Log.log ~section:`Server "Creating server socket at `%a`" Path.pp socket_path;
    let socket = Socket.initialize_unix_socket socket_path in

    if daemonize then
      let stdin = Daemon.null_fd () in
      let stdout = Daemon.fd_of_path (Path.absolute log_path) in
      Log.log ~section:`Server "Spawning the daemon now.";
      let { Daemon.pid; _ } as handle =
        Daemon.spawn
          (stdin, stdout, stdout)
          run_server_daemon_entry (socket, server_configuration)
      in
      Daemon.close handle;
      Log.log ~section:`Server "Forked off daemon with pid %d" pid;
      Log.info "Server starting in background";
      pid
    else begin
      setup server_configuration;
      Log.log ~section:`Server "Server running in terminal as pid %d" (Pid.to_int (Unix.getpid ()));
      Signal.Expert.handle
        Signal.int
        (fun _ -> ServerOperations.stop_server server_configuration socket);
      serve (socket, server_configuration);
      0
    end
  with AlreadyRunning ->
    Log.info "Server is already running";
    0


(** Default configuration when run from command line *)
let run_start_command
    log_path
    terminal
    verbose
    version
    sections
    debug
    strict
    declare
    show_error_traces
    infer
    recursive_infer
    sequential
    log_identifier
    project_root
    stub_roots
    source_root
    () =
  let configuration =
    Configuration.create
      ~verbose
      ?version
      ~sections
      ~debug
      ~infer
      ~recursive_infer
      ~strict
      ~declare
      ~show_error_traces
      ~log_identifier
      ~parallel:(not sequential)
      ~project_root:(Path.create_absolute project_root)
      ~stub_roots:(List.map ~f:Path.create_absolute stub_roots)
      ~source_root:(Path.create_absolute source_root)
      ()
  in
  let log_path = log_path >>| Path.create_absolute in
  start (ServerConfiguration.create ~daemonize:(not terminal) ?log_path configuration)
  |> ignore


let start_command =
  Command.basic_spec
    ~summary:"Starts a server in the foreground by default. See help for daemon options."
    Command.Spec.(
      (empty
       +> flag
         "-log-file"
         (optional file)
         ~doc:(Format.sprintf "filename Log file (Default is ./pyre/server/server.stdout)")
       +> flag
         "-terminal"
         no_arg
         ~doc:"Run the server from the terminal instead of running as a daemon.")
      ++ Check.spec)
    run_start_command


let stop source_root () =
  let configuration = Configuration.create ~source_root:(Path.create_absolute source_root) () in
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
    match Socket.read socket with
    | Protocol.StopResponse -> Log.info "Server stopped"
    | _ -> Log.error "Invalid response from server to stop request";
  with
  | NotRunning
  | Unix.Unix_error _
  | ServerConfiguration.ServerNotRunning ->
      Log.warning "No servers running"


let stop_command =
  Command.basic_spec
    ~summary:"Stop the server"
    Command.Spec.(
      empty
      +> anon (maybe_with_default "." ("source-root" %: string)))
    stop
