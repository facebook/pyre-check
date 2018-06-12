(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open Core
open Hack_parallel.Std


open Pyre

open Configuration
open ServerConfiguration
open ServerProtocol

module Time = Core_kernel.Time_ns.Span
module Handshake = CommandHandshake
module Socket = CommandSocket
module Request = ServerRequest
module State = ServerState

exception AlreadyRunning
exception NotRunning
open State

let register_signal_handlers server_configuration socket =
  Signal.Expert.handle
    Signal.int
    (fun _ -> ServerOperations.stop_server ~reason:"interrupt" server_configuration socket);
  Signal.Expert.handle
    Signal.pipe
    (fun _ -> ())


let spawn_watchman_client { configuration = { sections; source_root; _ }; _ } =
  CommandWatchman.run_command
    ~daemonize:true
    ~verbose:false
    ~sections
    ~source_root:(Path.absolute source_root)


let computation_thread request_queue configuration state =
  let rec loop ({ configuration = { source_root; _ }; pid_path; _ } as configuration) state =
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
                begin
                  match kind with
                  | Unix.EPIPE ->
                      Log.warning "Got an EPIPE while broadcasting to a persistent client";
                      { failures = failures + 1 }
                  | _ -> raise (Unix.Unix_error (kind, name, description))
                end
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
          Log.log ~section:`Server "Processing request %a" ServerProtocol.Request.pp request;
          Request.process_request socket state configuration request
        with
        | Request.InvalidRequest ->
            Log.error "Exiting due to invalid request";
            Mutex.critical_section
              state.lock
              ~f:(fun () ->
                  ServerOperations.stop_server
                    ~reason:"malformed request"
                    configuration
                    !(state.connections).socket);
            state, None
      in
      match origin with
      | ServerProtocol.Request.PersistentSocket socket ->
          let write_or_forget socket responses =
            try
              List.iter ~f:(Socket.write socket) responses
            with
            | Unix.Unix_error (kind, _, _) ->
                begin
                  match kind with
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
                      ()
                end
          in
          let state, response = process_request socket state configuration request in
          begin
            match response with
            | Some (LanguageServerProtocolResponse _)
            | Some (ClientExitResponse Persistent) ->
                response
                >>| (fun response -> write_or_forget socket [response])
                |> ignore
            | Some (TypeCheckResponse error_map) ->
                let responses = errors_to_lsp_responses error_map in
                write_or_forget socket responses
            | Some _ -> Log.error "Unexpected response for persistent client request"
            | None -> ()
          end;
          state
      | ServerProtocol.Request.FileNotifier
      | ServerProtocol.Request.Background ->
          let { socket; persistent_clients; _ } =
            Mutex.critical_section state.lock ~f:(fun () -> !(state.connections))
          in
          let state, response = process_request socket state configuration request in
          begin
            match response with
            | Some response ->
                broadcast_response state.lock persistent_clients response
            | None ->
                ()
          end;
          state
      | ServerProtocol.Request.NewConnectionSocket socket ->
          let { persistent_clients; _ } =
            Mutex.critical_section state.lock ~f:(fun () -> !(state.connections))
          in
          let state, response = process_request socket state configuration request in
          begin
            match response with
            | Some response ->
                Socket.write_ignoring_epipe socket response;
                broadcast_response state.lock persistent_clients response
            | None -> ()
          end;
          state
    in
    let state =
      match Squeue.length request_queue, List.is_empty state.deferred_requests with
      | 0, false ->
          let state =
            {
              state with
              deferred_requests = ServerProtocol.Request.flatten state.deferred_requests;
              last_request_time = Unix.time ();
            }
          in
          begin
            match state.deferred_requests with
            | [] ->
                state
            | request :: requests ->
                let state = { state with deferred_requests = requests } in
                handle_request state ~request:(ServerProtocol.Request.Background, request)
          end
      | 0, true ->
          (* Stop if the server is idle. *)
          let current_time = Unix.time () in
          if current_time -. state.last_request_time > State.stop_after_idle_for then
            begin
              Mutex.critical_section
                state.lock
                ~f:(fun () ->
                    ServerOperations.stop_server
                      ~reason:"idle"
                      configuration
                      !(state.connections).socket)
            end;
          (* Stop if there's any inconsistencies in the .pyre directory. *)
          let last_integrity_check =
            if current_time -. state.last_integrity_check > State.integrity_check_every then
              begin
                let pid_file =
                  Path.absolute pid_path
                  |> In_channel.create
                in
                let pid =
                  protect
                    ~f:(fun () -> In_channel.input_all pid_file)
                    ~finally:(fun () -> In_channel.close pid_file)
                in
                if not (Pid.to_string (Unix.getpid ()) = pid) then
                  Mutex.critical_section
                    state.lock
                    ~f:(fun () ->
                        Log.error
                          "Stopping server in integrity check. Got %s in the pid file, expected %s."
                          pid
                          (Pid.to_string (Unix.getpid ()));
                        ServerOperations.stop_server
                          ~reason:"failed integrity check"
                          configuration
                          !(state.connections).socket);
                current_time
              end
            else
              state.last_integrity_check
          in
          (* This sleep is necessary because OCaml threads aren't pre-emptively scheduled. *)
          Unix.nanosleep 0.1 |> ignore;
          { state with last_integrity_check }
      | _ ->
          let state = { state with last_request_time = Unix.time () } in
          handle_request state ~request:(Squeue.pop request_queue)
    in
    loop configuration state
  in
  loop configuration state


let request_handler_thread
    ({
      configuration = ({ expected_version; source_root; _ } as configuration);
      use_watchman;
      _;
    } as server_configuration,
      lock,
      connections,
      request_queue) =

  let get_readable_sockets () =
    Mutex.critical_section lock ~f:(fun () -> !connections)
  in
  let queue_request ~origin request =
    match request, origin with
    | ServerProtocol.Request.StopRequest, ServerProtocol.Request.NewConnectionSocket socket ->
        Socket.write socket StopResponse;
        ServerOperations.stop_server
          ~reason:"explicit request"
          server_configuration
          !(connections).socket
    | ServerProtocol.Request.StopRequest, _ ->
        ServerOperations.stop_server
          ~reason:"explicit request"
          server_configuration
          !(connections).socket
    | ServerProtocol.Request.ClientConnectionRequest client,
      ServerProtocol.Request.NewConnectionSocket socket ->
        Log.log ~section:`Server "Adding %s client" (show_client client);
        Mutex.critical_section
          lock
          ~f:(fun () ->
              let { persistent_clients; file_notifiers; _ } = !connections in
              Socket.write socket (ClientConnectionResponse client);
              connections :=
                begin
                  match client with
                  | Persistent ->
                      Hashtbl.set persistent_clients ~key:socket ~data:{ failures = 0 };
                      !connections
                  | FileNotifier ->
                      { !connections with file_notifiers = socket::file_notifiers }
                end)
    | ServerProtocol.Request.ClientConnectionRequest _, _ ->
        Log.error
          "Unexpected request origin %s for connection request"
          (ServerProtocol.Request.origin_name origin)
    | _ ->
        Squeue.push_or_drop request_queue (origin, request) |> ignore;
  in
  let handle_readable_persistent socket =
    try
      Log.log ~section:`Server "A persistent client socket is readable.";
      let request = Socket.read socket in
      queue_request ~origin:(ServerProtocol.Request.PersistentSocket socket) request
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
      queue_request ~origin:ServerProtocol.Request.FileNotifier request
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
              if List.is_empty file_notifiers && use_watchman then
                spawn_watchman_client server_configuration;
              connections := { !connections with file_notifiers });
  in
  let rec loop () =
    let { socket = server_socket; persistent_clients; file_notifiers } = get_readable_sockets () in
    if not (PyrePath.is_directory source_root) then
      begin
        Log.error "Stopping server due to missing source root.";
        ServerOperations.stop_server
          ~reason:"missing source root"
          server_configuration
          !(connections).socket
      end;
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
          Socket.write
            new_socket
            (Handshake.ServerConnected (Option.value ~default:"-1" expected_version));
          try
            Socket.read new_socket
            |> fun Handshake.ClientConnected ->
            let request = Socket.read new_socket in
            queue_request ~origin:(ServerProtocol.Request.NewConnectionSocket new_socket) request
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
  try
    loop ()
  with _ ->
    let backtrace = Printexc.get_backtrace () in
    Statistics.event
      ~flush:true
      ~name:"uncaught exception"
      ~configuration
      ~integers:[]
      ~normals:["exception backtrace", backtrace]
      ();
    ServerOperations.stop_server ~reason:"exception" server_configuration (!connections).socket


(** Main server either as a daemon or in terminal *)
let serve (socket, server_configuration) =
  Log.info "Server running as pid %d" (Pid.to_int (Unix.getpid ()));
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
  register_signal_handlers server_configuration socket;
  Thread.create
    request_handler_thread
    (server_configuration, lock, connections, request_queue)
  |> ignore;

  let state =
    ServerOperations.initialize lock connections server_configuration
  in
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
    ServerOperations.stop_server ~reason:"exception" server_configuration socket


(* Create lock file and pid file. Used for both daemon mode and in-terminal *)
let setup { lock_path; pid_path; _ } =
  let pid = Unix.getpid () |> Pid.to_int in
  if not (Lock.grab (Path.absolute lock_path)) then
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
    use_watchman;
    _;
  } as server_configuration) =
  try
    Log.initialize
      ~verbose:configuration.Configuration.verbose
      ~sections:configuration.Configuration.sections;

    Log.info "Starting up server...";

    if not (Lock.check (Path.absolute lock_path)) then
      raise AlreadyRunning;

    Log.log ~section:`Server "Creating server socket at `%a`" Path.pp socket_path;
    let socket = Socket.initialize_unix_socket socket_path in

    if use_watchman then
      spawn_watchman_client server_configuration;

    if daemonize then
      let stdin = Daemon.null_fd () in
      let log_path = Log.rotate (Path.absolute log_path) in
      let stdout = Daemon.fd_of_path log_path in
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
    use_watchman
    verbose
    version
    expected_version
    sections
    debug
    strict
    declare
    show_error_traces
    infer
    recursive_infer
    analyze
    sequential
    number_of_workers
    log_identifier
    project_root
    search_path
    typeshed
    source_root
    () =
  (* T29256759: backward compatibility code. Prefer the new option. *)
  let expected_version =
    Option.merge
      expected_version
      version
      ~f:(fun expected _ -> expected)
  in

  let configuration =
    Configuration.create
      ~verbose
      ?expected_version
      ~sections
      ~debug
      ~infer
      ~recursive_infer
      ~analyze
      ~strict
      ~declare
      ~show_error_traces
      ~log_identifier
      ~parallel:(not sequential)
      ~number_of_workers
      ~project_root:(Path.create_absolute project_root)
      ~search_path:(List.map ~f:Path.create_absolute search_path)
      ?typeshed:(typeshed >>| Path.create_absolute)
      ~source_root:(Path.create_absolute source_root)
      ()
  in
  let log_path = log_path >>| Path.create_absolute in
  start (ServerConfiguration.create ~daemonize:(not terminal) ~use_watchman ?log_path configuration)
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
      +> flag
        "-use-watchman"
        no_arg
        ~doc:"Subscribe to watchman for file changes."
      ++ CommandSpec.base_command_line_arguments)
    run_start_command


let run_stop graceful source_root () =
  let configuration = Configuration.create ~source_root:(Path.create_absolute source_root) () in
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
    Socket.write socket ServerProtocol.Request.StopRequest;
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
    poll_for_deletion (ServerConfiguration.socket_path configuration)
    |> exit_strategy
  with
  | NotRunning
  | Unix.Unix_error _
  | ServerConfiguration.ServerNotRunning ->
      Log.warning "No servers running";
      exit_strategy 1


let stop ?(graceful=false) source_root () =
  run_stop graceful source_root ()


let stop_command =
  Command.basic_spec
    ~summary:"Stop the server"
    Command.Spec.(
      empty
      +> flag "-graceful" no_arg ~doc:"Perform a graceful exit"
      +> anon (maybe_with_default "." ("source-root" %: string)))
    run_stop
