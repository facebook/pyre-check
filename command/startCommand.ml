(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open Core

open Pyre

open Network
open Scheduler
open Server
open State
open Protocol

module Time = Core_kernel.Time_ns.Span
module Request = Server.Request

exception AlreadyRunning


let spawn_watchman_client
    {
      Configuration.Server.configuration = { sections; project_root; local_root; search_path; _ };
      _;
    } =
  WatchmanCommand.run_command
    ~daemonize:true
    ~verbose:false
    ~sections
    ~local_root:(Path.absolute local_root)
    ~search_path
    ~project_root:(Some (Path.absolute project_root))


let computation_thread request_queue configuration state =
  let failure_threshold = 5 in
  let rec loop
      ({
        Configuration.Server.pid_path;
        configuration = analysis_configuration;
        _;
      }
       as configuration)
      state =
    let errors_to_lsp_responses error_map =
      let diagnostic_to_response = function
        | Ok diagnostic_error ->
            [
              diagnostic_error
              |> LanguageServer.Protocol.PublishDiagnostics.to_yojson
              |> Yojson.Safe.to_string
              |> fun serialized_diagnostic -> LanguageServerProtocolResponse serialized_diagnostic;
            ]
        | Error _ ->
            []
      in
      error_map
      |> List.map
        ~f:(fun (handle, errors) ->
            LanguageServer.Protocol.PublishDiagnostics.of_errors
              ~configuration:analysis_configuration
              handle
              errors)
      |> List.concat_map ~f:diagnostic_to_response
    in
    (* Decides what to broadcast to persistent clients after a request is processed. *)
    let broadcast_response lock persistent_clients response =
      match response with
      | TypeCheckResponse error_map ->
          let responses = errors_to_lsp_responses error_map in
          let write_or_mark_failure responses ~key:socket ~data:failures =
            try
              List.iter ~f:(Socket.write socket) responses;
              failures
            with Unix.Unix_error (Unix.EPIPE, _, _) ->
              Log.warning "Got an EPIPE while broadcasting to a persistent client";
              failures + 1
          in
          Mutex.critical_section lock ~f:(fun () ->
              Hashtbl.mapi_inplace ~f:(write_or_mark_failure responses) persistent_clients;
              Hashtbl.filter_inplace
                ~f:(fun failures -> failures < failure_threshold)
                persistent_clients)
      | _ ->
          ()
    in
    let rec handle_request ?(retries = 2) state ~request:(origin, request) =
      let process socket state configuration request =
        Log.log ~section:`Server "Processing request %a" Protocol.Request.pp request;
        Request.process ~socket ~state ~configuration ~request
      in
      try
        match origin with
        | Protocol.Request.PersistentSocket socket ->
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
                            | Some failures ->
                                if failures < failure_threshold then
                                  Hashtbl.set
                                    persistent_clients
                                    ~key:socket
                                    ~data:(failures + 1)
                                else
                                  Hashtbl.remove persistent_clients socket
                            | None ->
                                ())
                    | _ ->
                        ()
                  end
            in
            let { Request.state; response } = process socket state configuration request in
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
        | Protocol.Request.FileNotifier
        | Protocol.Request.Background ->
            let { socket; persistent_clients; _ } =
              Mutex.critical_section state.lock ~f:(fun () -> !(state.connections))
            in
            let { Request.state; response } = process socket state configuration request in
            begin
              match response with
              | Some response ->
                  broadcast_response state.lock persistent_clients response
              | None ->
                  ()
            end;
            state
        | Protocol.Request.NewConnectionSocket socket ->
            let { persistent_clients; _ } =
              Mutex.critical_section state.lock ~f:(fun () -> !(state.connections))
            in
            let { Request.state; response } = process socket state configuration request in
            begin
              match response with
              | Some response ->
                  Socket.write_ignoring_epipe socket response;
                  broadcast_response state.lock persistent_clients response
              | None -> ()
            end;
            state
      with uncaught_exception ->
        if retries > 0 then
          handle_request ~retries:(retries - 1) state ~request:(origin, request)
        else
          raise uncaught_exception
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
          begin
            match state.deferred_requests with
            | [] ->
                state
            | request :: requests ->
                let state = { state with deferred_requests = requests } in
                handle_request state ~request:(Protocol.Request.Background, request)
          end
      | 0, true ->
          (* Stop if the server is idle. *)
          let current_time = Unix.time () in
          let stop_after_idle_for = 24.0 *. 60.0 *. 60.0 (* 1 day *) in
          if current_time -. state.last_request_time > stop_after_idle_for then
            begin
              Mutex.critical_section
                state.lock
                ~f:(fun () ->
                    Operations.stop
                      ~reason:"idle"
                      ~configuration
                      ~socket:!(state.connections).socket)
            end;

          (* Stop if there's any inconsistencies in the .pyre directory. *)
          let last_integrity_check =
            let integrity_check_every = 60.0 (* 1 minute *) in
            if current_time -. state.last_integrity_check > integrity_check_every then
              begin
                try
                  let pid =
                    let pid_file =
                      Path.absolute pid_path
                      |> In_channel.create
                    in
                    protect
                      ~f:(fun () -> In_channel.input_all pid_file)
                      ~finally:(fun () -> In_channel.close pid_file)
                  in
                  if not (Pid.to_string (Unix.getpid ()) = pid) then
                    raise (Failure "pid mismatch");
                  current_time
                with _ ->
                  Mutex.critical_section
                    state.lock
                    ~f:(fun () ->
                        Operations.stop
                          ~reason:"failed integrity check"
                          ~configuration
                          ~socket:!(state.connections).socket;
                        current_time)
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
      Configuration.Server.configuration = ({ expected_version; local_root; _ });
      use_watchman;
      watchman_creation_timeout;
      _;
    } as server_configuration,
      lock,
      connections,
      request_queue) =
  let queue_request ~origin request =
    match request, origin with
    | Protocol.Request.StopRequest, Protocol.Request.NewConnectionSocket socket ->
        Socket.write socket StopResponse;
        Operations.stop
          ~reason:"explicit request"
          ~configuration:server_configuration
          ~socket:!(connections).socket
    | Protocol.Request.StopRequest, _ ->
        Operations.stop
          ~reason:"explicit request"
          ~configuration:server_configuration
          ~socket:!(connections).socket
    | Protocol.Request.ClientConnectionRequest client,
      Protocol.Request.NewConnectionSocket socket ->
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
                      Hashtbl.set persistent_clients ~key:socket ~data:0;
                      !connections
                  | FileNotifier ->
                      { !connections with file_notifiers = socket::file_notifiers }
                end)
    | Protocol.Request.ClientConnectionRequest _, _ ->
        Log.error
          "Unexpected request origin %s for connection request"
          (Protocol.Request.origin_name origin)
    | _ ->
        Squeue.push_or_drop request_queue (origin, request) |> ignore;
  in
  let handle_readable_persistent socket =
    try
      Log.log ~section:`Server "A persistent client socket is readable.";
      let request = Socket.read socket in
      queue_request ~origin:(Protocol.Request.PersistentSocket socket) request
    with
    | End_of_file
    | Unix.Unix_error (Unix.ECONNRESET, _, _) ->
        Log.log ~section:`Server "Persistent client disconnected";
        Mutex.critical_section lock
          ~f:(fun () ->
              let persistent_clients = !(connections).persistent_clients in
              Hashtbl.remove persistent_clients socket)
  in
  let handle_readable_file_notifier last_watchman_created socket =
    try
      Log.log ~section:`Server "A file notifier is readable.";
      let request = Socket.read socket in
      queue_request ~origin:Protocol.Request.FileNotifier request
    with
    | End_of_file
    | Unix.Unix_error (Unix.ECONNRESET, _, _) ->
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
              let old_watchman_pid = !(connections).watchman_pid in
              let wait_on_watchman pid =
                Log.log ~section:`Server "Waiting on watchman pid %d" (Pid.to_int pid);
                match Unix.wait_nohang (`Pid pid) with
                | None ->
                    Log.log ~section:`Server "Unable to wait on pid %d" (Pid.to_int pid)
                | Some _ ->
                    Log.log ~section:`Server "pid %d successfully reaped." (Pid.to_int pid)
              in
              Option.iter ~f:wait_on_watchman old_watchman_pid;

              let exceeds_timeout () =
                let current_time = Unix.time () in
                current_time -. (!last_watchman_created) >= watchman_creation_timeout
              in
              let watchman_pid =
                if List.is_empty file_notifiers && use_watchman && (exceeds_timeout ()) then
                  begin
                    last_watchman_created := Unix.time ();
                    Some (spawn_watchman_client server_configuration)
                  end
                else
                  None
              in
              connections := { !connections with file_notifiers; watchman_pid });
  in
  let last_watchman_created = ref 0.0 in
  let rec loop () =
    let { socket = server_socket; persistent_clients; file_notifiers; _ } =
      Mutex.critical_section lock ~f:(fun () -> !connections)
    in
    if not (PyrePath.is_directory local_root) then
      begin
        Log.error "Stopping server due to missing source root.";
        Operations.stop
          ~reason:"missing source root"
          ~configuration:server_configuration
          ~socket:!(connections).socket
      end;
    let readable =
      Unix.select
        ~restart:true
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
          try
            Socket.write
              new_socket
              (Handshake.ServerConnected (Option.value ~default:"-1" expected_version));
            Socket.read new_socket
            |> fun Handshake.ClientConnected ->
            let request = Socket.read new_socket in
            queue_request ~origin:(Protocol.Request.NewConnectionSocket new_socket) request
          with
          | Unix.Unix_error (Unix.EPIPE, _, _) ->
              Log.warning "EPIPE while writing to socket."
          | Unix.Unix_error (Unix.ECONNRESET, _, _) ->
              Log.warning "ECONNRESET while reading from socket."
          | End_of_file ->
              Log.warning "New client socket unreadable"
        end
      else if Mutex.critical_section lock ~f:(fun () -> Hashtbl.mem persistent_clients socket) then
        handle_readable_persistent socket
      else
        handle_readable_file_notifier last_watchman_created socket
    in
    List.iter ~f:handle_socket readable;
    loop ()
  in
  try
    loop ()
  with uncaught_exception ->
    Statistics.log_exception uncaught_exception ~fatal:true ~origin:"server";
    Operations.stop
      ~reason:"exception"
      ~configuration:server_configuration
      ~socket:(!connections).socket


(** Main server either as a daemon or in terminal *)
let serve
    ~socket
    ~server_configuration:({ Configuration.Server.configuration; _ } as server_configuration)
    ~watchman_pid =
  Version.log_version_banner ();
  (fun () ->
     Log.log ~section:`Server "Starting daemon server loop...";
     Configuration.Server.set_global server_configuration;
     let request_queue = Squeue.create 25 in
     let lock = Mutex.create () in
     let connections =
       ref {
         socket;
         persistent_clients = Unix.File_descr.Table.create ();
         file_notifiers = [];
         watchman_pid;
       }
     in

     (* Register signal handlers. *)
     Signal.Expert.handle
       Signal.int
       (fun _ -> Operations.stop ~reason:"interrupt" ~configuration:server_configuration ~socket);
     Signal.Expert.handle
       Signal.pipe
       (fun _ -> ());

     Thread.create
       request_handler_thread
       (server_configuration, lock, connections, request_queue)
     |> ignore;

     let state = Operations.start ~lock ~connections ~configuration:server_configuration () in
     try
       computation_thread request_queue server_configuration state
     with uncaught_exception ->
       Statistics.log_exception uncaught_exception ~fatal:true ~origin:"server";
       Operations.stop ~reason:"exception" ~configuration:server_configuration ~socket)
  |> Scheduler.run_process ~configuration


(* Create lock file and pid file. Used for both daemon mode and in-terminal *)
let acquire_lock ~server_configuration:{ Configuration.Server.lock_path; pid_path; _ } =
  let pid = Unix.getpid () |> Pid.to_int in
  if not (Lock.grab (Path.absolute lock_path)) then
    raise AlreadyRunning;
  Out_channel.with_file
    (Path.absolute pid_path)
    ~f:(fun out_channel -> Format.fprintf (Format.formatter_of_out_channel out_channel) "%d%!" pid)


(** Daemon forking code *)
type run_server_daemon_entry =
  ((Socket.t * Configuration.Server.t * Pid.t option),
   unit Daemon.in_channel,
   unit Daemon.out_channel) Daemon.entry


(** When spawned, a child is passed input/output channels that can communicate
    with the parent process. We don't currently use these, so close them after
    spawning. *)
let run_server_daemon_entry : run_server_daemon_entry =
  Daemon.register_entry_point
    "server_daemon"
    (fun (socket, server_configuration, watchman_pid) (parent_in_channel, parent_out_channel)  ->
       Daemon.close_in parent_in_channel;
       Daemon.close_out parent_out_channel;
       (* Detach the from a controlling terminal *)
       Unix.Terminal_io.setsid () |> ignore;
       acquire_lock ~server_configuration;
       serve ~socket ~server_configuration ~watchman_pid)


let run ({
    Configuration.Server.lock_path;
    socket_path;
    log_path;
    daemonize;
    configuration;
    use_watchman;
    _;
  } as server_configuration) =
  (fun () ->
     try
       Log.info "Starting up server...";
       if daemonize then
         Version.log_version_banner ();

       if not (Lock.check (Path.absolute lock_path)) then
         raise AlreadyRunning;

       Log.log ~section:`Server "Creating server socket at `%a`" Path.pp socket_path;
       let socket = Socket.initialize_unix_socket socket_path in

       let watchman_pid =
         if use_watchman then
           let pid = spawn_watchman_client server_configuration in
           (* If the server is being run as a daemon, the watchman pid will be detached from
              the server pid and shouldn't be waited on. *)
           if not daemonize then
             Some pid
           else
             None
         else
           None
       in
       if daemonize then
         let stdin = Daemon.null_fd () in
         let log_path = Log.rotate (Path.absolute log_path) in
         let stdout = Daemon.fd_of_path log_path in
         Log.log ~section:`Server "Spawning the daemon now.";
         let { Daemon.pid; _ } as handle =
           Daemon.spawn
             (stdin, stdout, stdout)
             run_server_daemon_entry (socket, server_configuration, watchman_pid)
         in
         Daemon.close handle;
         Log.log ~section:`Server "Forked off daemon with pid %d" pid;
         Log.info "Server starting in background";
         pid
       else
         begin
           acquire_lock ~server_configuration;
           serve ~socket ~server_configuration ~watchman_pid;
           0
         end
     with AlreadyRunning ->
       Log.info "Server is already running";
       0)
  |> Scheduler.run_process ~configuration


(** Default configuration when run from command line *)
let run_start_command
    log_path
    terminal
    use_watchman
    load_state_from
    save_state_to
    changed_files_path
    saved_state_project
    verbose
    expected_version
    sections
    debug
    strict
    declare
    show_error_traces
    infer
    recursive_infer
    run_additional_checks
    sequential
    filter_directories
    number_of_workers
    log_identifier
    logger
    project_root
    search_path
    typeshed
    excludes
    local_root
    () =
  let filter_directories =
    filter_directories
    >>| String.split_on_chars ~on:[';']
    >>| List.map ~f:String.strip
    >>| List.map ~f:Path.create_absolute
  in
  let configuration =
    Configuration.Analysis.create
      ~verbose
      ?expected_version
      ~sections
      ~debug
      ~infer
      ~recursive_infer
      ~run_additional_checks
      ~strict
      ~declare
      ~show_error_traces
      ~log_identifier
      ?logger
      ~parallel:(not sequential)
      ?filter_directories
      ~number_of_workers
      ~project_root:(Path.create_absolute project_root)
      ~search_path:(List.map search_path ~f:Path.SearchPath.create)
      ?typeshed:(typeshed >>| Path.create_absolute)
      ~excludes
      ~local_root:(Path.create_absolute local_root)
      ()
  in
  let log_path = log_path >>| Path.create_absolute in
  let saved_state_action =
    match save_state_to, saved_state_project with
    | Some path, _ ->
        Some (Configuration.Server.Save path)
    | None, Some project_name ->
        Some (Configuration.Server.Load (Configuration.Server.LoadFromProject project_name))
    | None, None ->
        match load_state_from, changed_files_path with
        | Some shared_memory_path, Some changed_files_path ->
            Some (Load (Configuration.Server.LoadFromFiles {
                Configuration.Server.shared_memory_path = Path.create_absolute shared_memory_path;
                changed_files_path = Path.create_absolute changed_files_path;
              }))
        | Some _, None ->
            Log.error "-changed-files-path must be set when -load-state-from is passed in.";
            exit 1
        | None, Some _ ->
            Log.error "-load-state-from must be set when -changed-files-path is passed in.";
            exit 1
        | _ ->
            None
  in
  run
    (Operations.create_configuration
       ~daemonize:(not terminal)
       ~use_watchman
       ?log_path
       ?saved_state_action
       configuration)
  |> ignore


let command =
  Command.basic_spec
    ~summary:"Starts a server in the foreground by default. See help for daemon options."
    Command.Spec.(
      empty
      +> flag
        "-log-file"
        (optional file)
        ~doc:(Format.sprintf "filename Log file (Default is ./pyre/server/server.stdout)")
      +> flag
        "-terminal"
        no_arg
        ~doc:"Run the server from the terminal instead of running as a daemon."
      +> flag
        "-use-watchman"
        no_arg
        ~doc:"Subscribe to watchman for file changes."
      +> flag
        "-load-state-from"
        (optional string)
        ~doc:"The Pyre server will start from the specified path if one is passed in."
      +> flag
        "-save-initial-state-to"
        (optional string)
        ~doc:"The Pyre server will save its initial state to the path passed in by this argument."
      +> flag
        "-changed-files-path"
        (optional string)
        ~doc:"Pyre will reanalyze the paths listed in path if started from a saved state."
      +> flag
        "-saved-state-project"
        (optional string)
        ~doc:"Pyre will attempt to fetch the project's saved state from the project name."
      ++ Specification.base_command_line_arguments)
    run_start_command
