(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Hack_parallel.Std

open Pyre

open WatchmanConstants

module Socket = PyreSocket


let subscription watchman_directory =
  let constraints =
    `List [
      `String "allof";
      `List [ `String "type"; `String "f" ];
      `List [ `String "not"; `String "empty" ];
      `List [ `String "suffix"; `String "py" ];
    ]
  in
  `List [
    `String "subscribe"; `String (Path.absolute watchman_directory);
    `String "pyre_file_change_subscription";
    `Assoc [
      "expression",
      constraints;
      "fields",
      `List [ `String "name" ];
    ];
  ]


let stop_watchman server_socket configuration =
  let pid_path = pid_path configuration in

  Unix.handle_unix_error (fun () -> server_socket >>| Unix.close |> ignore);
  (* This check is here to prevent a race where two watchman clients A and B
   * are spawned in quick succession, client A wins the race and gets to write its lock and
   * pid, and B gets a sigint. If the check weren't here, B would destroy A's lock and pid,
   * and a new client could run simultaneously with A. *)
  if Path.file_exists pid_path then
    begin
      let read_pid = In_channel.read_all (Path.absolute pid_path) |> Int.of_string in
      let my_pid = Unix.getpid () |> Pid.to_int in
      if my_pid = read_pid then begin
        Path.remove (lock_path configuration);
        Path.remove pid_path
      end
    end;
  exit 0


let recheck_threshold = 200


let build_symlink_map ~root =
  let files = File.list ~filter:(fun file -> Filename.check_suffix file ".py") ~root in
  List.fold
    ~init:Path.Map.empty
    ~f:(fun map path -> Map.set map ~key:(Path.follow_symlinks path) ~data:path)
    files


let set_symlink ~root ~symlinks ~path =
  if not (Path.Map.mem symlinks path) &&
     Path.directory_contains ~directory:root path then
    Map.set symlinks ~key:(Path.follow_symlinks path) ~data:path
  else
    symlinks


let process_response ~root ~watchman_directory ~symlinks response =
  let open Yojson.Safe in
  Log.info "Processing response %s" response;
  let response = from_string response in
  let keys = Util.keys response in
  if List.mem ~equal:String.equal keys "files" then
    (* If a list of updated files exists, default to typechecking. *)
    response
    |> Util.member "files"
    |> Util.to_list
    |> Util.filter_string
    |> fun files ->
    Log.info "Updated files: %a" Sexp.pp (sexp_of_list sexp_of_string files);
    if List.length files > recheck_threshold then
      begin
        Log.info "Detected a significant number of file changes, rechecking repository.";
        Some (build_symlink_map ~root, Protocol.Request.ReinitializeStateRequest)
      end
    else
      let relativize_to_root path =
        match Path.get_relative_to_root ~root ~path with
        | None -> path
        | Some relative -> Path.create_relative ~root ~relative
      in
      let paths =
        List.map ~f:(fun relative -> Path.create_relative ~root:watchman_directory ~relative) files
        |> List.map ~f:relativize_to_root
      in
      let symlinks =
        List.fold ~init:symlinks ~f:(fun symlinks path -> set_symlink ~symlinks ~root ~path) paths
      in
      let files =
        List.filter_map
          ~f:(fun path ->
              Map.find symlinks path
              >>| File.create)
          paths
      in
      Some (symlinks, Protocol.Request.TypeCheckRequest { Protocol.files; check_dependents = true })
  else
    None


(** Main entry called whether spawned as a daemon or not *)
let listen_for_changed_files
    server_socket
    watchman_directory
    ({ Configuration.source_root; _ } as configuration) =
  let symlinks = build_symlink_map ~root:source_root in
  let socket_path =
    let open Yojson.Safe in
    Unix.open_process_in "watchman get-sockname --no-pretty"
    |> from_channel
    |> Util.member "sockname"
    |> Util.to_string
  in
  Log.info "Watchman socket path is %s" socket_path;
  let in_channel, out_channel = Unix.open_connection (Unix.ADDR_UNIX socket_path) in
  Out_channel.output_lines
    out_channel
    [Yojson.to_string (subscription watchman_directory)];
  Out_channel.flush out_channel;
  ignore
    (In_channel.input_line in_channel >>= fun subscriber_response ->
     Log.info "Watchman subscriber response: %s" subscriber_response;
     (* Throw away the the first update that includes all the files *)
     In_channel.input_line in_channel >>= fun _ ->
     let rec loop symlinks =
       try
         In_channel.input_line in_channel
         >>= process_response ~root:source_root ~watchman_directory ~symlinks
         >>| (fun (symlinks, response) ->
             Log.info "Writing response %s" (Protocol.Request.show response);
             Socket.write server_socket response;
             symlinks)
         |> Option.value ~default:symlinks
         |> loop
       with
       | End_of_file ->
           Log.info "A socket was closed.";
           stop_watchman (Some server_socket) configuration
     in
     loop symlinks)


(* Walk up from the project root to try and find a .watchmanconfig. *)
let find_watchman_directory { Configuration.source_root; _ } =
  let rec directory_has_watchman_config directory =
    if Sys.is_file (directory ^/ ".watchmanconfig") = `Yes then
      Some (Path.create_absolute directory)
    else if Filename.dirname directory = directory then
      None
    else
      directory_has_watchman_config (Filename.dirname directory)
  in
  directory_has_watchman_config (Path.absolute source_root)

let initialize watchman_directory configuration =
  Log.info
    "Watchman subscription: %s"
    (Yojson.pretty_to_string (subscription watchman_directory));
  let server_socket =
    try
      Server.connect ~retries:5 ~configuration
    with
    | Server.ConnectionFailure -> stop_watchman None configuration
  in
  Socket.write server_socket (Protocol.Request.ClientConnectionRequest Protocol.FileNotifier);
  if Socket.read server_socket <> Protocol.ClientConnectionResponse Protocol.FileNotifier then
    failwith "Unexpected connection response from server";
  Signal.Expert.handle Signal.int (fun _ -> stop_watchman (Some server_socket) configuration);
  server_socket


type run_watchman_daemon_entry =
  (Configuration.t, unit Daemon.in_channel, unit Daemon.out_channel) Daemon.entry

let setup configuration pid =
  if Lock_file.create ~unlink_on_exit:true (Path.absolute (lock_path configuration)) <> true then
    failwith "Watchman client exists (lock is held). Exiting.";
  Out_channel.with_file
    (pid_path configuration |> Path.absolute)
    ~f:(fun out_channel ->
        Format.fprintf (Format.formatter_of_out_channel out_channel) "%d%!" pid)

let run_watchman_daemon_entry : run_watchman_daemon_entry =
  Daemon.register_entry_point
    "watchman_daemon"
    (fun configuration (parent_in_channel, parent_out_channel) ->
       Daemon.close_in parent_in_channel;
       Daemon.close_out parent_out_channel;
       (* Detach from a controlling terminal *)
       Unix.Terminal_io.setsid () |> ignore;
       let watchman_directory = find_watchman_directory configuration in
       match watchman_directory with
       | None ->
           exit 1
       | Some watchman_directory ->
           let server_socket = initialize watchman_directory configuration in
           listen_for_changed_files server_socket watchman_directory configuration)


let run_command daemonize verbose sections _ source_root () =
  let source_root = Path.create_absolute source_root in
  let configuration = Configuration.create ~source_root:source_root () in
  Log.initialize ~verbose ~sections;
  Unix.handle_unix_error (fun () -> Unix.mkdir_p (watchman_root configuration |> Path.absolute));
  if Lock_file.is_locked (Path.absolute (lock_path configuration)) then
    failwith "Watchman client exists (lock is held). Exiting.";

  if daemonize then
    begin
      let stdin = Daemon.null_fd () in
      let stdout = Daemon.fd_of_path (Path.absolute (log_path configuration)) in
      Log.info "Spawning the watchman daemon now.";
      let { Daemon.pid; _ } as handle =
        Daemon.spawn
          (stdin, stdout, stdout)
          run_watchman_daemon_entry
          configuration
      in
      Daemon.close handle;
      setup configuration pid;
      Log.info "Watchman daemon pid: %d" pid
    end
  else
    begin
      let watchman_directory = find_watchman_directory configuration in
      match watchman_directory with
      | None -> exit 1
      | Some watchman_directory ->
          begin
            let server_socket = initialize watchman_directory configuration in
            let pid = Unix.getpid () |> Pid.to_int in
            setup configuration pid;
            listen_for_changed_files server_socket watchman_directory configuration
          end
    end


let command =
  Command.basic_spec
    ~summary:"Starts a watchman listener in the current directory. \
              Starts a server in the current directory if it does not exist."
    Command.Spec.(
      empty
      +> flag "-daemonize" no_arg ~doc:"Run the watchman client in the background"
      +> flag "-verbose" no_arg ~doc:"Turn on verbose logging"
      +> flag
        "-logging-sections"
        (optional_with_default [] (Arg_type.comma_separated string))
        ~doc:"Comma-separated list of logging sections."
      +> flag
        "-project-root"
        (optional string)
        ~doc:"Only follow sources under this root directory."
        ~aliases:["-type-check-root"]
      +> anon (maybe_with_default "." ("source-root" %: string)))
    run_command
