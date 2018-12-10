(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre
open Network
open Scheduler
open Server

open Service.Constants.Watchman

module Time = Core_kernel.Time_ns.Span


type state = {
  configuration: Configuration.Analysis.t;
  watchman_directory: Path.t;
  symlinks: Path.t Path.Map.t;
}


let subscription watchman_directory =
  let constraints =
    `List [
      `String "allof";
      `List [ `String "type"; `String "f" ];
      `List [ `String "not"; `String "empty" ];
      `List [
        (* Do not use 'suffix-set', it is watchman-5.0+ only. *)
        `String "anyof";
        `List [ `String "suffix"; `String "py" ];
        `List [ `String "suffix"; `String "pyi" ];
      ];
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

  Unix.handle_unix_error (fun () -> Option.iter server_socket ~f:Unix.close);
  (* This check is here to prevent a race where two watchman clients A and B
   * are spawned in quick succession, client A wins the race and gets to write its lock and
   * pid, and B gets a sigint. If the check weren't here, B would destroy A's lock and pid,
   * and a new client could run simultaneously with A. *)
  if Path.file_exists pid_path then
    begin
      let read_pid =
        In_channel.read_all (Path.absolute pid_path)
        |> Int.of_string
      in
      let my_pid =
        Unix.getpid ()
        |> Pid.to_int
      in
      if my_pid = read_pid then
        begin
          Path.remove (lock_path configuration);
          Path.remove pid_path
        end
    end;
  exit 0


let recheck_threshold = 200


let build_symlink_map files =
  let add_symlink map path =
    try
      let key = Path.real_path path in
      Map.set map ~key ~data:path
    with Unix.Unix_error (error, name, parameters) ->
      Log.log_unix_error ~section:`Warning (error, name, parameters);
      map
  in
  List.fold ~init:Path.Map.empty ~f:add_symlink files


let set_symlink ({ configuration; symlinks; _ } as state) ~path =
  let symlinks =
    try
      let tracked path =
        Configuration.Analysis.search_path configuration
        |> List.map ~f:Path.SearchPath.to_path
        |> List.exists ~f:(fun directory -> Path.directory_contains ~directory path)
      in
      if not (Path.Map.mem symlinks path) && tracked path then
        Map.set symlinks ~key:(Path.real_path path) ~data:path
      else
        symlinks
    with Unix.Unix_error (error, name, parameters) ->
      (* Ensure that removed file notifications don't crash the watchman client. *)
      Log.log_unix_error ~section:`Warning (error, name, parameters);
      symlinks
  in
  { state with symlinks }


let process_response
    ({
      configuration = { Configuration.Analysis.local_root = root; _ };
      watchman_directory;
      _;
    } as state)
    serialized_response =
  let open Yojson.Safe in
  let response =
    try
      let response = from_string serialized_response in
      let keys = Util.keys response in
      if List.mem ~equal:String.equal keys "files" then
        Some response
      else
        None
    with Yojson.Json_error error ->
      Log.error "Json_error: %s" error;
      Log.info "Erroring watchman response: %s" serialized_response;
      None
  in
  response >>| fun response ->
  Log.info "Processing response %s" serialized_response;
  (* If a list of updated files exists, default to typechecking. *)
  response
  |> Util.member "files"
  |> Util.to_list
  |> Util.filter_string
  |> fun files ->
  Log.info "Updated files: %a" Sexp.pp [%message (files: string list)];
  let relativize_to_root path =
    match Path.get_relative_to_root ~root ~path with
    | None -> path
    | Some relative -> Path.create_relative ~root ~relative
  in
  let paths =
    List.map
      ~f:(fun relative -> Path.create_relative ~root:watchman_directory ~relative)
      files
    |> List.map ~f:relativize_to_root
  in
  let ({ symlinks; _ } as state) =
    List.fold
      paths
      ~init:state
      ~f:(fun state path -> set_symlink state ~path)
  in
  let files =
    List.filter_map
      ~f:(fun path ->
          Map.find symlinks path
          >>| File.create)
      paths
  in
  let is_stub file = String.is_suffix ~suffix:"pyi" (File.path file |> Path.absolute) in
  (state,
   Protocol.Request.TypeCheckRequest
     (Protocol.TypeCheckRequest.create
        ~update_environment_with:files
        ~check:(List.filter ~f:(fun file -> not (is_stub file)) files) ()))


(** Main entry called whether spawned as a daemon or not *)
let listen_for_changed_files
    server_socket
    watchman_directory
    ({ Configuration.Analysis.local_root; _ } as configuration) =
  try
    (fun () ->
       let symlinks =
         Path.list ~file_filter:(fun file -> Filename.check_suffix file ".py") ~root:local_root ()
         |> build_symlink_map
       in
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
       let watchman_socket = Unix.descr_of_in_channel in_channel in
       let rec loop state =
         try
           let ready =
             Unix.select
               ~read:[watchman_socket; server_socket]
               ~write:[]
               ~except:[]
               ~timeout:(`After (Time.of_int_sec 5))
               ()
             |> fun { Unix.Select_fds.read; _ } -> read
           in
           let handle_socket state socket =
             let handle_watchman state socket =
               let in_channel = Unix.in_channel_of_descr socket in
               In_channel.input_line in_channel
               >>= process_response state
               >>| (fun (state, response) ->
                   Log.info "Writing response %s" (Protocol.Request.show response);
                   Socket.write server_socket response;
                   state)
               |> Option.value ~default:state
             in

             if socket = watchman_socket then
               handle_watchman state socket
             else
               begin
                 Socket.read server_socket |> ignore;
                 state
               end
           in
           let state = List.fold ~init:state ~f:handle_socket ready in
           loop state
         with
         | End_of_file ->
             Log.info "A socket was closed.";
             stop_watchman (Some server_socket) configuration
       in
       begin
         In_channel.input_line in_channel
         >>= fun subscriber_response ->
         Log.info "Watchman subscriber response: %s" subscriber_response;
         (* Throw away the the first update that includes all the files *)
         In_channel.input_line in_channel
         >>= fun _ ->
         loop { configuration; watchman_directory; symlinks }
       end
       |> ignore)
    |> Scheduler.run_process ~configuration
  with uncaught_exception ->
    Statistics.log_exception uncaught_exception ~fatal:true ~origin:"watchman";
    raise uncaught_exception


let initialize watchman_directory configuration =
  Version.log_version_banner ();
  Log.info
    "Watchman subscription: %s"
    (Yojson.pretty_to_string (subscription watchman_directory));
  let server_socket =
    try
      Server.Operations.connect ~retries:5 ~configuration
    with
    | Server.Operations.ConnectionFailure -> stop_watchman None configuration
  in
  Socket.write server_socket (Protocol.Request.ClientConnectionRequest Protocol.FileNotifier);
  if Socket.read server_socket <> Protocol.ClientConnectionResponse Protocol.FileNotifier then
    failwith "Unexpected connection response from server";
  let stop _ = stop_watchman (Some server_socket) configuration in
  Signal.Expert.handle Signal.int stop;
  Signal.Expert.handle Signal.pipe stop;
  server_socket


type run_watchman_daemon_entry =
  (Configuration.Analysis.t, unit Daemon.in_channel, unit Daemon.out_channel) Daemon.entry


let setup configuration pid =
  if not (Lock.grab (Path.absolute (lock_path configuration))) then
    failwith "Watchman client exists (lock is held). Exiting.";
  Out_channel.with_file
    (pid_path configuration |> Path.absolute)
    ~f:(fun out_channel ->
        Format.fprintf (Format.formatter_of_out_channel out_channel) "%d%!" pid)


let run_watchman_daemon_entry : run_watchman_daemon_entry =
  Daemon.register_entry_point
    "watchman_daemon"
    (fun
      ({ Configuration.Analysis.project_root; _ } as configuration)
      (parent_in_channel, parent_out_channel) ->
      Daemon.close_in parent_in_channel;
      Daemon.close_out parent_out_channel;
      (* Detach from a controlling terminal *)
      Unix.Terminal_io.setsid () |> ignore;
      setup configuration (Unix.getpid () |> Pid.to_int);
      let watchman_directory = Path.search_upwards ~target:".watchmanconfig" ~root:project_root in
      match watchman_directory with
      | None ->
          exit 1
      | Some watchman_directory ->
          let server_socket = initialize watchman_directory configuration in
          listen_for_changed_files server_socket watchman_directory configuration)

let run_command ~daemonize ~verbose ~sections ~local_root ~search_path ~project_root =
  let local_root = Path.create_absolute local_root in
  let project_root =
    project_root
    >>| Path.create_absolute
    |> Option.value ~default:local_root
  in
  let configuration =
    Configuration.Analysis.create
      ~verbose
      ~sections
      ~local_root
      ~search_path
      ~project_root
      ()
  in
  (* Warn if watchman isn't picking up on changes in the current working directory. *)
  let () =
    let input =
      let channel = Unix.open_process_in "watchman watch-list" in
      protect
        ~f:(fun () -> In_channel.input_all channel)
        ~finally:(fun () -> In_channel.close channel)
    in
    let watchman_watches_root =
      let directory_contains_root directory =
        Path.directory_contains ~directory:(Path.create_absolute directory) project_root
      in
      try
        let watch_list = Yojson.Safe.from_string input in
        Yojson.Safe.Util.member "roots" watch_list
        |> Yojson.Safe.Util.to_list
        |> List.map ~f:Yojson.Safe.Util.to_string
        |> List.exists ~f:directory_contains_root
      with Yojson.Json_error _ ->
        false
    in
    if not watchman_watches_root then
      begin
        Log.info "watchman watch-list output: %s" input;
        Log.warning
          "Unable to find `%s` in watchman's watched directories, type errors might be inaccurate."
          (Path.absolute local_root);
        Log.warning
          "Documentation to integrate watchman is available at `%s`."
          "https://pyre-check.org/docs/watchman-integration.html";
      end;
  in
  Unix.handle_unix_error
    (fun () -> Unix.mkdir_p (watchman_root configuration |> Path.absolute));
  if not (Lock.check (Path.absolute (lock_path configuration))) then
    failwith "Watchman client exists (lock is held). Exiting.";

  if daemonize then
    begin
      let stdin = Daemon.null_fd () in
      let log_path = Log.rotate (Path.absolute (log_path configuration)) in
      let stdout = Daemon.fd_of_path log_path in
      Log.debug "Spawning the watchman daemon now.";
      let { Daemon.pid; _ } as handle =
        Daemon.spawn
          (stdin, stdout, stdout)
          run_watchman_daemon_entry
          configuration
      in
      Daemon.close handle;
      Log.debug "Watchman daemon pid: %d" pid;
      Pid.of_int pid
    end
  else
    let watchman_directory =
      Path.search_upwards ~target:".watchmanconfig" ~root:project_root
    in
    match watchman_directory with
    | None -> exit 1
    | Some watchman_directory ->
        let server_socket = initialize watchman_directory configuration in
        let pid = Unix.getpid () |> Pid.to_int in
        setup configuration pid;
        listen_for_changed_files server_socket watchman_directory configuration;
        Unix.getpid ()


let run daemonize verbose sections search_path project_root local_root () =
  run_command
    ~daemonize
    ~verbose
    ~sections
    ~local_root
    ~project_root
    ~search_path:(List.map search_path ~f:Path.SearchPath.create)
  |> ignore


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
        ~doc:"SECTION1,... Comma-separated list of logging sections."
      +> flag
        "-search-path"
        (optional_with_default [] (Arg_type.comma_separated string))
        ~doc:"DIRECTORY1,... Directories containing external modules to include."
      +> flag
        "-project-root"
        (optional string)
        ~doc:"ROOT Only follow sources under this root directory."
      +> anon (maybe_with_default "." ("source-root" %: string)))
    run
