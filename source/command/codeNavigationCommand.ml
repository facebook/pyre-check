(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core

module CodeNavigationConfiguration = struct
  type t = {
    base: CommandStartup.BaseConfiguration.t;
    socket_path: PyrePath.t;
    additional_logging_sections: string list;
    watchman_root: PyrePath.t option;
    critical_files: Server.CriticalFile.t list;
  }
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open Yojson.Safe.Util in
    let open JsonParsing in
    try
      match CommandStartup.BaseConfiguration.of_yojson json with
      | Result.Error _ as error -> error
      | Result.Ok base ->
          let critical_file_list_member =
            let to_critical_file json =
              Server.CriticalFile.of_yojson json |> Result.ok_or_failwith
            in
            list_member ~f:to_critical_file
          in

          let socket_path = json |> path_member "socket_path" in
          let watchman_root = json |> optional_path_member "watchman_root" in
          let critical_files = json |> critical_file_list_member "critical_files" ~default:[] in
          let additional_logging_sections =
            json |> string_list_member "additional_logging_sections" ~default:[]
          in
          Result.Ok
            { base; socket_path; watchman_root; critical_files; additional_logging_sections }
    with
    | Type_error (message, _)
    | Undefined (message, _) ->
        Result.Error message
    | other_exception -> Result.Error (Exn.to_string other_exception)


  let analysis_configuration_of
      {
        base =
          {
            CommandStartup.BaseConfiguration.source_paths;
            search_paths;
            excludes;
            checked_directory_allowlist;
            checked_directory_blocklist;
            extensions;
            log_path;
            global_root;
            local_root;
            debug;
            enable_type_comments;
            python_version = { Configuration.PythonVersion.major; minor; micro };
            parallel;
            number_of_workers;
            enable_readonly_analysis;
            enable_unawaited_awaitable_analysis;
            shared_memory =
              { Configuration.SharedMemory.heap_size; dependency_table_power; hash_table_power };
            remote_logging = _;
            profiling_output = _;
            memory_profiling_output = _;
          };
        socket_path = _;
        watchman_root = _;
        critical_files = _;
        additional_logging_sections = _;
      }
    =
    Configuration.Analysis.create
      ~parallel
      ~analyze_external_sources:false
      ~filter_directories:checked_directory_allowlist
      ~ignore_all_errors:checked_directory_blocklist
      ~number_of_workers
      ~local_root:(Option.value local_root ~default:global_root)
      ~project_root:global_root
      ~search_paths:(List.map search_paths ~f:SearchPath.normalize)
      ~debug
      ~excludes
      ~extensions
      ~store_type_check_resolution:true
      ~track_dependencies:true
      ~log_directory:(PyrePath.absolute log_path)
      ~python_major_version:major
      ~python_minor_version:minor
      ~python_micro_version:micro
      ~shared_memory_heap_size:heap_size
      ~shared_memory_dependency_table_power:dependency_table_power
      ~shared_memory_hash_table_power:hash_table_power
      ~enable_type_comments
      ~source_paths:(Configuration.SourcePaths.to_search_paths source_paths)
      ~enable_readonly_analysis
      ~enable_unawaited_awaitable_analysis
      ()


  let environment_controls_of code_navigation_configuration =
    analysis_configuration_of code_navigation_configuration
    |> Analysis.EnvironmentControls.create ~populate_call_graph:false ~use_lazy_module_tracking:true


  let start_options_of
      ({
         base = { CommandStartup.BaseConfiguration.source_paths; _ };
         socket_path;
         watchman_root;
         critical_files;
         additional_logging_sections = _;
       } as code_navigation_configuration)
    =
    let open Lwt.Infix in
    ServerCommand.watchman_options_of watchman_root
    >>= fun watchman ->
    let build_system_initializer = CodeNavigationServer.BuildSystem.get_initializer source_paths in
    Lwt.return
      {
        CodeNavigationServer.StartOptions.environment_controls =
          environment_controls_of code_navigation_configuration;
        source_paths;
        socket_path;
        watchman;
        build_system_initializer;
        critical_files;
      }
end

(* The Event module allows the code navigation server to communicate events the client relies on
 * to avoid the client having to poll or guess the server's state during initialization. These
 * events are written to ~output_channel, which is only stdout in practice at the moment. *)
module Event = struct
  type t = SocketCreated of PyrePath.t [@@deriving sexp, compare, hash, to_yojson]

  let serialize event = to_yojson event |> Yojson.Safe.to_string

  let write ~output_channel event =
    let open Lwt.Infix in
    serialize event |> Lwt_io.fprintl output_channel >>= fun () -> Lwt_io.flush output_channel
end

let write_event event =
  Lwt.catch
    (fun () -> Event.write ~output_channel:Lwt_io.stdout event)
    (function
      | Lwt_io.Channel_closed _
      | Caml_unix.Unix_error (Caml_unix.EPIPE, _, _) ->
          Lwt.return_unit
      | exn -> Lwt.fail exn)


let start_server_and_wait code_navigation_configuration =
  let open Lwt.Infix in
  CodeNavigationConfiguration.start_options_of code_navigation_configuration
  >>= fun start_options ->
  Lwt.catch
    (fun () ->
      CodeNavigationServer.Start.start_server
        start_options
        ~on_started:(fun { Server.ServerProperties.socket_path; _ } _ ->
          write_event (Event.SocketCreated socket_path)
          >>= fun () ->
          let wait_forever, _ = Lwt.wait () in
          wait_forever))
    (function
      | Server.Start.ServerStopped _ -> Lwt.return ServerCommand.ExitStatus.Ok
      | Server.Start.ServerInterrupted signal ->
          Log.error "Server process was interrupted with signal %s" (Signal.to_string signal);
          Lwt.return ServerCommand.ExitStatus.Error
      | exn ->
          let kind, message = Server.ServerError.kind_and_message_from_exception exn in
          Log.error "%a %s" Sexp.pp_hum (Server.ServerError.Kind.sexp_of_t kind) message;
          Lwt.return ServerCommand.ExitStatus.Error)


let run_server configuration_file =
  match
    CommandStartup.read_and_parse_json configuration_file ~f:CodeNavigationConfiguration.of_yojson
  with
  | Result.Error message ->
      Log.error "%s" message;
      exit 1
  | Result.Ok
      ({
         CodeNavigationConfiguration.base =
           {
             CommandStartup.BaseConfiguration.log_path;
             global_root;
             local_root;
             debug;
             remote_logging;
             profiling_output;
             memory_profiling_output;
             _;
           };
         additional_logging_sections;
         _;
       } as code_navigation_configuration) ->
      CommandStartup.setup_global_states
        ~global_root
        ~local_root
        ~debug
        ~additional_logging_sections
        ~remote_logging
        ~profiling_output
        ~memory_profiling_output
        ();

      (* Show start up notification. *)
      Server.StartupNotification.consume ~log_path ()
      |> Option.iter ~f:(fun message -> Log.warning "%s" message);

      (* Ignore SIGPIPE since >99% of the time they are non-fatal but the default Unix behavior is
         for it to terminate the server, which is not ideal. Besides, individual callsites can
         mostly detect the same class of issue by handling the EPIPE unix errno. *)
      Signal.Expert.(set Signal.pipe `Ignore);

      let exit_status = Lwt_main.run (start_server_and_wait code_navigation_configuration) in
      exit (ServerCommand.ExitStatus.exit_code exit_status)


let command =
  let filename_argument = Command.Param.(anon ("filename" %: Filename_unix.arg_type)) in
  Command.basic
    ~summary:"Start a new Pyre server for code navigation purpose"
    (Command.Param.map filename_argument ~f:(fun filename () -> run_server filename))
