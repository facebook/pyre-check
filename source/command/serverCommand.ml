(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Server

module ExitStatus = struct
  type t =
    | Ok
    | Error
  [@@deriving sexp, compare, hash]

  let exit_code = function
    | Ok -> 0
    | Error -> 1
end

let watchman_options_of = function
  | None -> Lwt.return_none
  | Some root ->
      let open Lwt.Infix in
      Watchman.Raw.create_exn () >>= fun raw -> Lwt.return_some { StartOptions.Watchman.root; raw }


module ServerConfiguration = struct
  type t = {
    base: CommandStartup.BaseConfiguration.t;
    socket_path: PyrePath.t;
    strict: bool;
    show_error_traces: bool;
    additional_logging_sections: string list;
    watchman_root: PyrePath.t option;
    taint_model_paths: PyrePath.t list;
    store_type_check_resolution: bool;
    critical_files: CriticalFile.t list;
    saved_state_action: SavedStateAction.t option;
    skip_initial_type_check: bool;
    use_lazy_module_tracking: bool;
  }
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open Yojson.Safe.Util in
    let open JsonParsing in
    try
      match CommandStartup.BaseConfiguration.of_yojson json with
      | Result.Error _ as error -> error
      | Result.Ok base ->
          let critial_file_list_member =
            let to_critical_file json = CriticalFile.of_yojson json |> Result.ok_or_failwith in
            list_member ~f:to_critical_file
          in

          let socket_path = json |> path_member "socket_path" in
          let watchman_root = json |> optional_path_member "watchman_root" in
          let taint_model_paths = json |> path_list_member "taint_model_paths" ~default:[] in
          let strict = json |> bool_member "strict" ~default:false in
          let show_error_traces = json |> bool_member "show_error_traces" ~default:false in
          let critical_files = json |> critial_file_list_member "critical_files" ~default:[] in
          let saved_state_action =
            json
            |> member "saved_state_action"
            |> function
            | `Null -> None
            | _ as json -> SavedStateAction.of_yojson json |> Result.ok_or_failwith |> Option.some
          in
          let store_type_check_resolution =
            json |> bool_member "store_type_check_resolution" ~default:false
          in
          let additional_logging_sections =
            json |> string_list_member "additional_logging_sections" ~default:[]
          in
          let skip_initial_type_check =
            json |> bool_member "skip_initial_type_check" ~default:false
          in
          let use_lazy_module_tracking =
            json |> bool_member "use_lazy_module_tracking" ~default:false
          in
          Result.Ok
            {
              base;
              socket_path;
              watchman_root;
              taint_model_paths;
              strict;
              show_error_traces;
              critical_files;
              saved_state_action;
              store_type_check_resolution;
              additional_logging_sections;
              skip_initial_type_check;
              use_lazy_module_tracking;
            }
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
        strict;
        socket_path = _;
        taint_model_paths;
        show_error_traces;
        store_type_check_resolution;
        watchman_root = _;
        critical_files = _;
        saved_state_action = _;
        additional_logging_sections = _;
        skip_initial_type_check = _;
        use_lazy_module_tracking = _;
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
      ~taint_model_paths
      ~strict
      ~debug
      ~show_error_traces
      ~excludes
      ~extensions
      ~store_type_check_resolution
      ~incremental_style:Configuration.Analysis.FineGrained
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


  let environment_controls_of ({ use_lazy_module_tracking; _ } as server_configuration) =
    analysis_configuration_of server_configuration
    |> Analysis.EnvironmentControls.create ~populate_call_graph:true ~use_lazy_module_tracking


  let start_options_of
      ({
         base = { CommandStartup.BaseConfiguration.source_paths; _ };
         socket_path;
         watchman_root;
         critical_files;
         saved_state_action;
         skip_initial_type_check;
         _;
       } as server_configuration)
    =
    let open Lwt.Infix in
    watchman_options_of watchman_root
    >>= fun watchman ->
    Lwt.return
      {
        StartOptions.environment_controls = environment_controls_of server_configuration;
        source_paths;
        socket_path;
        watchman;
        build_system_initializer = BuildSystem.get_initializer source_paths;
        critical_files;
        saved_state_action;
        skip_initial_type_check;
      }
end

module ServerEvent = struct
  type t =
    | SocketCreated of PyrePath.t
    | ServerInitialized
    | Exception of string * Server.ServerError.Kind.t
  [@@deriving sexp, compare, hash, to_yojson]

  let serialize event = to_yojson event |> Yojson.Safe.to_string

  let write ~output_channel event =
    let open Lwt.Infix in
    serialize event |> Lwt_io.fprintl output_channel >>= fun () -> Lwt_io.flush output_channel
end

let start_server_and_wait ~event_channel server_configuration =
  let open Lwt.Infix in
  let write_event event =
    Lwt.catch
      (fun () -> ServerEvent.write ~output_channel:event_channel event)
      (function
        | Lwt_io.Channel_closed _
        | Caml_unix.Unix_error (Caml_unix.EPIPE, _, _) ->
            Lwt.return_unit
        | exn -> Lwt.fail exn)
  in
  ServerConfiguration.start_options_of server_configuration
  >>= fun start_options ->
  let start_time = Timer.start () in
  let handle_error ~kind ~message () =
    Log.info "%s" message;
    write_event (ServerEvent.Exception (message, kind)) >>= fun () -> Lwt.return ExitStatus.Error
  in
  Start.start_server
    start_options
    ~on_server_socket_ready:(fun socket_path ->
      (* An empty message signals that server socket has been created. *)
      write_event (ServerEvent.SocketCreated socket_path))
    ~on_started:(fun _ server_state ->
      ExclusiveLock.Lazy.force server_state
      >>= fun _ ->
      write_event ServerEvent.ServerInitialized
      >>= fun () ->
      let wait_forever, _ = Lwt.wait () in
      wait_forever)
    ~on_exception:(function
      | Start.ServerStopped reason ->
          let reason = Option.value_map reason ~f:Stop.Reason.name_of ~default:"unknown" in
          Stop.log_stopped_server ~reason ~start_time ();
          Lwt.return ExitStatus.Ok
      | Start.ServerInterrupted signal ->
          Stop.log_stopped_server ~reason:(Signal.to_string signal) ~start_time ();
          let message =
            Format.sprintf "Server process get interrputed with signal %s" (Signal.to_string signal)
          in
          handle_error ~kind:Server.ServerError.Kind.Pyre ~message ()
      | exn ->
          let kind, message = Server.ServerError.kind_and_message_from_exception exn in
          handle_error ~kind ~message ())


let run_server configuration_file =
  match CommandStartup.read_and_parse_json configuration_file ~f:ServerConfiguration.of_yojson with
  | Result.Error message ->
      Log.error "%s" message;
      exit 1
  | Result.Ok
      ({
         ServerConfiguration.base =
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
       } as server_configuration) ->
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
      StartupNotification.consume ~log_path ()
      |> Option.iter ~f:(fun message -> Log.warning "%s" message);

      (* Ignore SIGPIPE since >99% of the time they are non-fatal but the default Unix behavior is
         for it to terminate the server, which is not ideal. Besides, individual callsites can
         mostly detect the same class of issue by handling the EPIPE unix errno. *)
      Signal.Expert.(set Signal.pipe `Ignore);

      let exit_status =
        Lwt_main.run (start_server_and_wait ~event_channel:Lwt_io.stdout server_configuration)
      in
      exit (ExitStatus.exit_code exit_status)


let command =
  Printexc.record_backtrace true;
  let filename_argument = Command.Param.(anon ("filename" %: Filename_unix.arg_type)) in
  Command.basic
    ~summary:"Starts a new Pyre server."
    (Command.Param.map filename_argument ~f:(fun filename () -> run_server filename))
