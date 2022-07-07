(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Server

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
            }
    with
    | Type_error (message, _)
    | Undefined (message, _) ->
        Result.Error message
    | other_exception -> Result.Error (Exn.to_string other_exception)


  let start_options_of
      {
        base = { CommandStartup.BaseConfiguration.source_paths; _ };
        socket_path;
        watchman_root;
        critical_files;
        saved_state_action;
        skip_initial_type_check;
        _;
      }
    =
    {
      StartOptions.source_paths;
      socket_path;
      watchman_root;
      critical_files;
      saved_state_action;
      skip_initial_type_check;
    }


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
      ()
end

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
        let start_options = ServerConfiguration.start_options_of server_configuration in
        let configuration = ServerConfiguration.analysis_configuration_of server_configuration in
        Lwt_main.run
          (Start.start_server_and_wait ~event_channel:Lwt_io.stdout ~configuration start_options)
      in
      exit (Start.ExitStatus.exit_code exit_status)


let command =
  let filename_argument = Command.Param.(anon ("filename" %: Filename.arg_type)) in
  Command.basic
    ~summary:"Starts a new Pyre server."
    (Command.Param.map filename_argument ~f:(fun filename () -> run_server filename))
