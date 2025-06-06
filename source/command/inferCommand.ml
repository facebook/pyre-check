(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Pyre

(* Infer command uses the same exit code scheme as check command. *)
module ExitStatus = CheckCommand.ExitStatus

module InferConfiguration = struct
  type path_list = PyrePath.t list [@@deriving sexp, compare, hash]

  let path_list_of_raw raw_paths = List.map raw_paths ~f:PyrePath.create_absolute

  type t = {
    base: CommandStartup.BaseConfiguration.t;
    paths_to_modify: path_list option;
  }
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open Yojson.Safe.Util in
    (* Parsing logic *)
    try
      match CommandStartup.BaseConfiguration.of_yojson json with
      | Result.Error _ as error -> error
      | Result.Ok base ->
          let paths_to_modify =
            json
            |> member "paths_to_modify"
            |> [%of_yojson: string list option]
            |> Result.ok_or_failwith
            >>| path_list_of_raw
          in
          Result.Ok { base; paths_to_modify }
    with
    | Type_error (message, _)
    | Undefined (message, _) ->
        Result.Error message
    | other_exception -> Result.Error (Exception.exn_to_string other_exception)


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
            python_version;
            system_platform;
            parallel;
            number_of_workers;
            long_lived_workers;
            enable_readonly_analysis;
            enable_strict_override_check;
            enable_strict_any_check;
            enable_unawaited_awaitable_analysis;
            include_suppressed_errors;
            shared_memory =
              { Configuration.SharedMemory.heap_size; dependency_table_power; hash_table_power };
            remote_logging = _;
            profiling_output = _;
            memory_profiling_output = _;
          };
        _;
      }
    =
    Configuration.Analysis.create
      ~parallel
      ~analyze_external_sources:false
      ~filter_directories:checked_directory_allowlist
      ~ignore_all_errors:checked_directory_blocklist
      ~number_of_workers
      ?long_lived_workers
      ~local_root:(Option.value local_root ~default:global_root)
      ~project_root:global_root
      ~search_paths
      ~strict:false
      ~debug
      ~show_error_traces:false
      ~excludes
      ~extensions
      ~track_dependencies:false
      ~log_directory:(PyrePath.absolute log_path)
      ~python_version
      ~system_platform
      ~shared_memory_heap_size:heap_size
      ~shared_memory_dependency_table_power_from_configuration:dependency_table_power
      ~shared_memory_hash_table_power:hash_table_power
      ~enable_type_comments
      ~source_paths:(Configuration.SourcePaths.to_search_paths source_paths)
      ~enable_readonly_analysis
      ~enable_strict_override_check
      ~enable_strict_any_check
      ~enable_unawaited_awaitable_analysis
      ~include_suppressed_errors
      ()
end

let run_infer_local ~configuration ~build_system ~paths_to_modify () =
  let result =
    Scheduler.with_scheduler
      ~configuration
      ~should_log_exception:(fun _ -> true)
      ~f:(fun scheduler ->
        let ({ TypeInference.Service.global_environment; _ } as environment_data) =
          TypeInference.Service.build_environment_data ~configuration ()
        in
        let filename_lookup qualifier =
          let source_code_api =
            Analysis.AnnotatedGlobalEnvironment.ReadOnly.get_untracked_source_code_api
              global_environment
          in
          Server.PathLookup.absolute_source_path_of_qualifier_with_build_system
            ~build_system
            ~source_code_api
            qualifier
        in
        TypeInference.Service.run_infer
          ~configuration
          ~scheduler
          ~filename_lookup
          ~paths_to_modify
          environment_data)
  in
  if configuration.debug then
    Memory.report_statistics ();
  Yojson.Safe.pretty_to_string (`List [TypeInference.Data.GlobalResult.to_yojson result])
  |> Log.print "%s";
  Lwt.return ExitStatus.Ok


let run_infer infer_configuration =
  let {
    InferConfiguration.base = { CommandStartup.BaseConfiguration.source_paths; _ };
    paths_to_modify;
  }
    =
    infer_configuration
  in
  Server.BuildSystem.with_build_system source_paths ~f:(fun build_system ->
      let configuration = InferConfiguration.analysis_configuration_of infer_configuration in
      run_infer_local ~configuration ~build_system ~paths_to_modify ())


let run_infer configuration_file =
  let exit_status =
    match CommandStartup.read_and_parse_json configuration_file ~f:InferConfiguration.of_yojson with
    | Result.Error message ->
        Log.error "%s" message;
        ExitStatus.PyreError
    | Result.Ok
        ({
           InferConfiguration.base =
             {
               CommandStartup.BaseConfiguration.global_root;
               local_root;
               debug;
               remote_logging;
               profiling_output;
               memory_profiling_output;
               _;
             };
           _;
         } as infer_configuration) ->
        CommandStartup.setup_global_states
          ~global_root
          ~local_root
          ~debug
          ~additional_logging_sections:[]
          ~remote_logging
          ~profiling_output
          ~memory_profiling_output
          ();

        Lwt_main.run
          (Lwt.catch
             (fun () -> run_infer infer_configuration)
             (fun exn -> Lwt.return (CheckCommand.on_exception exn)))
  in
  Statistics.flush ();
  exit (ExitStatus.exit_code exit_status)


let doc = "Runs type inference"

let command ?(name = "infer") () =
  let open Cmdliner in
  let filename = Arg.(required & pos 0 (some string) None & info [] ~docv:"filename") in
  let term = Term.(const run_infer $ filename) in
  let info = Cmd.info name ~doc in
  Cmd.v info term
