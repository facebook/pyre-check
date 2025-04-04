(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module defines the entrypoint for the backend portion of running `pyre check`. The `pyre
   check` command's fontend shells out to the backend to run the check, which runs in an environment
   with no dependency tracking. At the end we dump errors to stdout and exit, passing control back
   to the frontend which will handle displaying the errors for end users.

   Note that running `pyre` is actually using `pyre incremental` under the hood, which does not
   depend on this logic but rather on `serverCommand.ml`. *)

open Core

module ExitStatus = struct
  type t =
    | Ok
    | PyreError
    | BuckInternalError
    | BuckUserError
  [@@deriving sexp, compare, hash]

  let exit_code = function
    | Ok -> 0
    | PyreError -> 1
    | BuckInternalError -> 2
    | BuckUserError -> 3
end

module CheckConfiguration = struct
  type t = {
    base: CommandStartup.BaseConfiguration.t;
    strict: bool;
    show_error_traces: bool;
    additional_logging_sections: string list;
  }
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open Yojson.Safe.Util in
    let open JsonParsing.YojsonUtils in
    (* Parsing logic *)
    try
      match CommandStartup.BaseConfiguration.of_yojson json with
      | Result.Error _ as error -> error
      | Result.Ok base ->
          let strict = json |> bool_member "strict" ~default:false in
          let show_error_traces = json |> bool_member "show_error_traces" ~default:false in
          let additional_logging_sections =
            json |> string_list_member "additional_logging_sections" ~default:[]
          in
          Result.Ok { base; strict; show_error_traces; additional_logging_sections }
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
            enable_unawaited_awaitable_analysis;
            include_suppressed_errors;
            shared_memory =
              { Configuration.SharedMemory.heap_size; dependency_table_power; hash_table_power };
            remote_logging = _;
            profiling_output = _;
            memory_profiling_output = _;
          };
        show_error_traces;
        strict;
        additional_logging_sections = _;
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
      ~strict
      ~debug
      ~show_error_traces
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
      ~enable_unawaited_awaitable_analysis
      ~include_suppressed_errors
      ()
end

let with_performance_tracking ~debug f =
  let timer = Timer.start () in
  let result = f () in
  if debug then (
    let { Stdlib.Gc.minor_collections; major_collections; compactions; _ } =
      Stdlib.Gc.quick_stat ()
    in
    Statistics.performance
      ~name:"check"
      ~timer
      ~integers:
        [
          "gc_minor_collections", minor_collections;
          "gc_major_collections", major_collections;
          "gc_compactions", compactions;
        ]
      ~normals:["request kind", "FullCheck"]
      ();
    Memory.report_statistics ())
  else
    Statistics.performance ~name:"check" ~timer ~normals:["request kind", "FullCheck"] ();
  result


let do_check configuration =
  Scheduler.with_scheduler
    ~configuration
    ~should_log_exception:(fun _ -> true)
    ~f:(fun scheduler ->
      with_performance_tracking ~debug:configuration.debug (fun () ->
          let read_write_environment =
            Analysis.EnvironmentControls.create ~populate_call_graph:false configuration
            |> Analysis.ErrorsEnvironment.create_with_ast_environment
          in
          let type_check_qualifiers =
            Analysis.ErrorsEnvironment.AssumeGlobalModuleListing.global_module_paths_api
              read_write_environment
            |> Analysis.GlobalModulePathsApi.type_check_qualifiers
          in
          Analysis.ErrorsEnvironment.check_and_postprocess
            read_write_environment
            ~scheduler
            ~scheduler_policies:Configuration.SchedulerPolicies.empty
            type_check_qualifiers;
          Analysis.ErrorsEnvironment.read_only read_write_environment, type_check_qualifiers))


let compute_errors ~configuration ~build_system () =
  let errors_environment, type_check_qualifiers = do_check configuration in
  let errors =
    Analysis.ErrorsEnvironment.ReadOnly.get_errors_for_qualifiers
      errors_environment
      type_check_qualifiers
  in
  let source_code_api =
    Analysis.ErrorsEnvironment.ReadOnly.get_untracked_source_code_api errors_environment
  in
  List.map
    (List.sort ~compare:Analysis.AnalysisError.compare errors)
    ~f:(Server.RequestHandler.instantiate_error_with_build_system ~build_system ~source_code_api)


let print_errors errors =
  Yojson.Safe.to_string
    (`Assoc
      [
        ( "errors",
          `List
            (List.map ~f:(fun error -> Analysis.AnalysisError.Instantiated.to_yojson error) errors)
        );
      ])
  |> Log.print "%s"


let run_check ~build_system check_configuration =
  let errors =
    compute_errors
      ~configuration:(CheckConfiguration.analysis_configuration_of check_configuration)
      ~build_system
      ()
  in
  print_errors errors


let run_check_in_lwt check_configuration =
  let { CheckConfiguration.base = { CommandStartup.BaseConfiguration.source_paths; _ }; _ } =
    check_configuration
  in
  Server.BuildSystem.with_build_system source_paths ~f:(fun build_system ->
      run_check ~build_system check_configuration;
      Lwt.return ExitStatus.Ok)


let on_exception = function
  | Buck.Raw.BuckError { buck_command; arguments; description; exit_code; additional_logs } ->
      Log.error "Cannot build the project: %s. " description;
      (* Avoid spamming the user with repro command if the argument is really long. *)
      if Buck.Raw.ArgumentList.length arguments <= 20 then
        Log.error
          "To reproduce this error, run `%s`."
          (Buck.Raw.ArgumentList.to_buck_command ~buck_command arguments);
      if not (List.is_empty additional_logs) then (
        Log.error "Here are the last few lines of Buck log:";
        Log.error "  ...";
        List.iter additional_logs ~f:(Log.error "  %s"));
      let exit_status =
        (* See fbcode/buck2/app/buck2_client_ctx/src/exit_result.rs *)
        match exit_code with
        | Some 3 -> ExitStatus.BuckUserError
        | Some 1 ->
            (* unknown error, treat it as user error to be conservative. *)
            ExitStatus.BuckUserError
        | _ -> ExitStatus.BuckInternalError
      in
      exit_status
  | Buck.Interface.JsonError message ->
      Log.error "Cannot build the project because Buck returns malformed JSON: %s" message;
      ExitStatus.BuckUserError
  | Buck.Builder.LinkTreeConstructionError message ->
      Log.error
        "Cannot build the project because Pyre encounters a fatal error while constructing a link \
         tree: %s"
        message;
      ExitStatus.BuckUserError
  | Server.ChecksumMap.LoadError message ->
      Log.error "Cannot load external wheel properly. %s" message;
      ExitStatus.PyreError
  | exn ->
      let raw_backtrace = Stdlib.Printexc.get_raw_backtrace () in
      Log.log_exception "Pyre encountered an internal exception." exn raw_backtrace;
      ExitStatus.PyreError


let run_check_command configuration_file =
  let exit_status =
    match CommandStartup.read_and_parse_json configuration_file ~f:CheckConfiguration.of_yojson with
    | Result.Error message ->
        Log.error "%s" message;
        ExitStatus.PyreError
    | Result.Ok
        ({
           CheckConfiguration.base =
             {
               CommandStartup.BaseConfiguration.global_root;
               local_root;
               debug;
               remote_logging;
               profiling_output;
               memory_profiling_output;
               _;
             };
           additional_logging_sections;
           _;
         } as check_configuration) ->
        CommandStartup.setup_global_states
          ~global_root
          ~local_root
          ~debug
          ~additional_logging_sections
          ~remote_logging
          ~profiling_output
          ~memory_profiling_output
          ();

        Lwt_main.run
          (Lwt.catch
             (fun () -> run_check_in_lwt check_configuration)
             (fun exn -> Lwt.return (on_exception exn)))
  in
  Statistics.flush ();
  exit (ExitStatus.exit_code exit_status)


let doc = "Runs a full check without a server"

let command ?(name = "check") () =
  let open Cmdliner in
  let filename = Arg.(required & pos 0 (some string) None & info [] ~docv:"filename") in
  let term = Term.(const run_check_command $ filename) in
  let info = Cmd.info name ~doc in
  Cmd.v info term
