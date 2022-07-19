(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
    let open JsonParsing in
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
      ~local_root:(Option.value local_root ~default:global_root)
      ~project_root:global_root
      ~search_paths:(List.map search_paths ~f:SearchPath.normalize)
      ~strict
      ~debug
      ~show_error_traces
      ~excludes
      ~extensions
      ~incremental_style:Configuration.Analysis.Shallow
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

let with_performance_tracking ~debug f =
  let timer = Timer.start () in
  let result = f () in
  let { Caml.Gc.minor_collections; major_collections; compactions; _ } = Caml.Gc.stat () in
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
  if debug then
    Memory.report_statistics ();
  result


let do_check configuration =
  Scheduler.with_scheduler ~configuration ~f:(fun scheduler ->
      with_performance_tracking ~debug:configuration.debug (fun () ->
          let environment =
            let read_write_environment =
              Analysis.EnvironmentControls.create ~populate_call_graph:false configuration
              |> Analysis.ErrorsEnvironment.create
            in
            let () =
              Analysis.ErrorsEnvironment.check_and_preprocess read_write_environment ~scheduler
            in
            Analysis.ErrorsEnvironment.read_only read_write_environment
          in
          ( Analysis.ErrorsEnvironment.ReadOnly.get_all_errors environment,
            Analysis.ErrorsEnvironment.ReadOnly.ast_environment environment )))


let compute_errors ~configuration ~build_system () =
  let errors, ast_environment = do_check configuration in
  List.map
    (List.sort ~compare:Analysis.AnalysisError.compare errors)
    ~f:(Server.RequestHandler.instantiate_error ~build_system ~configuration ~ast_environment)


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


let run_check check_configuration =
  let { CheckConfiguration.base = { CommandStartup.BaseConfiguration.source_paths; _ }; _ } =
    check_configuration
  in
  Server.BuildSystem.with_build_system source_paths ~f:(fun build_system ->
      let errors =
        compute_errors
          ~configuration:(CheckConfiguration.analysis_configuration_of check_configuration)
          ~build_system
          ()
      in
      print_errors errors;
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
        match exit_code with
        | Some exit_code when exit_code < 10 -> ExitStatus.BuckUserError
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
  | Worker.Worker_exited_abnormally (pid, status)
  | Base.Exn.Finally (Worker.Worker_exited_abnormally (pid, status), _) ->
      let message =
        match status with
        | Caml.Unix.WEXITED return_code -> Format.sprintf "exited with return code %d" return_code
        | Caml.Unix.WSIGNALED signal -> Format.sprintf "was killed with signal %d" signal
        | Caml.Unix.WSTOPPED signal -> Format.sprintf "was stopped with signal %d" signal
      in
      Log.error
        "Pyre encountered an internal exception: Worker_exited_abnormally: process %d %s"
        pid
        message;
      ExitStatus.PyreError
  | _ as exn ->
      Log.error "Pyre encountered an internal exception: %s" (Exn.to_string exn);
      ExitStatus.PyreError


let run_check configuration_file =
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
             (fun () -> run_check check_configuration)
             (fun exn -> Lwt.return (on_exception exn)))
  in
  Statistics.flush ();
  exit (ExitStatus.exit_code exit_status)


let command =
  let filename_argument = Command.Param.(anon ("filename" %: Filename.arg_type)) in
  Command.basic
    ~summary:"Runs a full check without a server"
    (Command.Param.map filename_argument ~f:(fun filename () -> run_check filename))
