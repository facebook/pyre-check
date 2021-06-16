(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
module Path = PyrePath

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
    (* Source file discovery *)
    source_paths: Configuration.SourcePaths.t;
    search_paths: SearchPath.t list;
    excludes: string list;
    checked_directory_allowlist: Path.t list;
    checked_directory_blocklist: Path.t list;
    extensions: Configuration.Extension.t list;
    (* Auxiliary paths *)
    log_path: Path.t;
    global_root: Path.t;
    local_root: Path.t option;
    (* Type checking controls *)
    debug: bool;
    strict: bool;
    python_version: Configuration.PythonVersion.t;
    show_error_traces: bool;
    (* Parallelism controls *)
    parallel: bool;
    number_of_workers: int;
    (* Memory controls *)
    shared_memory: Configuration.SharedMemory.t;
    (* Logging controls *)
    additional_logging_sections: string list;
    remote_logging: Configuration.RemoteLogging.t option;
    profiling_output: string option;
    memory_profiling_output: string option;
  }
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open Yojson.Safe.Util in
    let open JsonParsing in
    (* Parsing logic *)
    try
      let source_paths =
        json
        |> member "source_paths"
        |> Configuration.SourcePaths.of_yojson
        |> Result.ok_or_failwith
      in
      let search_paths =
        json
        |> list_member
             "search_paths"
             ~f:(fun element -> to_string element |> SearchPath.create)
             ~default:[]
      in
      let excludes = json |> string_list_member "excludes" ~default:[] in
      let checked_directory_allowlist =
        json |> path_list_member "checked_directory_allowlist" ~default:[]
      in
      let checked_directory_blocklist =
        json |> path_list_member "checked_directory_blocklist" ~default:[]
      in
      let extensions =
        json
        |> string_list_member "extensions" ~default:[]
        |> List.map ~f:Configuration.Extension.create_extension
      in
      let log_path = json |> path_member "log_path" in
      let global_root = json |> path_member "global_root" in
      let local_root = json |> optional_path_member "local_root" in
      let debug = json |> bool_member "debug" ~default:false in
      let strict = json |> bool_member "strict" ~default:false in
      let python_version =
        json
        |> member "python_version"
        |> function
        | `Null -> Configuration.PythonVersion.default
        | _ as json -> Configuration.PythonVersion.of_yojson json |> Result.ok_or_failwith
      in
      let show_error_traces = json |> bool_member "show_error_traces" ~default:false in
      let parallel = json |> bool_member "parallel" ~default:false in
      let number_of_workers = json |> int_member "number_of_workers" ~default:1 in
      let shared_memory =
        json
        |> member "shared_memory"
        |> function
        | `Null -> Configuration.SharedMemory.default
        | _ as json -> Configuration.SharedMemory.of_yojson json |> Result.ok_or_failwith
      in
      let additional_logging_sections =
        json |> string_list_member "additional_logging_sections" ~default:[]
      in
      let remote_logging =
        json
        |> member "remote_logging"
        |> function
        | `Null -> None
        | _ as json ->
            Configuration.RemoteLogging.of_yojson json |> Result.ok_or_failwith |> Option.some
      in
      let profiling_output = json |> optional_string_member "profiling_output" in
      let memory_profiling_output = json |> optional_string_member "memory_profiling_output" in
      Result.Ok
        {
          source_paths;
          search_paths;
          excludes;
          checked_directory_allowlist;
          checked_directory_blocklist;
          extensions;
          log_path;
          global_root;
          local_root;
          debug;
          strict;
          python_version;
          show_error_traces;
          parallel;
          number_of_workers;
          shared_memory;
          additional_logging_sections;
          remote_logging;
          profiling_output;
          memory_profiling_output;
        }
    with
    | Type_error (message, _)
    | Undefined (message, _) ->
        Result.Error message
    | other_exception -> Result.Error (Exn.to_string other_exception)


  let analysis_configuration_of
      {
        source_paths;
        search_paths;
        excludes;
        checked_directory_allowlist;
        checked_directory_blocklist;
        extensions;
        log_path;
        global_root;
        local_root;
        debug;
        strict;
        python_version = { Configuration.PythonVersion.major; minor; micro };
        show_error_traces;
        parallel;
        number_of_workers;
        shared_memory =
          { Configuration.SharedMemory.heap_size; dependency_table_power; hash_table_power };
        additional_logging_sections = _;
        remote_logging = _;
        profiling_output = _;
        memory_profiling_output = _;
      }
    =
    let source_path =
      match source_paths with
      | Configuration.SourcePaths.Simple source_paths -> source_paths
      | Buck { Configuration.Buck.artifact_root; _ } -> [SearchPath.Root artifact_root]
    in
    Configuration.Analysis.create
      ~infer:false
      ~parallel
      ~analyze_external_sources:false
      ~filter_directories:checked_directory_allowlist
      ~ignore_all_errors:checked_directory_blocklist
      ~number_of_workers
      ~local_root:(Option.value local_root ~default:global_root)
      ~project_root:global_root
      ~search_path:(List.map search_paths ~f:SearchPath.normalize)
      ~strict
      ~debug
      ~show_error_traces
      ~excludes
      ~extensions
      ~incremental_style:Configuration.Analysis.FineGrained
      ~include_hints:false
      ~perform_autocompletion:false
      ~log_directory:(Path.absolute log_path)
      ~python_major_version:major
      ~python_minor_version:minor
      ~python_micro_version:micro
      ~shared_memory_heap_size:heap_size
      ~shared_memory_dependency_table_power:dependency_table_power
      ~shared_memory_hash_table_power:hash_table_power
      ~source_path
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
          let { Service.Check.errors; environment } =
            Service.Check.check
              ~scheduler
              ~configuration
              ~call_graph_builder:(module Analysis.Callgraph.DefaultBuilder)
          in
          ( errors,
            Analysis.TypeEnvironment.ast_environment environment
            |> Analysis.AstEnvironment.read_only )))


let compute_errors ~configuration ~build_system () =
  let errors, ast_environment = do_check configuration in
  List.map
    errors
    ~f:(Newserver.RequestHandler.instantiate_error ~build_system ~configuration ~ast_environment)


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
  let { CheckConfiguration.source_paths; _ } = check_configuration in
  Newserver.BuildSystem.with_build_system source_paths ~f:(fun build_system ->
      let errors =
        compute_errors
          ~configuration:(CheckConfiguration.analysis_configuration_of check_configuration)
          ~build_system
          ()
      in
      print_errors errors;
      Lwt.return ExitStatus.Ok)


let on_check_exception = function
  | Buck.Raw.BuckError { arguments; description; exit_code } ->
      Log.error
        "Cannot build the project: %s. To reproduce this error, run `%s`."
        description
        (Buck.Raw.ArgumentList.to_buck_command arguments);
      let exit_status =
        match exit_code with
        | Some exit_code when exit_code < 10 -> ExitStatus.BuckUserError
        | _ -> ExitStatus.BuckInternalError
      in
      Lwt.return exit_status
  | Buck.Builder.JsonError message ->
      Log.error "Cannot build the project because Buck returns malformed JSON: %s" message;
      Lwt.return ExitStatus.BuckUserError
  | Buck.Builder.LinkTreeConstructionError message ->
      Log.error
        "Cannot build the project because Pyre encounters a fatal error while constructing a link \
         tree: %s"
        message;
      Lwt.return ExitStatus.BuckUserError
  | _ as exn ->
      Log.error "Pyre encountered an internal exception: %s" (Exn.to_string exn);
      Lwt.return ExitStatus.PyreError


let run_check configuration_file =
  let exit_status =
    match
      NewCommandStartup.read_and_parse_json configuration_file ~f:CheckConfiguration.of_yojson
    with
    | Result.Error message ->
        Log.error "%s" message;
        ExitStatus.PyreError
    | Result.Ok
        ( {
            CheckConfiguration.global_root;
            local_root;
            debug;
            additional_logging_sections;
            remote_logging;
            profiling_output;
            memory_profiling_output;
            _;
          } as check_configuration ) ->
        NewCommandStartup.setup_global_states
          ~global_root
          ~local_root
          ~debug
          ~additional_logging_sections
          ~remote_logging
          ~profiling_output
          ~memory_profiling_output
          ();

        Lwt_main.run (Lwt.catch (fun () -> run_check check_configuration) on_check_exception)
  in
  Statistics.flush ();
  exit (ExitStatus.exit_code exit_status)


let command =
  let filename_argument = Command.Param.(anon ("filename" %: Filename.arg_type)) in
  Command.basic
    ~summary:"Runs a full check without a server"
    (Command.Param.map filename_argument ~f:(fun filename () -> run_check filename))
