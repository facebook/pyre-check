(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

(* Analyze command uses the same exit code scheme as check command. *)
module ExitStatus = struct
  type t =
    | CheckStatus of CheckCommand.ExitStatus.t
    | TaintConfigurationError
    | ModelVerificationError

  let exit_code = function
    (* 1-9 are reserved for CheckCommand.ExitStatus *)
    | CheckStatus status -> CheckCommand.ExitStatus.exit_code status
    | TaintConfigurationError -> 10
    | ModelVerificationError -> 11
end

module AnalyzeConfiguration = struct
  type t = {
    base: CommandStartup.BaseConfiguration.t;
    dump_call_graph: PyrePath.t option;
    dump_model_query_results: PyrePath.t option;
    find_missing_flows: string option;
    inline_decorators: bool;
    maximum_tito_depth: int option;
    maximum_trace_length: int option;
    no_verify: bool;
    verify_dsl: bool;
    repository_root: PyrePath.t option;
    rule_filter: int list option;
    save_results_to: PyrePath.t option;
    strict: bool;
    taint_model_paths: PyrePath.t list;
    use_cache: bool;
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
          let dump_call_graph = optional_path_member "dump_call_graph" json in
          let dump_model_query_results = optional_path_member "dump_model_query_results" json in
          let find_missing_flows = optional_string_member "find_missing_flows" json in
          let inline_decorators = bool_member "inline_decorators" ~default:false json in
          let maximum_tito_depth = optional_int_member "maximum_tito_depth" json in
          let maximum_trace_length = optional_int_member "maximum_trace_length" json in
          let no_verify = bool_member "no_verify" ~default:false json in
          let verify_dsl = bool_member "verify_dsl" ~default:false json in
          let repository_root = optional_path_member "repository_root" json in
          let rule_filter =
            member "rule_filter" json
            |> function
            | `Null -> None
            | _ as json -> Some (convert_each to_int json)
          in
          let save_results_to = optional_path_member "save_results_to" json in
          let strict = bool_member "strict" ~default:false json in
          let taint_model_paths = json |> path_list_member "taint_model_paths" ~default:[] in
          let use_cache = bool_member "use_cache" ~default:false json in

          Result.Ok
            {
              base;
              dump_call_graph;
              dump_model_query_results;
              find_missing_flows;
              inline_decorators;
              maximum_tito_depth;
              maximum_trace_length;
              no_verify;
              verify_dsl;
              repository_root;
              rule_filter;
              save_results_to;
              strict;
              taint_model_paths;
              use_cache;
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
            python_version = { Configuration.PythonVersion.major; minor; micro };
            parallel;
            number_of_workers;
            shared_memory =
              { Configuration.SharedMemory.heap_size; dependency_table_power; hash_table_power };
            enable_type_comments;
            remote_logging = _;
            profiling_output = _;
            memory_profiling_output = _;
          };
        dump_call_graph;
        dump_model_query_results;
        find_missing_flows;
        maximum_tito_depth;
        maximum_trace_length;
        no_verify;
        verify_dsl;
        rule_filter;
        save_results_to;
        strict;
        taint_model_paths;
        use_cache;
        inline_decorators;
        repository_root;
      }
    =
    let configuration =
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
        ~show_error_traces:false
        ~excludes
        ~extensions
        ~store_type_errors:false
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
    in
    {
      Configuration.StaticAnalysis.configuration;
      repository_root;
      result_json_path = save_results_to;
      dump_call_graph;
      verify_models = not no_verify;
      verify_dsl;
      rule_filter;
      find_missing_flows;
      dump_model_query_results;
      use_cache;
      inline_decorators;
      maximum_trace_length;
      maximum_tito_depth;
    }
end

let with_performance_tracking ~f =
  let timer = Timer.start () in
  let result = f () in
  let { Caml.Gc.minor_collections; major_collections; compactions; _ } = Caml.Gc.stat () in
  Statistics.performance
    ~name:"analyze"
    ~timer
    ~integers:
      [
        "gc_minor_collections", minor_collections;
        "gc_major_collections", major_collections;
        "gc_compactions", compactions;
      ]
    ();
  result


let run_analyze analyze_configuration =
  let { AnalyzeConfiguration.base = { CommandStartup.BaseConfiguration.source_paths; _ }; _ } =
    analyze_configuration
  in
  let ({ Configuration.StaticAnalysis.configuration = analysis_configuration; _ } as
      static_analysis_configuration)
    =
    AnalyzeConfiguration.analysis_configuration_of analyze_configuration
  in
  Server.BuildSystem.with_build_system source_paths ~f:(fun build_system ->
      Scheduler.with_scheduler ~configuration:analysis_configuration ~f:(fun scheduler ->
          with_performance_tracking ~f:(fun () ->
              TaintAnalysis.run_taint_analysis
                ~static_analysis_configuration
                ~build_system
                ~scheduler
                ());
          Lwt.return (ExitStatus.CheckStatus CheckCommand.ExitStatus.Ok)))


let on_exception = function
  | Taint.TaintConfiguration.TaintConfigurationError errors ->
      Yojson.Safe.pretty_to_string
        (`Assoc ["errors", `List (List.map errors ~f:Taint.TaintConfiguration.Error.to_json)])
      |> Log.print "%s";
      ExitStatus.TaintConfigurationError
  | Taint.ModelVerificationError.ModelVerificationErrors errors ->
      Yojson.Safe.pretty_to_string
        (`Assoc ["errors", `List (List.map errors ~f:Taint.ModelVerificationError.to_json)])
      |> Log.print "%s";
      ExitStatus.ModelVerificationError
  | exn -> ExitStatus.CheckStatus (CheckCommand.on_exception exn)


let run_analyze configuration_file =
  let exit_status =
    match
      CommandStartup.read_and_parse_json configuration_file ~f:AnalyzeConfiguration.of_yojson
    with
    | Result.Error message ->
        Log.error "%s" message;
        ExitStatus.CheckStatus CheckCommand.ExitStatus.PyreError
    | Result.Ok
        ({
           AnalyzeConfiguration.base =
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
         } as analyze_configuration) ->
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
             (fun () -> run_analyze analyze_configuration)
             (fun exn -> Lwt.return (on_exception exn)))
  in
  Statistics.flush ();
  exit (ExitStatus.exit_code exit_status)


let command =
  let filename_argument = Command.Param.(anon ("filename" %: Filename.arg_type)) in
  Command.basic
    ~summary:"Runs taint analysis"
    (Command.Param.map filename_argument ~f:(fun filename () -> run_analyze filename))
