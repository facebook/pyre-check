(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core

(* Analyze command uses the same exit code scheme as check command. *)
module ExitStatus = struct
  type t =
    | CheckStatus of CheckCommand.ExitStatus.t
    | TaintConfigurationError
    | ModelVerificationError
    | PyreflyFileFormatError

  let exit_code = function
    (* 1-9 are reserved for CheckCommand.ExitStatus *)
    | CheckStatus status -> CheckCommand.ExitStatus.exit_code status
    | TaintConfigurationError -> 10
    | ModelVerificationError -> 11
    | PyreflyFileFormatError -> 12
end

module AnalyzeConfiguration = struct
  type t = {
    base: CommandStartup.BaseConfiguration.t;
    dump_call_graph: PyrePath.t option;
    dump_model_query_results: PyrePath.t option;
    find_missing_flows: Configuration.MissingFlowKind.t option;
    infer_self_tito: bool;
    infer_argument_tito: bool;
    maximum_model_source_tree_width: int option;
    maximum_model_sink_tree_width: int option;
    maximum_model_tito_tree_width: int option;
    maximum_tree_depth_after_widening: int option;
    maximum_return_access_path_width: int option;
    maximum_return_access_path_depth_after_widening: int option;
    maximum_tito_collapse_depth: int option;
    maximum_tito_positions: int option;
    maximum_overrides_to_analyze: int option;
    maximum_trace_length: int option;
    maximum_tito_depth: int option;
    no_verify: bool;
    verify_dsl: bool;
    verify_taint_config_only: bool;
    repository_root: PyrePath.t option;
    rule_filter: int list option;
    source_filter: string list option;
    sink_filter: string list option;
    transform_filter: string list option;
    save_results_to: PyrePath.t option;
    output_format: Configuration.TaintOutputFormat.t;
    pyrefly_results: PyrePath.t option;
    strict: bool;
    taint_model_paths: PyrePath.t list;
    use_cache: bool;
    build_cache_only: bool;
    check_invariants: bool;
    limit_entrypoints: bool;
    compact_ocaml_heap: bool;
    saved_state: Configuration.StaticAnalysis.SavedState.t;
    compute_coverage: bool;
    scheduler_policies: Configuration.SchedulerPolicies.t;
    higher_order_call_graph_max_iterations: int option;
    maximum_target_depth: int option;
    maximum_parameterized_targets_at_call_site: int option;
  }
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open Core.Result in
    let open Yojson.Safe.Util in
    let open JsonParsing.YojsonUtils in
    (* Parsing logic *)
    try
      match CommandStartup.BaseConfiguration.of_yojson json with
      | Result.Error _ as error -> error
      | Result.Ok base ->
          let dump_call_graph = optional_path_member "dump_call_graph" json in
          let dump_model_query_results = optional_path_member "dump_model_query_results" json in
          optional_string_member "find_missing_flows" json
          |> (function
               | Some missing_flow ->
                   Configuration.MissingFlowKind.of_string missing_flow >>| Option.some
               | None -> Ok None)
          >>= fun find_missing_flows ->
          let higher_order_call_graph_max_iterations =
            optional_int_member "higher_order_call_graph_max_iterations" json
          in
          let maximum_target_depth = optional_int_member "maximum_target_depth" json in
          let maximum_parameterized_targets_at_call_site =
            optional_int_member "maximum_parameterized_targets_at_call_site" json
          in
          let infer_self_tito = bool_member "infer_self_tito" ~default:true json in
          let infer_argument_tito = bool_member "infer_argument_tito" ~default:false json in
          let maximum_model_source_tree_width =
            optional_int_member "maximum_model_source_tree_width" json
          in
          let maximum_model_sink_tree_width =
            optional_int_member "maximum_model_sink_tree_width" json
          in
          let maximum_model_tito_tree_width =
            optional_int_member "maximum_model_tito_tree_width" json
          in
          let maximum_tree_depth_after_widening =
            optional_int_member "maximum_tree_depth_after_widening" json
          in
          let maximum_return_access_path_width =
            optional_int_member "maximum_return_access_path_width" json
          in
          let maximum_return_access_path_depth_after_widening =
            optional_int_member "maximum_return_access_path_depth_after_widening" json
          in
          let maximum_tito_collapse_depth =
            optional_int_member "maximum_tito_collapse_depth" json
          in
          let maximum_tito_positions = optional_int_member "maximum_tito_positions" json in
          let maximum_overrides_to_analyze =
            optional_int_member "maximum_overrides_to_analyze" json
          in
          let maximum_trace_length = optional_int_member "maximum_trace_length" json in
          let maximum_tito_depth = optional_int_member "maximum_tito_depth" json in
          let no_verify = bool_member "no_verify" ~default:false json in
          let verify_dsl = bool_member "verify_dsl" ~default:false json in
          let verify_taint_config_only =
            bool_member "verify_taint_config_only" ~default:false json
          in
          let repository_root = optional_path_member "repository_root" json in
          let rule_filter = optional_list_member ~f:to_int "rule_filter" json in
          let source_filter = optional_list_member ~f:to_string "source_filter" json in
          let sink_filter = optional_list_member ~f:to_string "sink_filter" json in
          let transform_filter = optional_list_member ~f:to_string "transform_filter" json in
          let save_results_to = optional_path_member "save_results_to" json in
          string_member "output_format" ~default:"json" json
          |> Configuration.TaintOutputFormat.of_string
          >>= fun output_format ->
          let strict = bool_member "strict" ~default:false json in
          let pyrefly_results = optional_path_member "pyrefly_results" json in
          let taint_model_paths = json |> path_list_member "taint_model_paths" ~default:[] in
          let use_cache = bool_member "use_cache" ~default:false json in
          let build_cache_only = bool_member "build_cache_only" ~default:false json in
          let check_invariants = bool_member "check_invariants" ~default:false json in
          let limit_entrypoints = bool_member "limit_entrypoints" ~default:false json in
          let compact_ocaml_heap = bool_member "compact_ocaml_heap" ~default:false json in
          let compute_coverage = bool_member "compute_coverage" ~default:false json in
          let scheduler_policies =
            json
            |> member "scheduler_policies"
            |> function
            | `Null -> Configuration.SchedulerPolicies.empty
            | json -> Configuration.SchedulerPolicies.of_yojson json |> Result.ok_or_failwith
          in
          (match Yojson.Safe.Util.member "saved_state" json with
          | `Null -> Result.Ok Configuration.StaticAnalysis.SavedState.empty
          | saved_state ->
              let watchman_root = optional_string_member "watchman_root" saved_state in
              let project_name = optional_string_member "project_name" saved_state in
              let preset = optional_string_member "preset" saved_state in
              let cache_critical_files =
                list_member ~f:to_string "cache_critical_files" saved_state
              in
              Result.Ok
                {
                  Configuration.StaticAnalysis.SavedState.watchman_root;
                  project_name;
                  preset;
                  cache_critical_files;
                })
          >>= fun saved_state ->
          Result.Ok
            {
              base;
              pyrefly_results;
              dump_call_graph;
              dump_model_query_results;
              find_missing_flows;
              infer_self_tito;
              infer_argument_tito;
              maximum_model_source_tree_width;
              maximum_model_sink_tree_width;
              maximum_model_tito_tree_width;
              maximum_tree_depth_after_widening;
              maximum_return_access_path_width;
              maximum_return_access_path_depth_after_widening;
              maximum_tito_collapse_depth;
              maximum_tito_positions;
              maximum_overrides_to_analyze;
              maximum_trace_length;
              maximum_tito_depth;
              no_verify;
              verify_dsl;
              verify_taint_config_only;
              repository_root;
              rule_filter;
              source_filter;
              sink_filter;
              transform_filter;
              save_results_to;
              output_format;
              strict;
              taint_model_paths;
              use_cache;
              build_cache_only;
              check_invariants;
              limit_entrypoints;
              compact_ocaml_heap;
              saved_state;
              compute_coverage;
              scheduler_policies;
              higher_order_call_graph_max_iterations;
              maximum_target_depth;
              maximum_parameterized_targets_at_call_site;
            }
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
            enable_type_comments;
            remote_logging = _;
            profiling_output = _;
            memory_profiling_output = _;
          };
        dump_call_graph;
        dump_model_query_results;
        find_missing_flows;
        maximum_model_source_tree_width;
        maximum_model_sink_tree_width;
        maximum_model_tito_tree_width;
        maximum_tree_depth_after_widening;
        maximum_return_access_path_width;
        maximum_return_access_path_depth_after_widening;
        maximum_tito_collapse_depth;
        maximum_tito_positions;
        maximum_overrides_to_analyze;
        maximum_trace_length;
        maximum_tito_depth;
        no_verify;
        verify_dsl;
        verify_taint_config_only;
        rule_filter;
        source_filter;
        sink_filter;
        transform_filter;
        save_results_to;
        output_format;
        pyrefly_results;
        strict;
        taint_model_paths;
        use_cache;
        build_cache_only;
        infer_self_tito;
        infer_argument_tito;
        repository_root;
        check_invariants;
        limit_entrypoints;
        compact_ocaml_heap;
        saved_state;
        compute_coverage;
        scheduler_policies;
        higher_order_call_graph_max_iterations;
        maximum_target_depth;
        maximum_parameterized_targets_at_call_site;
      }
    =
    let configuration =
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
        ~taint_model_paths
        ~strict
        ~debug
        ~show_error_traces:false
        ~excludes
        ~extensions
        ~store_type_errors:false
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
    in
    {
      Configuration.StaticAnalysis.configuration;
      repository_root;
      save_results_to;
      output_format;
      pyrefly_results;
      dump_call_graph;
      verify_models = not no_verify;
      verify_dsl;
      verify_taint_config_only;
      rule_filter;
      source_filter;
      sink_filter;
      transform_filter;
      find_missing_flows;
      dump_model_query_results;
      use_cache;
      build_cache_only;
      infer_self_tito;
      infer_argument_tito;
      maximum_model_source_tree_width;
      maximum_model_sink_tree_width;
      maximum_model_tito_tree_width;
      maximum_tree_depth_after_widening;
      maximum_return_access_path_width;
      maximum_return_access_path_depth_after_widening;
      maximum_tito_collapse_depth;
      maximum_tito_positions;
      maximum_overrides_to_analyze;
      maximum_trace_length;
      maximum_tito_depth;
      check_invariants;
      limit_entrypoints;
      compact_ocaml_heap;
      saved_state;
      compute_coverage;
      scheduler_policies;
      higher_order_call_graph_max_iterations =
        Option.value
          higher_order_call_graph_max_iterations
          ~default:Configuration.StaticAnalysis.default_higher_order_call_graph_max_iterations;
      maximum_target_depth =
        Option.value
          maximum_target_depth
          ~default:Configuration.StaticAnalysis.default_maximum_target_depth;
      maximum_parameterized_targets_at_call_site;
    }
end

let with_performance_tracking ~debug ~f =
  let timer = Timer.start () in
  let result = f () in
  if debug then (
    let { Stdlib.Gc.minor_collections; major_collections; compactions; _ } =
      Stdlib.Gc.quick_stat ()
    in
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
    Memory.report_statistics ())
  else
    Statistics.performance ~name:"analyze" ~timer ();
  result


let verify_configuration ~static_analysis_configuration () =
  let (_ : Taint.TaintConfiguration.Heap.t) =
    TaintAnalysis.initialize_and_verify_configuration ~static_analysis_configuration
  in
  ()


let run_analyze analyze_configuration =
  let { AnalyzeConfiguration.base = { CommandStartup.BaseConfiguration.source_paths; debug; _ }; _ }
    =
    analyze_configuration
  in
  let ({ Configuration.StaticAnalysis.configuration = analysis_configuration; _ } as
      static_analysis_configuration)
    =
    AnalyzeConfiguration.analysis_configuration_of analyze_configuration
  in
  Server.BuildSystem.with_build_system source_paths ~f:(fun build_system ->
      Scheduler.with_scheduler
        ~configuration:analysis_configuration
        ~should_log_exception:(function
          | Taint.TaintConfiguration.TaintConfigurationError _ -> false
          | Taint.ModelVerificationError.ModelVerificationErrors _ -> false
          | Taint.Cache.BuildCacheOnly -> false
          | Interprocedural.PyreflyApi.PyreflyFileFormatError _ -> false
          | _ -> true)
        ~f:(fun scheduler ->
          with_performance_tracking ~debug ~f:(fun () ->
              if static_analysis_configuration.verify_taint_config_only then
                verify_configuration ~static_analysis_configuration ()
              else
                TaintAnalysis.run_taint_analysis
                  ~static_analysis_configuration
                  ~lookup_source:(Server.BuildSystem.lookup_source build_system)
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
  | Taint.Cache.BuildCacheOnly -> ExitStatus.CheckStatus CheckCommand.ExitStatus.Ok
  | Interprocedural.PyreflyApi.PyreflyFileFormatError { path; error } ->
      Log.error "%a: %a" PyrePath.pp path Interprocedural.PyreflyApi.Error.pp error;
      ExitStatus.PyreflyFileFormatError
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


let doc = "Runs taint analysis"

let command ?(name = "analyze") () =
  let open Cmdliner in
  let filename = Arg.(required & pos 0 (some string) None & info [] ~docv:"filename") in
  let term = Term.(const run_analyze $ filename) in
  let info = Cmd.info name ~doc in
  Cmd.v info term
