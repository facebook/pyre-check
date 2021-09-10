(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
module Path = PyrePath

(* Analyze command uses the same exit code scheme as check command. *)
module ExitStatus = NewCheckCommand.ExitStatus

module AnalyzeConfiguration = struct
  type t = {
    base: NewCommandStartup.BaseConfiguration.t;
    dump_call_graph: bool;
    dump_model_query_results: bool;
    find_missing_flows: string option;
    inline_decorators: bool;
    maximum_tito_depth: int option;
    maximum_trace_length: int option;
    no_verify: bool;
    repository_root: Path.t option;
    rule_filter: int list option;
    save_results_to: Path.t option;
    strict: bool;
    taint_model_paths: Path.t list;
    use_cache: bool;
  }
  [@@deriving sexp, compare, hash]

  let of_yojson json =
    let open Yojson.Safe.Util in
    let open JsonParsing in
    (* Parsing logic *)
    try
      match NewCommandStartup.BaseConfiguration.of_yojson json with
      | Result.Error _ as error -> error
      | Result.Ok base ->
          let dump_call_graph = bool_member "dump_call_graph" ~default:false json in
          let dump_model_query_results =
            bool_member "dump_model_query_results" ~default:false json
          in
          let find_missing_flows = optional_string_member "find_missing_flows" json in
          let inline_decorators = bool_member "inline_decorators" ~default:false json in
          let maximum_tito_depth = optional_int_member "maximum_tito_depth" json in
          let maximum_trace_length = optional_int_member "maximum_trace_length" json in
          let no_verify = bool_member "no_verify" ~default:false json in
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
            NewCommandStartup.BaseConfiguration.source_paths;
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
        rule_filter;
        save_results_to;
        strict;
        taint_model_paths;
        use_cache;
        inline_decorators = _;
        repository_root = _;
      }
    =
    let source_path =
      match source_paths with
      | Configuration.SourcePaths.Simple source_paths -> source_paths
      | Buck { Configuration.Buck.artifact_root; _ } -> [SearchPath.Root artifact_root]
    in
    let configuration =
      Configuration.Analysis.create
        ~parallel
        ~analyze_external_sources:false
        ~filter_directories:checked_directory_allowlist
        ~ignore_all_errors:checked_directory_blocklist
        ~number_of_workers
        ~local_root:(Option.value local_root ~default:global_root)
        ~project_root:global_root
        ~search_path:(List.map search_paths ~f:SearchPath.normalize)
        ~taint_model_paths
        ~strict
        ~debug
        ~show_error_traces:false
        ~excludes
        ~extensions
        ~incremental_style:Configuration.Analysis.Shallow
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
    in
    {
      Configuration.StaticAnalysis.configuration;
      result_json_path = save_results_to;
      dump_call_graph;
      verify_models = not no_verify;
      rule_filter;
      find_missing_flows;
      dump_model_query_results;
      use_cache;
      maximum_trace_length;
      maximum_tito_depth;
    }
end

let with_performance_tracking f =
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


let run_taint_analysis
    ~static_analysis_configuration:
      ({ Configuration.StaticAnalysis.configuration; use_cache; _ } as
      static_analysis_configuration)
    ~inline_decorators
    ~build_system
    ~repository_root
    ()
  =
  let run () =
    Scheduler.with_scheduler ~configuration ~f:(fun scheduler ->
        let analysis_kind = TaintAnalysis.abstract_kind in
        Interprocedural.FixpointAnalysis.initialize_configuration
          ~static_analysis_configuration
          analysis_kind;

        let environment = Service.StaticAnalysis.type_check ~scheduler ~configuration ~use_cache in

        let qualifiers =
          Analysis.TypeEnvironment.module_tracker environment
          |> Analysis.ModuleTracker.tracked_explicit_modules
        in

        let initial_callables =
          Service.StaticAnalysis.fetch_initial_callables
            ~scheduler
            ~configuration
            ~environment:(Analysis.TypeEnvironment.read_only environment)
            ~qualifiers
            ~use_cache
        in

        let initialized_models =
          let { Service.StaticAnalysis.callables_with_dependency_information; stubs; _ } =
            initial_callables
          in
          Interprocedural.FixpointAnalysis.initialize_models
            analysis_kind
            ~static_analysis_configuration
            ~scheduler
            ~environment:(Analysis.TypeEnvironment.read_only environment)
            ~callables:(List.map callables_with_dependency_information ~f:fst)
            ~stubs
        in

        let environment, initial_callables =
          if inline_decorators then (
            Log.info "Inlining decorators for taint analysis...";
            let timer = Timer.start () in
            let { Interprocedural.AnalysisResult.InitializedModels.initial_models; _ } =
              Interprocedural.AnalysisResult.InitializedModels.get_models initialized_models
            in
            let updated_environment =
              Interprocedural.DecoratorHelper.type_environment_with_decorators_inlined
                ~configuration
                ~scheduler
                ~recheck:Server.IncrementalCheck.recheck
                ~decorators_to_skip:(Taint.Result.decorators_to_skip initial_models)
                environment
            in
            Statistics.performance
              ~name:"Inlined decorators"
              ~phase_name:"Inlining decorators"
              ~timer
              ();

            let updated_initial_callables =
              (* We need to re-fetch initial callables, since inlining creates new callables. *)
              Service.StaticAnalysis.fetch_initial_callables
                ~scheduler
                ~configuration
                ~environment:(Analysis.TypeEnvironment.read_only updated_environment)
                ~qualifiers
                ~use_cache:false
            in
            updated_environment, updated_initial_callables)
          else
            environment, initial_callables
        in

        let environment = Analysis.TypeEnvironment.read_only environment in
        let ast_environment = Analysis.TypeEnvironment.ReadOnly.ast_environment environment in

        let { Interprocedural.AnalysisResult.InitializedModels.initial_models; skip_overrides } =
          Interprocedural.AnalysisResult.InitializedModels.get_models_including_generated_models
            initialized_models
            ~updated_environment:(Some environment)
        in
        let filename_lookup path_reference =
          match
            Newserver.RequestHandler.instantiate_path
              ~build_system
              ~configuration
              ~ast_environment
              path_reference
          with
          | None -> None
          | Some full_path ->
              let root = Option.value repository_root ~default:configuration.local_root in
              Path.get_relative_to_root ~root ~path:(Path.create_absolute full_path)
        in
        Service.StaticAnalysis.analyze
          ~scheduler
          ~analysis:analysis_kind
          ~static_analysis_configuration
          ~filename_lookup
          ~environment
          ~qualifiers
          ~initial_callables
          ~initial_models
          ~skip_overrides
          ())
  in
  with_performance_tracking run


let run_analyze analyze_configuration =
  let {
    AnalyzeConfiguration.base = { NewCommandStartup.BaseConfiguration.source_paths; _ };
    inline_decorators;
    repository_root;
    _;
  }
    =
    analyze_configuration
  in
  Newserver.BuildSystem.with_build_system source_paths ~f:(fun build_system ->
      let static_analysis_configuration =
        AnalyzeConfiguration.analysis_configuration_of analyze_configuration
      in
      run_taint_analysis
        ~static_analysis_configuration
        ~build_system
        ~inline_decorators
        ~repository_root
        ();
      Lwt.return ExitStatus.Ok)


let run_analyze configuration_file =
  let exit_status =
    match
      NewCommandStartup.read_and_parse_json configuration_file ~f:AnalyzeConfiguration.of_yojson
    with
    | Result.Error message ->
        Log.error "%s" message;
        ExitStatus.PyreError
    | Result.Ok
        ({
           AnalyzeConfiguration.base =
             {
               NewCommandStartup.BaseConfiguration.global_root;
               local_root;
               debug;
               remote_logging;
               profiling_output;
               memory_profiling_output;
               _;
             };
           _;
         } as analyze_configuration) ->
        NewCommandStartup.setup_global_states
          ~global_root
          ~local_root
          ~debug
          ~additional_logging_sections:[]
          ~remote_logging
          ~profiling_output
          ~memory_profiling_output
          ();
        Lwt_main.run
          (Lwt.catch (fun () -> run_analyze analyze_configuration) NewCheckCommand.on_exception)
  in
  Statistics.flush ();
  exit (ExitStatus.exit_code exit_status)


let command =
  let filename_argument = Command.Param.(anon ("filename" %: Filename.arg_type)) in
  Command.basic
    ~summary:"Runs taint analysis"
    (Command.Param.map filename_argument ~f:(fun filename () -> run_analyze filename))
