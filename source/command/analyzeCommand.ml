(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre

let get_analysis_kind = function
  | "taint" -> TaintAnalysis.abstract_kind
  | _ ->
      Log.error "Invalid analysis kind specified.";
      failwith "bad argument"


let run_analysis
    analysis
    result_json_path
    no_verify
    dump_call_graph
    repository_root
    rule_filter
    find_missing_flows
    dump_model_query_results
    use_cache
    inline_decorators
    maximum_trace_length
    maximum_tito_depth
    _verbose
    expected_version
    sections
    debug
    strict
    show_error_traces
    sequential
    filter_directories
    ignore_all_errors
    number_of_workers
    log_identifier
    logger
    profiling_output
    memory_profiling_output
    project_root
    source_path
    search_path
    taint_models_paths
    excludes
    extensions
    log_directory
    python_major_version
    python_minor_version
    python_micro_version
    shared_memory_heap_size
    shared_memory_dependency_table_power
    shared_memory_hash_table_power
    local_root
    ()
  =
  try
    Log.GlobalState.initialize ~debug ~sections;
    let source_path = Option.value source_path ~default:[local_root] in
    let local_root = SearchPath.create local_root |> SearchPath.get_root in
    Statistics.GlobalState.initialize
      ~log_identifier
      ?logger
      ~project_name:(Path.last local_root)
      ();
    Profiling.GlobalState.initialize ?profiling_output ?memory_profiling_output ();
    let filter_directories =
      filter_directories
      >>| String.split_on_chars ~on:[';']
      >>| List.map ~f:String.strip
      >>| List.map ~f:(Path.create_absolute ~follow_symbolic_links:true)
    in
    let ignore_all_errors =
      ignore_all_errors
      >>| String.split_on_chars ~on:[';']
      >>| List.map ~f:String.strip
      >>| List.map ~f:(Path.create_absolute ~follow_symbolic_links:true)
    in
    let repository_root = repository_root >>| Path.create_absolute ~follow_symbolic_links:true in
    let configuration =
      Configuration.Analysis.create
        ?expected_version
        ~debug
        ~strict
        ~show_error_traces
        ~project_root:(Path.create_absolute ~follow_symbolic_links:true project_root)
        ~parallel:(not sequential)
        ?filter_directories
        ?ignore_all_errors
        ~number_of_workers
        ~search_path:(List.map search_path ~f:SearchPath.create_normalized)
        ~taint_model_paths:
          (List.map taint_models_paths ~f:(Path.create_absolute ~follow_symbolic_links:true))
        ~excludes
        ~extensions:(List.map ~f:Configuration.Extension.create_extension extensions)
        ?log_directory
        ?python_major_version
        ?python_minor_version
        ?python_micro_version
        ?shared_memory_heap_size
        ?shared_memory_dependency_table_power
        ?shared_memory_hash_table_power
        ~local_root
        ~source_path:(List.map source_path ~f:SearchPath.create_normalized)
        ()
    in
    let static_analysis_configuration =
      let result_json_path = result_json_path >>| Path.create_absolute in
      let dump_call_graph = dump_call_graph >>| Path.create_absolute in
      let dump_model_query_results = dump_model_query_results >>| Path.create_absolute in
      let () =
        match result_json_path with
        | Some path when not (Path.is_directory path) ->
            Log.error "--save-results-to path must be a directory.";
            failwith "bad argument"
        | _ -> ()
      in
      {
        Configuration.StaticAnalysis.configuration;
        result_json_path;
        dump_call_graph;
        verify_models = not no_verify;
        rule_filter;
        find_missing_flows;
        dump_model_query_results;
        use_cache;
        maximum_trace_length;
        maximum_tito_depth;
      }
    in
    let analysis_kind = get_analysis_kind analysis in
    (fun () ->
      let timer = Timer.start () in
      Scheduler.with_scheduler ~configuration ~f:(fun scheduler ->
          Interprocedural.FixpointAnalysis.initialize_configuration
            ~static_analysis_configuration
            analysis_kind;

          let environment =
            Service.StaticAnalysis.type_check ~scheduler ~configuration ~use_cache
          in

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
            match repository_root with
            | Some root ->
                Analysis.AstEnvironment.ReadOnly.get_real_path
                  ~configuration
                  ast_environment
                  path_reference
                >>= Pyre.Path.follow_symbolic_link
                >>= fun path -> Pyre.Path.get_relative_to_root ~root ~path
            | None ->
                Analysis.AstEnvironment.ReadOnly.get_real_path_relative
                  ~configuration
                  ast_environment
                  path_reference
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
            ();
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
            ()))
    |> Scheduler.run_process
  with
  | error ->
      Log.log_exception error;
      raise error


let command =
  Command.basic_spec
    ~summary:"Runs a static analysis without a server (default)."
    Command.Spec.(
      empty
      +> flag "-analysis" (optional_with_default "taint" string) ~doc:"Type of analysis to run."
      +> flag
           "-save-results-to"
           (optional string)
           ~doc:"Directory where pyre analyze will save its results"
      +> flag
           "-no-verify"
           no_arg
           ~doc:"Do not verify that all models passed into the analysis are valid."
      +> flag "-dump-call-graph" (optional string) ~doc:"Dump the call graph in the given file."
      +> flag
           "-repository-root"
           (optional string)
           ~doc:"The repository root to use for path relativization (set to local root if missing)."
      +> flag
           "-rules"
           (optional (Arg_type.comma_separated int))
           ~doc:"If set, filter the analysis to only consider the provided rule numbers."
      +> flag
           "-find-missing-flows"
           (optional string)
           ~doc:"Perform a taint analysis to find missing flows."
      +> flag
           "-dump-model-query-results"
           (optional string)
           ~doc:"Dump model query results in the given file."
      +> flag "-use-cache" no_arg ~doc:"Store information in .pyre/pysa.cache for faster runs."
      +> flag
           "-inline-decorators"
           no_arg
           ~doc:"Inline decorators at use sites to catch flows through the decorators."
      +> flag "-maximum-trace-length" (optional int) ~doc:"Limit the trace length of taint flows."
      +> flag
           "-maximum-tito-depth"
           (optional int)
           ~doc:"Limit the depth of inferred taint-in-taint-out in taint flows."
      ++ Specification.base_command_line_arguments)
    run_analysis
