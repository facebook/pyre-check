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
  | "type_inference" -> TypeInference.Analysis.abstract_kind
  | _ ->
      Log.error "Invalid analysis kind specified.";
      failwith "bad argument"


let should_infer analysis = String.equal analysis "type_inference"

let type_environment_with_decorators_inlined ~configuration ~scheduler environment =
  let open Analysis in
  let open Interprocedural in
  let open Ast in
  let decorator_bodies =
    DecoratorHelper.all_decorator_bodies (TypeEnvironment.read_only environment)
  in
  let environment =
    AstEnvironment.create
      ~additional_preprocessing:
        (DecoratorHelper.inline_decorators
           ~environment:(TypeEnvironment.read_only environment)
           ~decorator_bodies)
      (AstEnvironment.module_tracker (TypeEnvironment.ast_environment environment))
    |> AnnotatedGlobalEnvironment.create
    |> TypeEnvironment.create
  in
  let all_internal_paths =
    let get_internal_path source_path =
      let path = SourcePath.full_path ~configuration source_path in
      Option.some_if (SourcePath.is_internal_path ~configuration path) path
    in
    ModuleTracker.source_paths
      (AstEnvironment.module_tracker (TypeEnvironment.ast_environment environment))
    |> List.filter_map ~f:get_internal_path
  in
  let _ =
    Server.IncrementalCheck.recheck
      ~configuration
      ~scheduler
      ~environment
      ~errors:(Ast.Reference.Table.create ())
      all_internal_paths
  in
  environment


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
        ~infer:(should_infer analysis)
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
        ~local_root
        ~source_path:(List.map source_path ~f:SearchPath.create_normalized)
        ()
    in
    let result_json_path = result_json_path >>| Path.create_absolute in
    let () =
      match result_json_path with
      | Some path when not (Path.is_directory path) ->
          Log.error "--save-results-to path must be a directory.";
          failwith "bad argument"
      | _ -> ()
    in
    (fun () ->
      let timer = Timer.start () in
      (* In order to save time, sanity check models before starting the analysis. *)
      Log.info "Verifying model syntax and configuration.";
      Taint.Model.get_model_sources ~paths:configuration.Configuration.Analysis.taint_model_paths
      |> List.iter ~f:(fun (path, source) -> Taint.Model.verify_model_syntax ~path ~source);
      Taint.TaintConfiguration.create
        ~rule_filter:None
        ~paths:configuration.Configuration.Analysis.taint_model_paths
      |> ignore;
      Scheduler.with_scheduler ~configuration ~f:(fun scheduler ->
          let cached_environment =
            if use_cache then Service.StaticAnalysis.Cache.load_environment ~configuration else None
          in
          let environment =
            match cached_environment with
            | Some loaded_environment ->
                Log.warning "Using cached type environment.";
                loaded_environment
            | _ ->
                let configuration =
                  (* In order to get an accurate call graph and type information, we need to ensure
                     that we schedule a type check for external files. *)
                  { configuration with analyze_external_sources = true }
                in
                Log.info "No cached type environment loaded, starting a clean run.";
                Service.Check.check
                  ~scheduler
                  ~configuration
                  ~call_graph_builder:(module Analysis.Callgraph.NullBuilder)
                |> fun { environment; _ } ->
                if use_cache then
                  Service.StaticAnalysis.Cache.save_environment ~configuration ~environment;
                environment
          in
          let environment =
            if inline_decorators then (
              Log.info "Inlining decorators for taint analysis...";
              type_environment_with_decorators_inlined ~configuration ~scheduler environment )
            else
              environment
          in
          let ast_environment =
            Analysis.TypeEnvironment.ast_environment environment
            |> Analysis.AstEnvironment.read_only
          in
          let qualifiers =
            Analysis.TypeEnvironment.module_tracker environment
            |> Analysis.ModuleTracker.tracked_explicit_modules
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
            ~analysis_kind:(get_analysis_kind analysis)
            ~static_analysis_configuration:
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
              }
            ~filename_lookup
            ~environment:(Analysis.TypeEnvironment.read_only environment)
            ~qualifiers
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
           ~doc:"file A JSON file that Pyre Analyze will save its' results to."
      +> flag
           "-no-verify"
           no_arg
           ~doc:"Do not verify that all models passed into the analysis are valid."
      +> flag "-dump-call-graph" no_arg ~doc:"Store call graph in .pyre/call_graph.json"
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
      +> flag "-dump-model-query-results" no_arg ~doc:"Provide debugging output for model queries."
      +> flag "-use-cache" no_arg ~doc:"Store information in .pyre/pysa.cache for faster runs."
      +> flag
           "-inline-decorators"
           no_arg
           ~doc:"Inline decorators at use sites to catch flows through the decorators."
      +> flag "-maximum-trace-length" (optional int) ~doc:"Limit the trace length of taint flows."
      ++ Specification.base_command_line_arguments)
    run_analysis
