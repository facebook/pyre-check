(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Taint
module Target = Interprocedural.Target

let initialize_configuration
    ~static_analysis_configuration:
      { Configuration.StaticAnalysis.configuration = { taint_model_paths; _ }; _ }
  =
  (* In order to save time, sanity check models before starting the analysis. *)
  Log.info "Verifying model syntax and configuration.";
  let timer = Timer.start () in
  ModelParser.get_model_sources ~paths:taint_model_paths
  |> List.iter ~f:(fun (path, source) -> ModelParser.verify_model_syntax ~path ~source);
  let (_ : TaintConfiguration.t) =
    TaintConfiguration.create
      ~rule_filter:None
      ~find_missing_flows:None
      ~dump_model_query_results_path:None
      ~maximum_trace_length:None
      ~maximum_tito_depth:None
      ~taint_model_paths
    |> TaintConfiguration.exception_on_error
  in
  Statistics.performance
    ~name:"Verified model syntax and configuration"
    ~phase_name:"Verifying model syntax and configuration"
    ~timer
    ()


let parse_and_save_decorators_to_skip
    ~inline_decorators
    { Configuration.Analysis.taint_model_paths; _ }
  =
  Analysis.InlineDecorator.set_should_inline_decorators inline_decorators;
  if inline_decorators then (
    let timer = Timer.start () in
    Log.info "Getting decorators to skip when inlining...";
    let model_sources = ModelParser.get_model_sources ~paths:taint_model_paths in
    let decorators_to_skip =
      List.concat_map model_sources ~f:(fun (path, source) ->
          Analysis.InlineDecorator.decorators_to_skip ~path source)
    in
    List.iter decorators_to_skip ~f:(fun decorator ->
        Analysis.InlineDecorator.DecoratorsToSkip.add decorator decorator);
    Statistics.performance
      ~name:"Getting decorators to skip when inlining"
      ~phase_name:"Getting decorators to skip when inlining"
      ~timer
      ())


(** Perform a full type check and build a type environment. *)
let type_check ~scheduler ~configuration ~cache =
  Cache.type_environment cache (fun () ->
      let configuration =
        (* In order to get an accurate call graph and type information, we need to ensure that we
           schedule a type check for external files. *)
        { configuration with Configuration.Analysis.analyze_external_sources = true }
      in
      let type_environment =
        Analysis.EnvironmentControls.create ~populate_call_graph:false configuration
        |> Analysis.ErrorsEnvironment.create
        |> Analysis.ErrorsEnvironment.type_environment
      in
      let () = Analysis.TypeEnvironment.populate_for_project_modules ~scheduler type_environment in
      type_environment)


let join_parse_result
    {
      ModelParser.models = models_left;
      queries = queries_left;
      skip_overrides = skip_overrides_left;
      errors = errors_left;
    }
    {
      ModelParser.models = models_right;
      queries = queries_right;
      skip_overrides = skip_overrides_right;
      errors = errors_right;
    }
  =
  {
    ModelParser.models = Registry.merge ~join:Model.join_user_models models_left models_right;
    queries = List.rev_append queries_right queries_left;
    errors = List.rev_append errors_right errors_left;
    skip_overrides = Set.union skip_overrides_left skip_overrides_right;
  }


let parse_models_and_queries_from_sources
    ~static_analysis_configuration:{ Configuration.StaticAnalysis.rule_filter; _ }
    ~scheduler
    ~resolution
    ~callables
    ~stubs
    ~taint_configuration
    sources
  =
  (* TODO(T117715045): Do not pass all callables and stubs explicitly to map_reduce,
   * since this will marshal-ed between processes and hence is costly. *)
  let map state sources =
    List.fold sources ~init:state ~f:(fun state (path, source) ->
        ModelParser.parse
          ~resolution
          ~path
          ~source
          ~configuration:taint_configuration
          ~callables
          ~stubs
          ?rule_filter
          ()
        |> join_parse_result state)
  in
  Scheduler.map_reduce
    scheduler
    ~policy:(Scheduler.Policy.legacy_fixed_chunk_count ())
    ~initial:
      {
        ModelParser.models = Registry.empty;
        queries = [];
        skip_overrides = Ast.Reference.Set.empty;
        errors = [];
      }
    ~map
    ~reduce:join_parse_result
    ~inputs:sources
    ()


let parse_models_and_queries_from_configuration
    ~scheduler
    ~static_analysis_configuration:
      ({
         Configuration.StaticAnalysis.verify_models;
         configuration = { taint_model_paths; _ };
         rule_filter;
         find_missing_flows;
         dump_model_query_results;
         maximum_trace_length;
         maximum_tito_depth;
         _;
       } as static_analysis_configuration)
    ~environment
    ~callables
    ~stubs
  =
  let resolution =
    Analysis.TypeCheck.resolution
      (Analysis.TypeEnvironment.ReadOnly.global_resolution environment)
      (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
      (module Analysis.TypeCheck.DummyContext)
  in
  let find_missing_flows =
    find_missing_flows >>= TaintConfiguration.missing_flows_kind_from_string
  in
  let taint_configuration =
    TaintConfiguration.create
      ~rule_filter
      ~find_missing_flows
      ~dump_model_query_results_path:dump_model_query_results
      ~maximum_trace_length
      ~maximum_tito_depth
      ~taint_model_paths
    |> TaintConfiguration.exception_on_error
  in
  let () = TaintConfiguration.register taint_configuration in
  let { ModelParser.models = user_models; errors; skip_overrides; queries } =
    ModelParser.get_model_sources ~paths:taint_model_paths
    |> parse_models_and_queries_from_sources
         ~static_analysis_configuration
         ~scheduler
         ~resolution
         ~callables
         ~stubs
         ~taint_configuration
  in
  ModelVerificationError.verify_models_and_dsl errors verify_models;
  let class_models = ClassModels.infer ~environment ~user_models in
  {
    ModelParser.models = Registry.merge ~join:Model.join_user_models user_models class_models;
    queries;
    skip_overrides;
    errors;
  }


let initialize_models ~scheduler ~static_analysis_configuration ~environment ~callables ~stubs =
  let stubs = Target.HashSet.of_list stubs in

  Log.info "Parsing taint models...";
  let timer = Timer.start () in
  let { ModelParser.models; queries; skip_overrides; errors } =
    parse_models_and_queries_from_configuration
      ~scheduler
      ~static_analysis_configuration
      ~environment
      ~callables:(Some (Target.HashSet.of_list callables))
      ~stubs
  in
  Statistics.performance ~name:"Parsed taint models" ~phase_name:"Parsing taint models" ~timer ();

  let models =
    match queries with
    | [] -> models
    | _ ->
        Log.info "Generating models from model queries...";
        let timer = Timer.start () in
        let taint_configuration = TaintConfiguration.get () in
        let models_and_names, errors =
          TaintModelQuery.ModelQuery.generate_models_from_queries
            ~static_analysis_configuration
            ~scheduler
            ~environment
            ~callables
            ~stubs
            ~taint_configuration
            queries
        in
        let () =
          match taint_configuration.dump_model_query_results_path with
          | Some path ->
              TaintModelQuery.ModelQuery.DumpModelQueryResults.dump_to_file ~models_and_names ~path
          | None -> ()
        in
        ModelVerificationError.verify_models_and_dsl errors static_analysis_configuration.verify_dsl;
        let models =
          models_and_names
          |> TaintModelQuery.ModelQuery.ModelQueryRegistryMap.get_registry
               ~model_join:Model.join_user_models
          |> Registry.merge ~join:Model.join_user_models models
        in
        Statistics.performance
          ~name:"Generated models from model queries"
          ~phase_name:"Generating models from model queries"
          ~timer
          ();
        models
  in

  let models =
    MissingFlow.add_obscure_models
      ~static_analysis_configuration
      ~environment
      ~stubs
      ~initial_models:models
  in

  { ModelParser.models; skip_overrides; queries = []; errors }


(** Aggressively remove things we do not need anymore from the shared memory. *)
let purge_shared_memory ~environment ~qualifiers =
  let ast_environment = Analysis.TypeEnvironment.ast_environment environment in
  Analysis.AstEnvironment.remove_sources ast_environment qualifiers;
  Memory.SharedMemory.collect `aggressive;
  ()


let run_taint_analysis
    ~static_analysis_configuration:
      ({
         Configuration.StaticAnalysis.configuration;
         repository_root;
         inline_decorators;
         use_cache;
         _;
       } as static_analysis_configuration)
    ~build_system
    ~scheduler
    ()
  =
  try
    let () = initialize_configuration ~static_analysis_configuration in

    (* Collect decorators to skip before type-checking because decorator inlining happens in an
       early phase of type-checking and needs to know which decorators to skip. *)
    let () = parse_and_save_decorators_to_skip ~inline_decorators configuration in

    let cache = Cache.load ~scheduler ~configuration ~enabled:use_cache in

    let environment = type_check ~scheduler ~configuration ~cache in

    let qualifiers =
      Analysis.TypeEnvironment.module_tracker environment
      |> Analysis.ModuleTracker.read_only
      |> Analysis.ModuleTracker.ReadOnly.tracked_explicit_modules
    in

    let read_only_environment = Analysis.TypeEnvironment.read_only environment in

    let class_hierarchy_graph =
      Cache.class_hierarchy_graph cache (fun () ->
          let timer = Timer.start () in
          let class_hierarchy_graph =
            Interprocedural.ClassHierarchyGraph.from_qualifiers
              ~scheduler
              ~environment:read_only_environment
              ~qualifiers
          in
          Statistics.performance
            ~name:"Computed class hierarchy graph"
            ~phase_name:"Computing class hierarchy graph"
            ~timer
            ();
          class_hierarchy_graph)
    in

    let class_interval_graph =
      let timer = Timer.start () in
      let class_interval_graph =
        Interprocedural.ClassIntervalSetGraph.Heap.from_class_hierarchy class_hierarchy_graph
        |> Interprocedural.ClassIntervalSetGraph.SharedMemory.from_heap
      in
      Statistics.performance
        ~name:"Computed class intervals"
        ~phase_name:"Computing class intervals"
        ~timer
        ();
      class_interval_graph
    in

    let initial_callables =
      Cache.initial_callables cache (fun () ->
          let timer = Timer.start () in
          let initial_callables =
            Interprocedural.FetchCallables.from_qualifiers
              ~scheduler
              ~configuration
              ~environment:read_only_environment
              ~include_unit_tests:false
              ~qualifiers
          in
          Statistics.performance
            ~name:"Fetched initial callables to analyze"
            ~phase_name:"Fetching initial callables to analyze"
            ~timer
            ();
          initial_callables)
    in

    let { ModelParser.models = initial_models; skip_overrides; _ } =
      initialize_models
        ~scheduler
        ~static_analysis_configuration
        ~environment:(Analysis.TypeEnvironment.read_only environment)
        ~callables:(Interprocedural.FetchCallables.get_callables initial_callables)
        ~stubs:(Interprocedural.FetchCallables.get_stubs initial_callables)
    in

    let ast_environment =
      environment
      |> Analysis.TypeEnvironment.read_only
      |> Analysis.TypeEnvironment.ReadOnly.ast_environment
    in

    Log.info "Computing overrides...";
    let timer = Timer.start () in
    let {
      Interprocedural.OverrideGraph.override_graph_heap;
      override_graph_shared_memory;
      skipped_overrides;
    }
      =
      Cache.override_graph cache (fun () ->
          Interprocedural.OverrideGraph.build_whole_program_overrides
            ~scheduler
            ~environment:(Analysis.TypeEnvironment.read_only environment)
            ~include_unit_tests:false
            ~skip_overrides
            ~maximum_overrides:(TaintConfiguration.get_maximum_overrides_to_analyze ())
            ~qualifiers)
    in
    Statistics.performance ~name:"Overrides computed" ~phase_name:"Computing overrides" ~timer ();

    Log.info "Building call graph...";
    let timer = Timer.start () in
    let { Interprocedural.CallGraph.whole_program_call_graph; define_call_graphs } =
      Interprocedural.CallGraph.build_whole_program_call_graph
        ~scheduler
        ~static_analysis_configuration
        ~environment:(Analysis.TypeEnvironment.read_only environment)
        ~override_graph:override_graph_shared_memory
        ~store_shared_memory:true
        ~attribute_targets:(Registry.object_targets initial_models)
        ~callables:(Interprocedural.FetchCallables.get_callables initial_callables)
    in
    Statistics.performance ~name:"Call graph built" ~phase_name:"Building call graph" ~timer ();

    Log.info "Computing dependencies...";
    let timer = Timer.start () in
    let {
      Interprocedural.DependencyGraph.dependency_graph;
      override_targets;
      callables_kept;
      callables_to_analyze;
    }
      =
      Interprocedural.DependencyGraph.build_whole_program_dependency_graph
        ~prune:true
        ~initial_callables
        ~call_graph:whole_program_call_graph
        ~overrides:override_graph_heap
    in
    Statistics.performance
      ~name:"Computed dependencies"
      ~phase_name:"Computing dependencies"
      ~timer
      ();

    let initial_models =
      MissingFlow.add_unknown_callee_models
        ~static_analysis_configuration
        ~call_graph:whole_program_call_graph
        ~initial_models
    in

    Log.info "Purging shared memory...";
    let timer = Timer.start () in
    let () = purge_shared_memory ~environment ~qualifiers in
    Statistics.performance
      ~name:"Purged shared memory"
      ~phase_name:"Purging shared memory"
      ~timer
      ();

    Log.info
      "Analysis fixpoint started for %d overrides and %d functions..."
      (List.length override_targets)
      (List.length callables_kept);
    let fixpoint_timer = Timer.start () in
    let fixpoint_state =
      Taint.Fixpoint.compute
        ~scheduler
        ~type_environment:(Analysis.TypeEnvironment.read_only environment)
        ~override_graph:override_graph_shared_memory
        ~dependency_graph
        ~context:
          {
            Taint.Fixpoint.Context.type_environment = Analysis.TypeEnvironment.read_only environment;
            class_interval_graph;
            define_call_graphs;
          }
        ~initial_callables:(Interprocedural.FetchCallables.get_callables initial_callables)
        ~stubs:(Interprocedural.FetchCallables.get_stubs initial_callables)
        ~override_targets
        ~callables_to_analyze
        ~initial_models
        ~max_iterations:100
        ~epoch:Taint.Fixpoint.Epoch.initial
    in

    let filename_lookup path_reference =
      match Server.PathLookup.instantiate_path ~build_system ~ast_environment path_reference with
      | None -> None
      | Some full_path ->
          let root = Option.value repository_root ~default:configuration.local_root in
          PyrePath.get_relative_to_root ~root ~path:(PyrePath.create_absolute full_path)
    in
    let callables =
      Target.Set.of_list (List.rev_append (Registry.targets initial_models) callables_to_analyze)
    in
    let summary =
      Reporting.report
        ~scheduler
        ~static_analysis_configuration
        ~filename_lookup
        ~override_graph:override_graph_shared_memory
        ~callables
        ~skipped_overrides
        ~fixpoint_timer
        ~fixpoint_state
    in
    Yojson.Safe.pretty_to_string (`List summary) |> Log.print "%s"
  with
  | (TaintConfiguration.TaintConfigurationError _ | ModelVerificationError.ModelVerificationErrors _)
    as exn ->
      raise exn
  | exn ->
      (* The backtrace is lost if the exception is caught at the top level, because of `Lwt`.
       * Let's print the exception here to ease debugging. *)
      Log.log_exception "Taint analysis failed." exn (Worker.exception_backtrace exn);
      raise exn
