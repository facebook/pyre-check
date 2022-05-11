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


(* TODO(T117715045): This should go in a generic `registry` module. *)
let merge_model_maps left right =
  Target.Map.merge left right ~f:(fun ~key:_ -> function
    | `Both (left, right) -> Some (Model.join left right)
    | `Left model
    | `Right model ->
        Some model)


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
    ModelParser.models = merge_model_maps models_left models_right;
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
        ModelParser.models = Target.Map.empty;
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
  let () = ModelVerificationError.register errors in
  let () =
    if not (List.is_empty errors) then
      (* Exit or log errors, depending on whether models need to be verified. *)
      if not verify_models then begin
        Log.error "Found %d model verification errors!" (List.length errors);
        List.iter errors ~f:(fun error -> Log.error "%s" (ModelVerificationError.display error))
      end
      else
        raise (ModelParser.ModelVerificationError errors)
  in
  let class_models = ClassModels.infer ~environment ~user_models in
  {
    ModelParser.models = merge_model_maps user_models class_models;
    queries;
    skip_overrides;
    errors;
  }


let generate_models_from_queries
    ~static_analysis_configuration:{ Configuration.StaticAnalysis.rule_filter; _ }
    ~scheduler
    ~environment
    ~callables
    ~stubs
    ~taint_configuration
    ~initial_models
    queries
  =
  let resolution =
    Analysis.TypeCheck.resolution
      (Analysis.TypeEnvironment.ReadOnly.global_resolution environment)
      (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
      (module Analysis.TypeCheck.DummyContext)
  in
  let callables =
    Hash_set.fold stubs ~f:(Core.Fn.flip List.cons) ~init:callables
    |> List.filter_map ~f:(function
           | Target.Function _ as callable -> Some callable
           | Target.Method _ as callable -> Some callable
           | _ -> None)
  in
  TaintModelQuery.ModelQuery.apply_all_rules
    ~resolution
    ~scheduler
    ~configuration:taint_configuration
    ~rule_filter
    ~rules:queries
    ~callables
    ~stubs
    ~environment
    ~models:initial_models


(* Registers the Taint analysis with the interprocedural analysis framework. *)
include Taint.Result.Register (struct
  include Taint.Result

  let initialize_models ~scheduler ~static_analysis_configuration ~environment ~callables ~stubs =
    let stubs = Target.HashSet.of_list stubs in

    Log.info "Parsing taint models...";
    let timer = Timer.start () in
    let { ModelParser.models; queries; skip_overrides; _ } =
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
          let models =
            generate_models_from_queries
              ~static_analysis_configuration
              ~scheduler
              ~environment
              ~callables
              ~stubs
              ~initial_models:models
              ~taint_configuration:(TaintConfiguration.get ())
              queries
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

    { Interprocedural.AnalysisResult.initial_models = models; skip_overrides }


  let analyze ~environment ~callable ~qualifier ~define ~sanitizers ~modes existing_model =
    let profiler =
      if Ast.Statement.Define.dump_perf (Ast.Node.value define) then
        TaintProfiler.create ()
      else
        TaintProfiler.none
    in
    let call_graph_of_define =
      match Interprocedural.CallGraph.SharedMemory.get ~callable with
      | Some call_graph -> call_graph
      | None -> Format.asprintf "Missing call graph for `%a`" Target.pp callable |> failwith
    in
    let forward, result, triggered_sinks =
      TaintProfiler.track_duration ~profiler ~name:"Forward analysis" ~f:(fun () ->
          ForwardAnalysis.run
            ~profiler
            ~environment
            ~qualifier
            ~define
            ~call_graph_of_define
            ~existing_model)
    in
    let backward =
      TaintProfiler.track_duration ~profiler ~name:"Backward analysis" ~f:(fun () ->
          BackwardAnalysis.run
            ~profiler
            ~environment
            ~qualifier
            ~define
            ~call_graph_of_define
            ~existing_model
            ~triggered_sinks)
    in
    let forward, backward =
      if Model.ModeSet.contains Model.Mode.SkipAnalysis modes then
        empty_model.forward, empty_model.backward
      else
        forward, backward
    in
    let model = { Model.forward; backward; sanitizers; modes } in
    let model =
      TaintProfiler.track_duration ~profiler ~name:"Sanitize" ~f:(fun () ->
          Model.apply_sanitizers model)
    in
    TaintProfiler.dump profiler;
    result, model


  let analyze
      ~environment
      ~callable
      ~qualifier
      ~define:
        ({ Ast.Node.value = { Ast.Statement.Define.signature = { name; _ }; _ }; _ } as define)
      ~existing
    =
    let define_qualifier = Ast.Reference.delocalize name in
    let open Analysis in
    let open Ast in
    let module_reference =
      let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
      let annotated_global_environment =
        GlobalResolution.annotated_global_environment global_resolution
      in
      (* Pysa inlines decorators when a function is decorated. However, we want issues and models to
         point to the lines in the module where the decorator was defined, not the module where it
         was inlined. So, look up the originating module, if any, and use that as the module
         qualifier. *)
      InlineDecorator.InlinedNameToOriginalName.get define_qualifier
      >>= AnnotatedGlobalEnvironment.ReadOnly.get_global_location annotated_global_environment
      >>| fun { Location.WithModule.module_reference; _ } -> module_reference
    in
    let qualifier = Option.value ~default:qualifier module_reference in
    match existing with
    | Some ({ Model.modes; _ } as model) when Model.ModeSet.contains Model.Mode.SkipAnalysis modes
      ->
        let () = Log.info "Skipping taint analysis of %a" Target.pp_pretty callable in
        [], model
    | Some ({ sanitizers; modes; _ } as model) ->
        analyze ~callable ~environment ~qualifier ~define ~sanitizers ~modes model
    | None ->
        analyze
          ~callable
          ~environment
          ~qualifier
          ~define
          ~sanitizers:Model.Sanitizers.empty
          ~modes:Model.ModeSet.empty
          empty_model
end)

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
    Service.StaticAnalysis.parse_and_save_decorators_to_skip ~inline_decorators configuration;
    let cache = Service.StaticAnalysis.Cache.load ~scheduler ~configuration ~enabled:use_cache in
    let environment = Service.StaticAnalysis.type_check ~scheduler ~configuration ~cache in

    let qualifiers =
      Analysis.TypeEnvironment.module_tracker environment
      |> Analysis.ModuleTracker.read_only
      |> Analysis.ModuleTracker.ReadOnly.tracked_explicit_modules
    in

    let read_only_environment = Analysis.TypeEnvironment.read_only environment in

    let class_hierarchy_graph =
      Service.StaticAnalysis.build_class_hierarchy_graph
        ~scheduler
        ~cache
        ~environment:read_only_environment
        ~qualifiers
    in

    let _ = Service.StaticAnalysis.build_class_intervals class_hierarchy_graph in

    let initial_callables =
      Service.StaticAnalysis.fetch_initial_callables
        ~scheduler
        ~configuration
        ~cache
        ~environment:read_only_environment
        ~qualifiers
    in

    let { Interprocedural.AnalysisResult.initial_models; skip_overrides } =
      let { Service.StaticAnalysis.callables_with_dependency_information; stubs; _ } =
        initial_callables
      in
      Interprocedural.FixpointAnalysis.initialize_models
        abstract_kind
        ~static_analysis_configuration
        ~scheduler
        ~environment:(Analysis.TypeEnvironment.read_only environment)
        ~callables:(List.map callables_with_dependency_information ~f:fst)
        ~stubs
    in

    let ast_environment =
      environment
      |> Analysis.TypeEnvironment.read_only
      |> Analysis.TypeEnvironment.ReadOnly.ast_environment
    in

    Log.info "Computing overrides...";
    let timer = Timer.start () in
    let { Interprocedural.DependencyGraphSharedMemory.overrides; skipped_overrides } =
      Service.StaticAnalysis.record_overrides_for_qualifiers
        ~scheduler
        ~cache
        ~environment:(Analysis.TypeEnvironment.read_only environment)
        ~skip_overrides
        ~qualifiers
    in
    let override_dependencies = Interprocedural.DependencyGraph.from_overrides overrides in
    Statistics.performance ~name:"Overrides computed" ~phase_name:"Computing overrides" ~timer ();

    Log.info "Building call graph...";
    let timer = Timer.start () in
    let callgraph =
      Service.StaticAnalysis.build_call_graph
        ~scheduler
        ~static_analysis_configuration
        ~environment:(Analysis.TypeEnvironment.read_only environment)
        ~attribute_targets:(Service.StaticAnalysis.object_targets_from_models initial_models)
        ~qualifiers
    in
    Statistics.performance ~name:"Call graph built" ~phase_name:"Building call graph" ~timer ();

    Log.info "Computing dependencies...";
    let timer = Timer.start () in
    let dependencies, callables_to_analyze, override_targets =
      Service.StaticAnalysis.build_dependency_graph
        ~callables_with_dependency_information:
          initial_callables.callables_with_dependency_information
        ~callgraph
        ~override_dependencies
    in
    Statistics.performance
      ~name:"Computed dependencies"
      ~phase_name:"Computing dependencies"
      ~timer
      ();

    let initial_models =
      MissingFlow.add_unknown_callee_models
        ~static_analysis_configuration
        ~callgraph
        ~initial_models
    in

    Log.info "Purging shared memory...";
    let timer = Timer.start () in
    let () = Service.StaticAnalysis.purge_shared_memory ~environment ~qualifiers in
    Statistics.performance
      ~name:"Purged shared memory"
      ~phase_name:"Purging shared memory"
      ~timer
      ();

    Log.info "Recording initial models in shared memory...";
    let timer = Timer.start () in
    Interprocedural.FixpointAnalysis.record_initial_models
      ~callables:(List.map initial_callables.callables_with_dependency_information ~f:fst)
      ~stubs:initial_callables.stubs
      initial_models;
    Statistics.performance
      ~name:"Recorded initial models"
      ~phase_name:"Recording initial models"
      ~timer
      ();

    Log.info
      "Analysis fixpoint started for %d overrides and %d functions..."
      (List.length override_targets)
      (List.length callables_to_analyze);
    let callables_to_analyze = List.rev_append override_targets callables_to_analyze in
    let fixpoint_timer = Timer.start () in
    let fixpoint_iterations =
      Interprocedural.FixpointAnalysis.compute_fixpoint
        ~scheduler
        ~environment:(Analysis.TypeEnvironment.read_only environment)
        ~analysis:abstract_kind
        ~dependencies
        ~filtered_callables:initial_callables.filtered_callables
        ~all_callables:callables_to_analyze
        Interprocedural.FixpointState.Epoch.initial
    in

    let filename_lookup path_reference =
      match
        Server.RequestHandler.instantiate_path ~build_system ~ast_environment path_reference
      with
      | None -> None
      | Some full_path ->
          let root = Option.value repository_root ~default:configuration.local_root in
          PyrePath.get_relative_to_root ~root ~path:(PyrePath.create_absolute full_path)
    in
    let callables =
      Target.Set.of_list (List.rev_append (Target.Map.keys initial_models) callables_to_analyze)
    in
    let summary =
      Reporting.report
        ~scheduler
        ~static_analysis_configuration
        ~filename_lookup
        ~callables
        ~skipped_overrides
        ~fixpoint_timer
        ~fixpoint_iterations:(Some fixpoint_iterations)
    in
    Yojson.Safe.pretty_to_string (`List summary) |> Log.print "%s"
  with
  | (TaintConfiguration.TaintConfigurationError _ | ModelParser.ModelVerificationError _) as exn ->
      raise exn
  | exn ->
      (* The backtrace is lost if the exception is caught at the top level, because of `Lwt`.
       * Let's print the exception here to ease debugging. *)
      Log.log_exception "Taint analysis failed." exn (Worker.exception_backtrace exn);
      raise exn
