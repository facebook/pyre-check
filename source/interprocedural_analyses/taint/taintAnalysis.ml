(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TaintAnalysis: this is the entry point of the taint analysis. *)

open Core
open Pyre
open Taint
module Target = Interprocedural.Target
module PyrePysaEnvironment = Analysis.PyrePysaEnvironment
module PyrePysaLogic = Analysis.PyrePysaLogic

let initialize_and_verify_configuration
    ~static_analysis_configuration:
      {
        Configuration.StaticAnalysis.configuration = { taint_model_paths; _ };
        rule_filter;
        source_filter;
        sink_filter;
        transform_filter;
        find_missing_flows;
        dump_model_query_results;
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
        _;
      }
  =
  let step_logger =
    StepLogger.start
      ~start_message:"Initializing and verifying taint configuration"
      ~end_message:"Initialized and verified taint configuration"
  in
  let open Core.Result in
  let taint_configuration =
    TaintConfiguration.from_taint_model_paths taint_model_paths
    >>= TaintConfiguration.with_command_line_options
          ~rule_filter
          ~source_filter
          ~sink_filter
          ~transform_filter
          ~find_missing_flows
          ~dump_model_query_results_path:dump_model_query_results
          ~infer_self_tito
          ~infer_argument_tito
          ~maximum_model_source_tree_width
          ~maximum_model_sink_tree_width
          ~maximum_model_tito_tree_width
          ~maximum_tree_depth_after_widening
          ~maximum_return_access_path_width
          ~maximum_return_access_path_depth_after_widening
          ~maximum_tito_collapse_depth
          ~maximum_tito_positions
          ~maximum_overrides_to_analyze
          ~maximum_trace_length
          ~maximum_tito_depth
    |> TaintConfiguration.exception_on_error
  in
  let () = StepLogger.finish step_logger in
  taint_configuration


let verify_model_syntax ~static_analysis_configuration =
  let step_logger =
    StepLogger.start ~start_message:"Verifying model syntax" ~end_message:"Verified model syntax"
  in
  let () =
    ModelParser.get_model_sources
      ~paths:
        static_analysis_configuration.Configuration.StaticAnalysis.configuration.taint_model_paths
    |> List.iter ~f:(fun (path, source) -> ModelParser.verify_model_syntax ~path ~source)
  in
  let () = StepLogger.finish step_logger in
  ()


let parse_model_modes
    ~static_analysis_configuration:
      {
        Configuration.StaticAnalysis.configuration = { taint_model_paths; _ };
        inline_decorators;
        _;
      }
  =
  let step_logger =
    StepLogger.start
      ~start_message:"Parsing taint models modes"
      ~end_message:"Parsed taint models modes"
  in
  let model_modes =
    ModelParser.get_model_sources ~paths:taint_model_paths
    |> List.map ~f:(fun (path, source) -> ModelParser.parse_model_modes ~path ~source)
    |> List.fold
         ~init:Ast.Reference.SerializableMap.empty
         ~f:
           (Ast.Reference.SerializableMap.union (fun _ left right ->
                Some (Model.ModeSet.join_user_modes left right)))
  in
  let decorator_preprocessing_configuration =
    {
      PyrePysaLogic.DecoratorPreprocessing.Configuration.actions =
        ModelParser.decorator_actions_from_modes model_modes;
      enable_inlining = inline_decorators;
      enable_discarding = true;
    }
  in
  let skip_type_checking_callables =
    model_modes
    |> Ast.Reference.SerializableMap.filter (fun _ modes ->
           Model.ModeSet.contains Model.Mode.SkipAnalysis modes)
    |> Ast.Reference.SerializableMap.keys
    |> Ast.Reference.SerializableSet.of_list
  in
  let () = StepLogger.finish step_logger in
  decorator_preprocessing_configuration, skip_type_checking_callables


let resolve_module_path
    ~lookup_source
    ~absolute_source_path_of_qualifier
    ~static_analysis_configuration:
      { Configuration.StaticAnalysis.configuration = { local_root; _ }; repository_root; _ }
    qualifier
  =
  match absolute_source_path_of_qualifier ~lookup_source qualifier with
  | None -> None
  | Some path ->
      let root = Option.value repository_root ~default:local_root in
      let path = PyrePath.create_absolute path in
      Some
        {
          Interprocedural.RepositoryPath.filename = PyrePath.get_relative_to_root ~root ~path;
          path;
        }


let write_modules_to_file
    ~static_analysis_configuration:
      ({ Configuration.StaticAnalysis.configuration = { local_root; _ }; _ } as
      static_analysis_configuration)
    ~lookup_source
    ~absolute_source_path_of_qualifier
    ~path
    qualifiers
  =
  let step_logger =
    StepLogger.start
      ~start_message:(Format.sprintf "Writing modules to `%s`" (PyrePath.absolute path))
      ~end_message:"Wrote modules"
  in
  let to_json_lines qualifier =
    let path =
      resolve_module_path
        ~lookup_source
        ~absolute_source_path_of_qualifier
        ~static_analysis_configuration
        qualifier
      |> function
      | Some { path; _ } -> `String (PyrePath.absolute path)
      | None -> `Null
    in
    [
      {
        Interprocedural.NewlineDelimitedJson.Line.kind = Module;
        data = `Assoc ["name", `String (Ast.Reference.show qualifier); "path", path];
      };
    ]
  in
  Interprocedural.NewlineDelimitedJson.write_file
    ~path
    ~configuration:(`Assoc ["repo", `String (PyrePath.absolute local_root)])
    ~to_json_lines
    qualifiers;
  let () = StepLogger.finish step_logger in
  ()


let write_functions_to_file
    ~static_analysis_configuration:
      { Configuration.StaticAnalysis.configuration = { local_root; _ }; _ }
    ~path
    definitions
  =
  let step_logger =
    StepLogger.start
      ~start_message:(Format.sprintf "Writing functions to `%s`" (PyrePath.absolute path))
      ~end_message:"Wrote functions"
  in
  let to_json_lines definition =
    [
      {
        Interprocedural.NewlineDelimitedJson.Line.kind = Function;
        data = `Assoc ["name", `String (Ast.Reference.show definition)];
      };
    ]
  in
  Interprocedural.NewlineDelimitedJson.write_file
    ~path
    ~configuration:(`Assoc ["repo", `String (PyrePath.absolute local_root)])
    ~to_json_lines
    definitions;
  let () = StepLogger.finish step_logger in
  ()


let create_pyre_read_write_api_and_perform_type_analysis
    ~scheduler
    ~static_analysis_configuration:
      ({ Configuration.StaticAnalysis.configuration; save_results_to; _ } as
      static_analysis_configuration)
    ~lookup_source
    ~decorator_configuration
    ~skip_type_checking_callables
  =
  let save_qualifiers_and_definitions absolute_source_path_of_qualifier qualifiers definitions =
    match save_results_to with
    | Some directory ->
        write_modules_to_file
          ~static_analysis_configuration
          ~lookup_source
          ~absolute_source_path_of_qualifier
          ~path:(PyrePath.append directory ~element:"modules.json")
          qualifiers;
        write_functions_to_file
          ~static_analysis_configuration
          ~path:(PyrePath.append directory ~element:"functions.json")
          definitions;
        ()
    | None -> ()
  in
  PyrePysaEnvironment.ReadWrite.create_with_cold_start
    ~scheduler
    ~configuration
    ~decorator_configuration
    ~skip_type_checking_callables
    ~callback_with_qualifiers_and_definitions:save_qualifiers_and_definitions


let parse_models_and_queries_from_sources
    ~scheduler
    ~pyre_api
    ~taint_configuration
    ~source_sink_filter
    ~definitions
    ~stubs
    ~python_version
    sources
  =
  (* TODO(T117715045): Do not pass all definitions explicitly to map_reduce,
   * since this will marshal-ed between processes and hence is costly. *)
  let map sources =
    let taint_configuration = TaintConfiguration.SharedMemory.get taint_configuration in
    List.fold sources ~init:ModelParseResult.empty ~f:(fun state (path, source) ->
        ModelParser.parse
          ~pyre_api
          ~path
          ~source
          ~taint_configuration
          ~source_sink_filter:(Some source_sink_filter)
          ~definitions
          ~stubs
          ~python_version
          ()
        |> ModelParseResult.join state)
  in
  Scheduler.map_reduce
    scheduler
    ~policy:
      (Scheduler.Policy.fixed_chunk_count
         ~minimum_chunks_per_worker:1
         ~minimum_chunk_size:1
         ~preferred_chunks_per_worker:1
         ())
    ~initial:ModelParseResult.empty
    ~map
    ~reduce:ModelParseResult.join
    ~inputs:sources
    ()


let parse_models_and_queries_from_configuration
    ~scheduler
    ~pyre_api
    ~static_analysis_configuration:{ Configuration.StaticAnalysis.verify_models; configuration; _ }
    ~taint_configuration
    ~source_sink_filter
    ~definitions
    ~stubs
  =
  let python_version = ModelParser.PythonVersion.from_configuration configuration in
  let ({ ModelParseResult.errors; _ } as parse_result) =
    ModelParser.get_model_sources ~paths:configuration.taint_model_paths
    |> parse_models_and_queries_from_sources
         ~scheduler
         ~pyre_api
         ~taint_configuration
         ~source_sink_filter
         ~definitions
         ~stubs
         ~python_version
  in
  let () = ModelVerificationError.verify_models_and_dsl ~raise_exception:verify_models errors in
  parse_result


let initialize_models
    ~scheduler
    ~pyre_api
    ~static_analysis_configuration:
      ({ Configuration.StaticAnalysis.scheduler_policies; _ } as static_analysis_configuration)
    ~taint_configuration
    ~taint_configuration_shared_memory
    ~class_hierarchy_graph
    ~initial_callables
  =
  let open TaintConfiguration.Heap in
  let step_logger =
    StepLogger.start ~start_message:"Parsing taint models" ~end_message:"Parsed taint models"
  in
  let definitions_hashset =
    initial_callables |> Interprocedural.FetchCallables.get_definitions |> Target.HashSet.of_list
  in
  let stubs_list = Interprocedural.FetchCallables.get_stubs initial_callables in
  let stubs_hashset = Target.HashSet.of_list stubs_list in
  let stubs_shared_memory = Interprocedural.Target.HashsetSharedMemory.from_heap stubs_list in
  let { ModelParseResult.models; queries; errors } =
    parse_models_and_queries_from_configuration
      ~scheduler
      ~pyre_api
      ~static_analysis_configuration
      ~taint_configuration:taint_configuration_shared_memory
      ~source_sink_filter:taint_configuration.source_sink_filter
      ~definitions:(Some definitions_hashset)
      ~stubs:(Interprocedural.Target.HashsetSharedMemory.read_only stubs_shared_memory)
  in
  let () =
    StepLogger.finish
      ~integers:["models", Registry.size models; "queries", List.length queries]
      step_logger
  in

  let models, errors =
    match queries with
    | [] -> models, errors
    | _ ->
        let step_logger =
          StepLogger.start
            ~start_message:"Generating models from model queries"
            ~end_message:"Generated models from model queries"
        in
        let verbose = Option.is_some taint_configuration.dump_model_query_results_path in
        let {
          ModelQueryExecution.ExecutionResult.models = model_query_results;
          errors = model_query_errors;
        }
          =
          ModelQueryExecution.generate_models_from_queries
            ~pyre_api
            ~scheduler
            ~scheduler_policies
            ~class_hierarchy_graph
            ~verbose
            ~error_on_unexpected_models:true
            ~error_on_empty_result:true
            ~source_sink_filter:(Some taint_configuration.source_sink_filter)
            ~definitions_and_stubs:
              (Interprocedural.FetchCallables.get initial_callables ~definitions:true ~stubs:true)
            ~stubs:(Interprocedural.Target.HashsetSharedMemory.read_only stubs_shared_memory)
            queries
        in
        let () =
          match taint_configuration.dump_model_query_results_path with
          | Some path ->
              ModelQueryExecution.DumpModelQueryResults.dump_to_file ~model_query_results ~path
          | None -> ()
        in
        let () =
          ModelVerificationError.verify_models_and_dsl
            ~raise_exception:static_analysis_configuration.verify_dsl
            model_query_errors
        in
        let errors = List.append errors model_query_errors in
        let models =
          model_query_results
          |> ModelQueryExecution.ModelQueryRegistryMap.get_registry
               ~model_join:Model.join_user_models
          |> Registry.merge ~join:Model.join_user_models models
        in
        let () = StepLogger.finish ~integers:["models", Registry.size models] step_logger in
        models, errors
  in

  let models =
    ClassModels.infer ~pyre_api ~user_models:models
    |> Registry.merge ~join:Model.join_user_models models
  in

  let models =
    MissingFlow.add_obscure_models
      ~static_analysis_configuration
      ~pyre_api
      ~stubs:stubs_hashset
      ~initial_models:models
  in

  let () = Interprocedural.Target.HashsetSharedMemory.cleanup stubs_shared_memory in

  { ModelParseResult.models; queries = []; errors }


let compact_ocaml_heap ~name =
  let step_logger =
    StepLogger.start
      ~start_message:(Format.sprintf "Compacting OCaml heap: %s" name)
      ~end_message:(Format.sprintf "Compacted OCaml heap: %s" name)
  in
  let original = Gc.get () in
  Gc.set { original with space_overhead = 25; max_overhead = 25 };
  Gc.compact ();
  Gc.set original;
  let () = StepLogger.finish step_logger in
  ()


let compute_coverage
    ~pyre_api
    ~scheduler
    ~scheduler_policies
    ~resolve_module_path
    ~callables_to_analyze
    ~all_callables
    ~rules
    ~fixpoint_state
  =
  let step_logger =
    StepLogger.start
      ~start_message:"Computing file coverage"
      ~end_message:"Finished computing file coverage"
  in
  let file_coverage =
    FileCoverage.from_callables
      ~scheduler
      ~scheduler_policies
      ~pyre_api
      ~resolve_module_path
      ~callables:callables_to_analyze
  in
  let () = StepLogger.finish step_logger in

  let step_logger =
    StepLogger.start
      ~start_message:"Computing kind coverage"
      ~end_message:"Finished computing kind coverage"
  in
  let compute_kind_coverage callables =
    callables
    |> List.filter_map ~f:(fun callable ->
           match TaintFixpoint.get_model fixpoint_state callable with
           | Some model -> Some (KindCoverage.from_model model)
           | None -> None)
    |> Algorithms.fold_balanced ~f:KindCoverage.union ~init:KindCoverage.empty
  in
  let scheduler_policy =
    Scheduler.Policy.from_configuration_or_default
      scheduler_policies
      Configuration.ScheduleIdentifier.TaintKindCoverage
      ~default:
        (Scheduler.Policy.fixed_chunk_size
           ~minimum_chunks_per_worker:1
           ~minimum_chunk_size:1
           ~preferred_chunk_size:100000
           ())
  in
  let kind_coverage_from_models =
    Scheduler.map_reduce
      scheduler
      ~policy:scheduler_policy
      ~initial:KindCoverage.empty
      ~map:compute_kind_coverage
      ~reduce:KindCoverage.union
      ~inputs:all_callables
      ()
  in
  let () = StepLogger.finish step_logger in

  let step_logger =
    StepLogger.start
      ~start_message:"Computing rule coverage"
      ~end_message:"Finished computing rule coverage"
  in
  let rule_coverage = RuleCoverage.from_rules ~kind_coverage_from_models rules in
  let () = StepLogger.finish step_logger in
  file_coverage, rule_coverage


let run_taint_analysis
    ~static_analysis_configuration:
      ({
         Configuration.StaticAnalysis.configuration;
         use_cache;
         build_cache_only;
         limit_entrypoints;
         compact_ocaml_heap = compact_ocaml_heap_flag;
         saved_state;
         compute_coverage = compute_coverage_flag;
         scheduler_policies;
         _;
       } as static_analysis_configuration)
    ~lookup_source
    ~scheduler
    ()
  =
  let taint_configuration = initialize_and_verify_configuration ~static_analysis_configuration in

  (* In order to save time, sanity check models before starting the analysis. *)
  let () = verify_model_syntax ~static_analysis_configuration in

  (* Parse taint models to find:
   * - decorators to inline or discard;
   * - functions to skip for type checking;
   *
   * This must be done early since decorator inlining is a preprocessing phase of
   * type-checking. *)
  let decorator_configuration, skip_type_checking_callables =
    parse_model_modes ~static_analysis_configuration
  in

  let cache =
    Cache.try_load
      ~scheduler
      ~scheduler_policies
      ~saved_state
      ~configuration
      ~decorator_configuration
      ~skip_type_checking_callables
      ~enabled:use_cache
  in

  (* We should NOT store anything in memory before calling `Cache.try_load` *)
  let pyre_read_write_api, cache =
    Cache.pyre_read_write_api cache (fun () ->
        create_pyre_read_write_api_and_perform_type_analysis
          ~scheduler
          ~scheduler_policies
          ~static_analysis_configuration
          ~lookup_source
          ~decorator_configuration
          ~skip_type_checking_callables)
  in
  let pyre_api = PyrePysaEnvironment.ReadOnly.of_read_write_api pyre_read_write_api in

  if compact_ocaml_heap_flag then
    compact_ocaml_heap ~name:"after type check";

  (* We must store the taint configuration into the shared memory after type checking, because type
     checking may reset the shared memory. *)
  let taint_configuration_shared_memory =
    TaintConfiguration.SharedMemory.from_heap taint_configuration
  in

  let qualifiers = PyrePysaEnvironment.ReadOnly.explicit_qualifiers pyre_api in

  let class_hierarchy_graph, cache =
    Cache.class_hierarchy_graph cache (fun () ->
        let step_logger =
          StepLogger.start
            ~start_message:"Computing class hierarchy graph"
            ~end_message:"Computed class hierarchy graph"
        in
        let class_hierarchy_graph =
          Interprocedural.ClassHierarchyGraph.Heap.from_qualifiers
            ~scheduler
            ~scheduler_policies
            ~pyre_api
            ~qualifiers
        in
        let () = StepLogger.finish step_logger in
        class_hierarchy_graph)
  in

  let class_interval_graph, cache =
    Cache.class_interval_graph cache (fun () ->
        let step_logger =
          StepLogger.start
            ~start_message:"Computing class intervals"
            ~end_message:"Computed class intervals"
        in
        let class_interval_graph =
          Interprocedural.ClassIntervalSetGraph.Heap.from_class_hierarchy class_hierarchy_graph
        in
        let () = StepLogger.finish step_logger in
        class_interval_graph)
  in

  let class_interval_graph_shared_memory =
    Interprocedural.ClassIntervalSetGraph.SharedMemory.from_heap class_interval_graph
  in

  let initial_callables, cache =
    Cache.initial_callables cache (fun () ->
        let step_logger =
          StepLogger.start
            ~start_message:"Fetching initial callables to analyze"
            ~end_message:"Fetched initial callables to analyze"
        in
        let initial_callables =
          Interprocedural.FetchCallables.from_qualifiers
            ~scheduler
            ~scheduler_policies
            ~configuration
            ~pyre_api
            ~qualifiers
        in
        let () =
          StepLogger.finish
            ~integers:(Interprocedural.FetchCallables.get_stats initial_callables)
            step_logger
        in
        initial_callables)
  in

  let { ModelParseResult.models = initial_models; errors = model_verification_errors; _ } =
    initialize_models
      ~scheduler
      ~pyre_api
      ~static_analysis_configuration
      ~taint_configuration
      ~taint_configuration_shared_memory
      ~class_hierarchy_graph
      ~initial_callables
  in

  let step_logger =
    StepLogger.start ~start_message:"Computing overrides" ~end_message:"Overrides computed"
  in
  let maximum_overrides = TaintConfiguration.maximum_overrides_to_analyze taint_configuration in
  let skip_overrides_targets = Registry.skip_overrides initial_models in
  let analyze_all_overrides_targets = Registry.analyze_all_overrides initial_models in
  let ( {
          Interprocedural.OverrideGraph.override_graph_heap;
          override_graph_shared_memory;
          skipped_overrides;
        },
        cache )
    =
    Cache.override_graph
      ~skip_overrides_targets
      ~maximum_overrides
      ~analyze_all_overrides_targets
      cache
      (fun ~skip_overrides_targets ~maximum_overrides ~analyze_all_overrides_targets () ->
        Interprocedural.OverrideGraph.build_whole_program_overrides
          ~static_analysis_configuration
          ~scheduler
          ~pyre_api
          ~skip_overrides_targets
          ~maximum_overrides
          ~analyze_all_overrides_targets
          ~qualifiers)
  in
  let override_graph_shared_memory_read_only =
    Interprocedural.OverrideGraph.SharedMemory.read_only override_graph_shared_memory
  in
  let () = StepLogger.finish step_logger in

  let step_logger =
    StepLogger.start
      ~start_message:"Indexing global constants"
      ~end_message:"Finished constant propagation analysis"
  in
  let global_constants, cache =
    Cache.global_constants cache (fun () ->
        Interprocedural.GlobalConstants.SharedMemory.from_qualifiers
          ~scheduler
          ~scheduler_policies
          ~pyre_api
          ~qualifiers)
  in
  let () = StepLogger.finish step_logger in

  let resolve_module_path =
    resolve_module_path
      ~lookup_source
      ~absolute_source_path_of_qualifier:
        (PyrePysaEnvironment.ReadOnly.absolute_source_path_of_qualifier pyre_api)
      ~static_analysis_configuration
  in

  let step_logger =
    StepLogger.start ~start_message:"Building call graph" ~end_message:"Call graph built"
  in
  let definitions = Interprocedural.FetchCallables.get_definitions initial_callables in
  let attribute_targets = Registry.object_targets initial_models in
  let skip_analysis_targets = Registry.skip_analysis initial_models in
  let decorator_resolution = Interprocedural.CallGraph.DecoratorResolution.Results.empty in
  let { Interprocedural.CallGraph.SharedMemory.whole_program_call_graph; define_call_graphs }, cache
    =
    Cache.call_graph
      ~attribute_targets
      ~skip_analysis_targets
      ~definitions
      cache
      (fun ~attribute_targets ~skip_analysis_targets ~definitions () ->
        Interprocedural.CallGraph.SharedMemory.build_whole_program_call_graph
          ~scheduler
          ~static_analysis_configuration
          ~pyre_api
          ~resolve_module_path:(Some resolve_module_path)
          ~override_graph:(Some override_graph_shared_memory_read_only)
          ~store_shared_memory:true
          ~attribute_targets
          ~skip_analysis_targets
          ~decorators:Interprocedural.CallGraph.CallableToDecoratorsMap.empty
          ~definitions
          ~decorator_resolution)
  in
  let () = StepLogger.finish step_logger in

  let prune_method =
    if limit_entrypoints then
      let entrypoint_references = Registry.entrypoints initial_models in
      let () =
        Log.info
          "Pruning call graph by the following entrypoints: %s"
          ([%show: Target.t list] entrypoint_references)
      in
      Interprocedural.DependencyGraph.PruneMethod.Entrypoints entrypoint_references
    else
      Interprocedural.DependencyGraph.PruneMethod.Internals
  in

  let step_logger =
    StepLogger.start ~start_message:"Computing dependencies" ~end_message:"Computed dependencies"
  in
  let {
    Interprocedural.DependencyGraph.dependency_graph;
    override_targets;
    callables_kept;
    callables_to_analyze;
  }
    =
    Interprocedural.DependencyGraph.build_whole_program_dependency_graph
      ~static_analysis_configuration
      ~prune:prune_method
      ~initial_callables
      ~call_graph:whole_program_call_graph
      ~overrides:override_graph_heap
  in
  let () = StepLogger.finish step_logger in

  let () =
    Cache.save
      ~maximum_overrides
      ~attribute_targets
      ~skip_type_checking_callables
      ~skip_analysis_targets
      ~skip_overrides_targets
      ~analyze_all_overrides_targets
      ~skipped_overrides
      ~override_graph_shared_memory
      ~initial_callables
      ~call_graph_shared_memory:define_call_graphs
      ~whole_program_call_graph
      ~global_constants
      cache
  in
  (if use_cache && build_cache_only then
     let () = Log.info "Cache has been built. Exiting now" in
     raise Cache.BuildCacheOnly);

  let () = PyrePysaEnvironment.ReadWrite.purge_shared_memory pyre_read_write_api in

  let initial_models =
    MissingFlow.add_unknown_callee_models
      ~static_analysis_configuration
      ~call_graph:whole_program_call_graph
      ~initial_models
  in

  let () = PyrePysaEnvironment.ReadWrite.purge_shared_memory pyre_read_write_api in

  if compact_ocaml_heap_flag then
    compact_ocaml_heap ~name:"before fixpoint";

  let fixpoint_step_logger =
    StepLogger.start
      ~start_message:
        (Format.sprintf
           "Analysis fixpoint started for %d overrides and %d functions..."
           (List.length override_targets)
           (List.length callables_kept))
      ~end_message:"Analysis fixpoint complete"
  in
  let initial_models =
    initial_models
    |> TaintFixpoint.Registry.to_alist
    |> TaintFixpoint.SharedModels.of_alist_parallel ~scheduler
  in
  let shared_models =
    TaintFixpoint.record_initial_models
      ~scheduler
      ~initial_models
      ~initial_callables:(Interprocedural.FetchCallables.get_definitions initial_callables)
      ~stubs:(Interprocedural.FetchCallables.get_stubs initial_callables)
      ~override_targets
  in
  let fixpoint_state =
    Taint.TaintFixpoint.compute
      ~scheduler
      ~scheduler_policy:(Taint.TaintFixpoint.get_scheduler_policy scheduler_policies)
      ~override_graph:override_graph_shared_memory_read_only
      ~dependency_graph
      ~context:
        {
          Taint.TaintFixpoint.Context.taint_configuration = taint_configuration_shared_memory;
          pyre_api;
          class_interval_graph = class_interval_graph_shared_memory;
          define_call_graphs = Interprocedural.CallGraph.SharedMemory.read_only define_call_graphs;
          global_constants = Interprocedural.GlobalConstants.SharedMemory.read_only global_constants;
          decorator_resolution;
        }
      ~callables_to_analyze
      ~max_iterations:100
      ~error_on_max_iterations:true
      ~epoch:Taint.TaintFixpoint.Epoch.initial
      ~shared_models
  in

  let all_callables =
    List.rev_append (TaintFixpoint.SharedModels.targets initial_models) callables_to_analyze
  in

  let file_coverage, rule_coverage =
    if not compute_coverage_flag then
      FileCoverage.empty, RuleCoverage.empty
    else
      compute_coverage
        ~pyre_api
        ~scheduler
        ~scheduler_policies
        ~resolve_module_path
        ~callables_to_analyze
        ~all_callables
        ~rules:taint_configuration.TaintConfiguration.Heap.rules
        ~fixpoint_state
  in

  let callables = Target.Set.of_list all_callables in

  let errors =
    TaintReporting.produce_errors
      ~scheduler
      ~static_analysis_configuration
      ~taint_configuration:taint_configuration_shared_memory
      ~resolve_module_path
      ~callables
      ~fixpoint_step_logger
      ~fixpoint_state
  in

  if compact_ocaml_heap_flag then
    compact_ocaml_heap ~name:"before saving results";

  let {
    Configuration.StaticAnalysis.save_results_to;
    output_format;
    configuration = { local_root; _ };
    _;
  }
    =
    static_analysis_configuration
  in
  (* Dump results to output directory if one was provided, and return a list of json (empty whenever
     we dumped to a directory) to summarize *)
  let summary =
    match save_results_to with
    | Some result_directory ->
        TaintReporting.save_results_to_directory
          ~scheduler
          ~taint_configuration:taint_configuration_shared_memory
          ~result_directory
          ~output_format
          ~local_root
          ~resolve_module_path
          ~resolve_callable_location:(Target.get_callable_location ~pyre_api)
          ~override_graph:override_graph_shared_memory_read_only
          ~callables
          ~skipped_overrides
          ~model_verification_errors
          ~fixpoint_state
          ~errors
          ~cache
          ~file_coverage
          ~rule_coverage;
        []
    | _ -> errors
  in
  Yojson.Safe.pretty_to_string (`List summary) |> Log.print "%s"
