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
module PyrePysaApi = Interprocedural.PyrePysaApi
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
        disable_model_shaping;
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
        maximum_capture_trace_length;
        _;
      }
  =
  let step_logger =
    StepLogger.start
      ~start_message:"Initializing and verifying taint configuration"
      ~end_message:"Initialized and verified taint configuration"
      ()
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
          ~disable_model_shaping
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
          ~maximum_capture_trace_length
    |> TaintConfiguration.exception_on_error
  in
  let () = StepLogger.finish step_logger in
  taint_configuration


let verify_model_syntax ~static_analysis_configuration =
  let step_logger =
    StepLogger.start ~start_message:"Verifying model syntax" ~end_message:"Verified model syntax" ()
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
      { Configuration.StaticAnalysis.configuration = { taint_model_paths; _ }; _ }
  =
  let step_logger =
    StepLogger.start
      ~start_message:"Parsing taint models modes"
      ~end_message:"Parsed taint models modes"
      ()
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
      ()
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
      ()
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
      ({ Configuration.StaticAnalysis.configuration; pyrefly_results; save_results_to; _ } as
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
  PyrePysaApi.ReadWrite.create_with_cold_start
    ~scheduler
    ~configuration
    ~pyrefly_results
    ~decorator_configuration
    ~skip_type_checking_callables
    ~callback_with_qualifiers_and_definitions:save_qualifiers_and_definitions


let parse_models_and_queries_from_sources
    ~scheduler
    ~pyre_api
    ~taint_configuration
    ~source_sink_filter
    ~callables_to_definitions_map
    ~python_version
    sources
  =
  let map sources =
    let taint_configuration = TaintConfiguration.SharedMemory.get taint_configuration in
    List.fold sources ~init:ModelParseResult.empty ~f:(fun state (path, source) ->
        ModelParser.parse
          ~pyre_api
          ~path
          ~source
          ~taint_configuration
          ~source_sink_filter:(Some source_sink_filter)
          ~callables_to_definitions_map
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
    ~callables_to_definitions_map
  =
  let python_version = ModelParser.PythonVersion.from_configuration configuration in
  let ({ ModelParseResult.errors; _ } as parse_result) =
    ModelParser.get_model_sources ~paths:configuration.taint_model_paths
    |> parse_models_and_queries_from_sources
         ~scheduler
         ~pyre_api
         ~taint_configuration
         ~source_sink_filter
         ~callables_to_definitions_map
         ~python_version
  in
  let errors = ModelVerifier.filter_unused_stdlib_modules_errors errors in
  let () = ModelVerificationError.verify_models_and_dsl ~raise_exception:verify_models errors in
  { parse_result with errors }


module ModelGenerationResult = struct
  type t = {
    models: SharedModels.t;
    errors: ModelVerificationError.t list;
  }
end

let initialize_models
    ~scheduler
    ~pyre_api
    ~static_analysis_configuration:
      ({ Configuration.StaticAnalysis.scheduler_policies; _ } as static_analysis_configuration)
    ~taint_configuration
    ~taint_configuration_shared_memory
    ~class_hierarchy_graph
    ~callables_to_definitions_map
    ~initial_callables
  =
  let open TaintConfiguration.Heap in
  let step_logger =
    StepLogger.start ~start_message:"Parsing taint models" ~end_message:"Parsed taint models" ()
  in
  let { ModelParseResult.models = regular_models; queries; errors } =
    parse_models_and_queries_from_configuration
      ~scheduler
      ~pyre_api
      ~static_analysis_configuration
      ~taint_configuration:taint_configuration_shared_memory
      ~source_sink_filter:taint_configuration.source_sink_filter
      ~callables_to_definitions_map:(Some callables_to_definitions_map)
  in
  let () =
    StepLogger.finish
      ~integers:["models", Registry.size regular_models; "queries", List.length queries]
      step_logger
  in

  let models_from_queries, errors =
    match queries with
    | [] -> SharedModels.create (), errors
    | _ ->
        let step_logger =
          StepLogger.start
            ~start_message:"Generating models from model queries"
            ~end_message:"Generated models from model queries"
            ()
        in
        let verbose = Option.is_some taint_configuration.dump_model_query_results_path in
        let model_query_results =
          ModelQueryExecution.generate_models_from_queries
            ~pyre_api
            ~scheduler
            ~scheduler_policies
            ~class_hierarchy_graph
            ~callables_to_definitions_map
            ~verbose
            ~error_on_unexpected_models:true
            ~error_on_empty_result:true
            ~source_sink_filter:(Some taint_configuration.source_sink_filter)
            ~definitions_and_stubs:
              (Interprocedural.FetchCallables.get initial_callables ~definitions:true ~stubs:true)
            queries
        in
        let () =
          match taint_configuration.dump_model_query_results_path with
          | Some path -> ModelQueryExecution.ExecutionResult.dump_to_file model_query_results ~path
          | None -> ()
        in
        let model_query_errors =
          ModelQueryExecution.ExecutionResult.get_errors model_query_results
        in
        let () =
          ModelVerificationError.verify_models_and_dsl
            ~raise_exception:static_analysis_configuration.verify_dsl
            model_query_errors
        in
        let errors = List.append errors model_query_errors in
        let models = ModelQueryExecution.ExecutionResult.get_models model_query_results in
        let () = StepLogger.finish ~integers:["models", SharedModels.size models] step_logger in
        models, errors
  in

  let models =
    SharedModels.join_with_registry_sequential
      models_from_queries
      ~model_join:Model.join_user_models
      regular_models
  in

  let models =
    ClassModels.infer
      ~scheduler
      ~scheduler_policies
      ~pyre_api
      ~user_models:(SharedModels.read_only models)
    |> SharedModels.join_with_registry_sequential models ~model_join:Model.join_user_models
  in

  let models =
    MissingFlow.add_obscure_models
      ~scheduler
      ~static_analysis_configuration
      ~callables_to_definitions_map
      ~stubs:(Interprocedural.FetchCallables.get_stubs initial_callables)
      ~initial_models:models
  in

  { ModelGenerationResult.models; errors }


let compact_ocaml_heap ~name =
  let step_logger =
    StepLogger.start
      ~start_message:(Format.sprintf "Compacting OCaml heap: %s" name)
      ~end_message:(Format.sprintf "Compacted OCaml heap: %s" name)
      ()
  in
  let original = Gc.get () in
  Gc.set { original with space_overhead = 25; max_overhead = 25 };
  Gc.compact ();
  Gc.set original;
  let () = StepLogger.finish step_logger in
  ()


let compute_coverage
    ~scheduler
    ~scheduler_policies
    ~callables_to_definitions_map
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
      ()
  in
  let file_coverage =
    FileCoverage.from_callables
      ~scheduler
      ~scheduler_policies
      ~callables_to_definitions_map
      ~resolve_module_path
      ~callables:callables_to_analyze
  in
  let () = StepLogger.finish step_logger in

  let step_logger =
    StepLogger.start
      ~start_message:"Computing kind coverage"
      ~end_message:"Finished computing kind coverage"
      ()
  in
  let compute_kind_coverage callables =
    callables
    |> List.filter_map ~f:(fun callable ->
           match TaintFixpoint.State.ReadOnly.get_model fixpoint_state callable with
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
      ()
  in
  let rule_coverage = RuleCoverage.from_rules ~kind_coverage_from_models rules in
  let () = StepLogger.finish step_logger in
  file_coverage, rule_coverage


let cleanup_shared_memory_after_call_graph_fixpoint
    ~callables_to_decorators_map
    ~original_define_call_graphs
    ~call_graph_fixpoint
  =
  Interprocedural.CallableToDecoratorsMap.SharedMemory.cleanup callables_to_decorators_map;
  Interprocedural.CallGraph.SharedMemory.cleanup original_define_call_graphs;
  Interprocedural.CallGraphFixpoint.cleanup ~keep_models:true call_graph_fixpoint


let run_taint_analysis
    ~static_analysis_configuration:
      ({
         Configuration.StaticAnalysis.configuration;
         pyrefly_results;
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
      ~pyrefly_results
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
  let pyre_api = PyrePysaApi.ReadOnly.of_read_write_api pyre_read_write_api in

  if compact_ocaml_heap_flag then
    compact_ocaml_heap ~name:"after type check";

  (* We must store the taint configuration into the shared memory after type checking, because type
     checking may reset the shared memory. *)
  let taint_configuration_shared_memory =
    TaintConfiguration.SharedMemory.from_heap taint_configuration
  in

  let qualifiers = PyrePysaApi.ReadOnly.explicit_qualifiers pyre_api in

  let class_hierarchy_graph, cache =
    Cache.class_hierarchy_graph cache (fun () ->
        let step_logger =
          StepLogger.start
            ~start_message:"Computing class hierarchy graph"
            ~end_message:"Computed class hierarchy graph"
            ()
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
            ()
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
            ()
        in
        let initial_callables =
          Interprocedural.FetchCallables.from_qualifiers
            ~scheduler
            ~scheduler_policy:
              (Scheduler.Policy.from_configuration_or_default
                 scheduler_policies
                 Configuration.ScheduleIdentifier.TaintFetchCallables
                 ~default:
                   (Scheduler.Policy.fixed_chunk_count
                      ~minimum_chunks_per_worker:1
                      ~minimum_chunk_size:1
                      ~preferred_chunks_per_worker:1
                      ()))
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

  let definitions_and_stubs =
    Interprocedural.FetchCallables.get initial_callables ~definitions:true ~stubs:true
  in
  let step_logger =
    StepLogger.start
      ~start_message:"Building a map from callable names to definitions"
      ~end_message:"Map from callable names to definitions built"
      ()
  in
  let callables_to_definitions_map =
    Interprocedural.CallablesSharedMemory.ReadWrite.from_callables
      ~scheduler
      ~scheduler_policy:
        (Scheduler.Policy.from_configuration_or_default
           scheduler_policies
           Configuration.ScheduleIdentifier.CallablesSharedMemory
           ~default:
             (Scheduler.Policy.fixed_chunk_count
                ~minimum_chunks_per_worker:1
                ~minimum_chunk_size:1
                ~preferred_chunks_per_worker:1
                ()))
      ~pyre_api
      definitions_and_stubs
  in
  let () = StepLogger.finish step_logger in

  let definitions = Interprocedural.FetchCallables.get_definitions initial_callables in

  let { ModelGenerationResult.models = initial_models; errors = model_verification_errors; _ } =
    initialize_models
      ~scheduler
      ~pyre_api
      ~static_analysis_configuration
      ~taint_configuration
      ~taint_configuration_shared_memory
      ~class_hierarchy_graph
      ~callables_to_definitions_map:
        (Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
      ~initial_callables
  in

  let step_logger =
    StepLogger.start ~start_message:"Computing overrides" ~end_message:"Overrides computed" ()
  in
  let maximum_overrides = TaintConfiguration.maximum_overrides_to_analyze taint_configuration in
  let skip_overrides_targets = SharedModels.skip_overrides ~scheduler initial_models in
  let analyze_all_overrides_targets =
    SharedModels.analyze_all_overrides ~scheduler initial_models
  in
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
      ()
  in
  let global_constants, cache =
    Cache.global_constants cache (fun () ->
        Interprocedural.GlobalConstants.SharedMemory.from_qualifiers
          ~scheduler
          ~scheduler_policies
          ~pyre_api
          ~callables_to_definitions_map:
            (Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
          ~qualifiers)
  in
  let () = StepLogger.finish step_logger in

  let resolve_module_path =
    resolve_module_path
      ~lookup_source
      ~absolute_source_path_of_qualifier:
        (PyrePysaApi.ReadOnly.absolute_source_path_of_qualifier pyre_api)
      ~static_analysis_configuration
  in

  let pyre_read_write_api =
    PyrePysaApi.ReadWrite.parse_type_of_expressions
      pyre_read_write_api
      ~scheduler
      ~scheduler_policies
  in
  let pyre_api = PyrePysaApi.ReadOnly.of_read_write_api pyre_read_write_api in

  let type_of_expression_shared_memory =
    Interprocedural.TypeOfExpressionSharedMemory.create
      ~pyre_api
      ~callables_to_definitions_map:
        (Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
      ()
  in

  let attribute_targets = SharedModels.object_targets initial_models in
  let skip_analysis_targets = SharedModels.skip_analysis ~scheduler initial_models in
  let skip_analysis_targets_hashset =
    skip_analysis_targets |> Target.Set.elements |> Target.HashSet.of_list
  in
  let callables_to_decorators_map =
    let step_logger =
      StepLogger.start
        ~start_message:"Building map from callables to decorators"
        ~end_message:"Map from callables to decorators built"
        ()
    in
    let callables_to_decorators_map =
      Interprocedural.CallableToDecoratorsMap.SharedMemory.create
        ~scheduler
        ~scheduler_policy:
          (Scheduler.Policy.from_configuration_or_default
             scheduler_policies
             Configuration.ScheduleIdentifier.CallableToDecoratorsMap
             ~default:Interprocedural.CallGraphBuilder.default_scheduler_policy)
        ~pyre_api
        ~callables_to_definitions_map:
          (Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
        ~skip_analysis_targets:skip_analysis_targets_hashset
        definitions
    in
    let () =
      Interprocedural.CallableToDecoratorsMap.SharedMemory.save_decorator_counts_to_directory
        ~static_analysis_configuration
        ~scheduler
        callables_to_decorators_map
    in
    let () = StepLogger.finish step_logger in
    callables_to_decorators_map
  in

  let step_logger =
    StepLogger.start ~start_message:"Building call graph" ~end_message:"Call graph built" ()
  in
  let ( ({
           Interprocedural.CallGraph.SharedMemory.whole_program_call_graph =
             original_whole_program_call_graph;
           define_call_graphs = original_define_call_graphs;
         } as original_call_graphs),
        cache )
    =
    Cache.call_graph
      ~attribute_targets
      ~skip_analysis_targets:skip_analysis_targets_hashset
      ~definitions
      cache
      (fun ~attribute_targets ~skip_analysis_targets ~definitions () ->
        Interprocedural.CallGraphBuilder.build_whole_program_call_graph
          ~scheduler
          ~static_analysis_configuration
          ~pyre_api
          ~resolve_module_path:(Some resolve_module_path)
          ~override_graph:(Some override_graph_shared_memory_read_only)
          ~store_shared_memory:true
          ~attribute_targets
          ~skip_analysis_targets
          ~check_invariants:(TaintConfiguration.runtime_check_invariants ())
          ~definitions
          ~callables_to_definitions_map:
            (Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
          ~callables_to_decorators_map:
            (Interprocedural.CallableToDecoratorsMap.SharedMemory.read_only
               callables_to_decorators_map)
          ~global_constants:
            (Interprocedural.GlobalConstants.SharedMemory.read_only global_constants)
          ~type_of_expression_shared_memory
          ~create_dependency_for:Interprocedural.CallGraph.AllTargetsUseCase.CallGraphDependency)
  in
  let () =
    StepLogger.finish
      ~integers:
        [
          ( "edges",
            Interprocedural.CallGraph.WholeProgramCallGraph.number_edges
              original_whole_program_call_graph );
        ]
      step_logger
  in

  let prune_method =
    if limit_entrypoints then
      let entrypoint_references = SharedModels.entrypoints ~scheduler initial_models in
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
    StepLogger.start ~start_message:"Computing dependencies" ~end_message:"Computed dependencies" ()
  in
  let original_dependency_graph =
    Interprocedural.DependencyGraph.build_whole_program_dependency_graph
      ~static_analysis_configuration
      ~prune:prune_method
      ~initial_callables
      ~call_graph:original_whole_program_call_graph
      ~overrides:override_graph_heap
      ~ignore_decorated_targets:false
  in
  let () = StepLogger.finish step_logger in

  let ( {
          Interprocedural.DependencyGraph.dependency_graph;
          override_targets;
          callables_kept;
          callables_to_analyze;
        },
        get_define_call_graph,
        higher_order_call_graph_fixpoint )
    =
    let step_logger =
      StepLogger.start
        ~start_message:"Computing higher order call graphs"
        ~end_message:"Computed higher order call graphs"
        ()
    in
    let {
      Interprocedural.CallGraphFixpoint.whole_program_call_graph;
      get_define_call_graph;
      fixpoint;
      _;
    }
      =
      Interprocedural.CallGraphFixpoint.compute
        ~scheduler
        ~scheduler_policy:
          (Scheduler.Policy.from_configuration_or_default
             scheduler_policies
             Configuration.ScheduleIdentifier.HigherOrderCallGraph
             ~default:
               (Scheduler.Policy.fixed_chunk_size
                  ~minimum_chunks_per_worker:1
                  ~minimum_chunk_size:100
                  ~preferred_chunk_size:30000
                  ()))
        ~static_analysis_configuration
        ~resolve_module_path:(Some resolve_module_path)
        ~pyre_api
        ~call_graph:original_call_graphs
        ~dependency_graph:original_dependency_graph
        ~override_graph_shared_memory
        ~callables_to_definitions_map
        ~callables_to_decorators_map
        ~type_of_expression_shared_memory
        ~skip_analysis_targets:skip_analysis_targets_hashset
        ~called_when_parameter:(SharedModels.called_when_parameter ~scheduler initial_models)
    in
    let () = StepLogger.finish step_logger in

    let step_logger =
      StepLogger.start
        ~start_message:"Computing dependencies from higher order call graphs"
        ~end_message:"Computed dependencies from higher order call graphs"
        ()
    in
    let dependency_graph =
      Interprocedural.DependencyGraph.build_whole_program_dependency_graph
        ~static_analysis_configuration
        ~prune:prune_method
        ~initial_callables
        ~call_graph:whole_program_call_graph
        ~overrides:override_graph_heap
        ~ignore_decorated_targets:true
    in
    let () = StepLogger.finish step_logger in
    dependency_graph, get_define_call_graph, fixpoint
  in

  (* TODO(T215367584): Cache higher order call graphs. *)
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
      ~initial_models
      ~call_graph_shared_memory:original_define_call_graphs
      ~whole_program_call_graph:original_whole_program_call_graph
      ~global_constants
      cache
  in
  (if use_cache && build_cache_only then
     let () = Log.info "Cache has been built. Exiting now" in
     raise Cache.BuildCacheOnly);
  let () =
    cleanup_shared_memory_after_call_graph_fixpoint
      ~callables_to_decorators_map
      ~original_define_call_graphs
      ~call_graph_fixpoint:higher_order_call_graph_fixpoint
  in

  let () = PyrePysaApi.ReadWrite.purge_sources_from_shared_memory pyre_read_write_api in

  let initial_models =
    MissingFlow.add_unknown_callee_models
      ~static_analysis_configuration
      ~call_graph:original_whole_program_call_graph
      ~initial_models
  in

  let step_logger =
    StepLogger.start
      ~start_message:"Initializing taint models for parameterized callables"
      ~end_message:"Initialized taint models for parameterized callables"
      ()
  in
  let initial_models =
    SharedModels.initialize_for_parameterized_callables
      ~higher_order_call_graph_fixpoint
      initial_models
  in
  let () = StepLogger.finish step_logger in

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
      ()
  in
  (* This should always be called immediately before `TaintFixpoint.compute`, because it initializes
     `TaintFixpoint.State`. *)
  let fixpoint_state =
    TaintFixpoint.record_initial_models
      ~scheduler
      ~initial_models
      ~callables_to_analyze
      ~stubs:(Interprocedural.FetchCallables.get_stubs initial_callables)
      ~override_targets
  in
  let fixpoint =
    Taint.TaintFixpoint.compute
      ~scheduler
      ~scheduler_policy:(Taint.TaintFixpoint.get_scheduler_policy scheduler_policies)
      ~override_graph:override_graph_shared_memory_read_only
      ~dependency_graph
      ~skip_analysis_targets:skip_analysis_targets_hashset
      ~context:
        {
          Taint.TaintFixpoint.Context.taint_configuration = taint_configuration_shared_memory;
          pyre_api;
          class_interval_graph = class_interval_graph_shared_memory;
          get_define_call_graph;
          global_constants = Interprocedural.GlobalConstants.SharedMemory.read_only global_constants;
          type_of_expression_shared_memory;
          callables_to_definitions_map =
            Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map;
        }
      ~callables_to_analyze
      ~max_iterations:150
      ~error_on_max_iterations:true
      ~epoch:Taint.TaintFixpoint.Epoch.initial
      ~state:fixpoint_state
  in

  let all_callables = List.rev_append (SharedModels.targets initial_models) callables_to_analyze in

  let file_coverage, rule_coverage =
    if not compute_coverage_flag then
      FileCoverage.empty, RuleCoverage.empty
    else
      compute_coverage
        ~scheduler
        ~scheduler_policies
        ~callables_to_definitions_map:
          (Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
        ~resolve_module_path
        ~callables_to_analyze
        ~all_callables
        ~rules:taint_configuration.TaintConfiguration.Heap.rules
        ~fixpoint_state:(TaintFixpoint.State.read_only fixpoint.TaintFixpoint.state)
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
      ~fixpoint
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
        let callables_to_definitions_map =
          Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map
        in
        TaintReporting.save_results_to_directory
          ~scheduler
          ~taint_configuration:taint_configuration_shared_memory
          ~result_directory
          ~output_format
          ~local_root
          ~resolve_module_path
          ~resolve_callable_location:
            (Interprocedural.CallablesSharedMemory.ReadOnly.get_location_opt
               callables_to_definitions_map)
          ~override_graph:override_graph_shared_memory_read_only
          ~callables
          ~skipped_overrides
          ~model_verification_errors
          ~fixpoint_state:(TaintFixpoint.State.read_only fixpoint.TaintFixpoint.state)
          ~errors
          ~cache
          ~file_coverage
          ~rule_coverage;
        []
    | _ -> errors
  in
  Yojson.Safe.pretty_to_string (`List summary) |> Log.print "%s"
