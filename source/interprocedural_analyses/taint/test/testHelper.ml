(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TestHelper: utility functions for unit and integration tests. *)

module CamlUnix = Unix
open Core
open OUnit2
open Ast
open Pyre
open Taint
open Interprocedural
module AccessPath = Analysis.TaintAccessPath
module PyrePysaApi = Interprocedural.PyrePysaApi
module PyrePysaLogic = Analysis.PyrePysaLogic

type parameter_sinks = {
  name: string;
  sinks: Sinks.t list;
}

type parameter_sources = {
  name: string;
  sources: Sources.t list;
}

type parameter_titos = {
  name: string;
  titos: Sinks.t list;
}

type parameter_sanitize = {
  name: string;
  sanitize: Sanitize.t;
}

type error_expectation = {
  code: int;
  pattern: string;
}

type expectation = {
  kind: [ `Function | `Method | `Override | `Object | `PropertySetter ];
  define_name: string;
  parameter_generations: parameter_sources list;
  parameter_sources: parameter_sources list;
  parameter_sinks: parameter_sinks list;
  parameter_titos: parameter_titos list;
  returns: Sources.t list;
  return_sinks: Sinks.t list;
  errors: error_expectation list;
  global_sanitizer: Sanitize.t;
  parameters_sanitizer: Sanitize.t;
  return_sanitizer: Sanitize.t;
  parameter_sanitizers: parameter_sanitize list;
  add_breadcrumbs_to_state: string list;
  analysis_modes: Model.ModeSet.t;
}

let print_parameter_taint ~print_taint map =
  map
  |> Map.to_alist
  |> List.map ~f:(fun (key, value) ->
         Format.asprintf "%s -> %s" key (value |> List.map ~f:print_taint |> String.concat ~sep:", "))
  |> String.concat ~sep:"; "


let outcome
    ~kind
    ?(parameter_generations = [])
    ?(parameter_sources = [])
    ?(parameter_sinks = [])
    ?(parameter_titos = [])
    ?(returns = [])
    ?(return_sinks = [])
    ?(errors = [])
    ?(global_sanitizer = Sanitize.empty)
    ?(parameters_sanitizer = Sanitize.empty)
    ?(return_sanitizer = Sanitize.empty)
    ?(parameter_sanitizers = [])
    ?(add_breadcrumbs_to_state = [])
    ?(analysis_modes = Model.ModeSet.empty)
    define_name
  =
  {
    kind;
    define_name;
    parameter_generations;
    parameter_sources;
    parameter_sinks;
    parameter_titos;
    returns;
    return_sinks;
    errors;
    global_sanitizer;
    parameters_sanitizer;
    return_sanitizer;
    parameter_sanitizers;
    add_breadcrumbs_to_state;
    analysis_modes;
  }


let create_callable ~pyre_api kind define_name =
  let name = Reference.create define_name in
  match kind with
  | `Method -> Target.create_method_from_reference name
  | `Function -> Target.create_function name
  | `PropertySetter ->
      if PyrePysaApi.ReadOnly.is_pyrefly pyre_api then
        Target.create_method_from_reference
          ~kind:Target.PyreflyPropertySetter
          (Reference.create (Format.sprintf "%s@setter" define_name))
      else
        Target.create_method_from_reference ~kind:Target.Pyre1PropertySetter name
  | `Override -> Target.create_override_from_reference name
  | `Object -> Target.create_object name


let check_expectation
    ~get_model
    ~get_errors
    ~pyre_api
    ~taint_configuration
    {
      kind;
      define_name;
      parameter_generations;
      parameter_sources = expected_parameter_sources;
      parameter_sinks;
      parameter_titos;
      returns;
      return_sinks;
      errors;
      global_sanitizer;
      parameters_sanitizer;
      return_sanitizer;
      parameter_sanitizers;
      add_breadcrumbs_to_state = expected_add_breadcrumbs_to_state;
      analysis_modes = expected_analysis_modes;
    }
  =
  let callable = create_callable ~pyre_api kind define_name in
  let extract_sinks_by_parameter_name (root, sink_tree) sink_map =
    match AccessPath.Root.parameter_name root with
    | Some name ->
        let sinks =
          Domains.BackwardState.Tree.collapse
            ~breadcrumbs:Features.BreadcrumbMayAlwaysSet.empty
            sink_tree
          |> Domains.BackwardTaint.kinds
        in
        let sinks =
          Core.Map.find sink_map name
          |> Option.value ~default:[]
          |> List.rev_append sinks
          |> List.dedup_and_sort ~compare:Sinks.compare
        in
        Core.Map.set sink_map ~key:name ~data:sinks
    | _ -> sink_map
  in
  let extract_sources_by_parameter_name (root, source_tree) sink_map =
    match AccessPath.Root.parameter_name root with
    | Some name ->
        let sinks =
          Domains.ForwardState.Tree.collapse
            ~breadcrumbs:Features.BreadcrumbMayAlwaysSet.empty
            source_tree
          |> Domains.ForwardTaint.kinds
        in
        let sinks =
          Core.Map.find sink_map name
          |> Option.value ~default:[]
          |> List.rev_append sinks
          |> List.dedup_and_sort ~compare:Sources.compare
        in
        Core.Map.set sink_map ~key:name ~data:sinks
    | _ -> sink_map
  in
  let {
    Model.backward;
    forward;
    parameter_sources = actual_parameter_sources;
    sanitizers;
    add_breadcrumbs_to_state;
    model_generators = _;
    modes;
  }
    =
    Option.value_exn
      ~message:(Format.asprintf "Model not found for %a" Target.pp callable)
      (get_model callable)
  in
  assert_equal ~cmp:Model.ModeSet.equal ~printer:Model.ModeSet.show modes expected_analysis_modes;
  let sink_taint_map =
    Domains.BackwardState.fold
      Domains.BackwardState.KeyValue
      backward.sink_taint
      ~f:extract_sinks_by_parameter_name
      ~init:String.Map.empty
  in
  let parameter_generation_taint_map =
    Domains.ForwardState.fold
      Domains.ForwardState.KeyValue
      forward.generations
      ~f:extract_sources_by_parameter_name
      ~init:String.Map.empty
  in
  let parameter_source_taint_map =
    Domains.ForwardState.fold
      Domains.ForwardState.KeyValue
      actual_parameter_sources.parameter_sources
      ~f:extract_sources_by_parameter_name
      ~init:String.Map.empty
  in
  let extract_parameter_sanitize map (root, sanitize) =
    match AccessPath.Root.parameter_name root with
    | Some name -> Core.Map.set map ~key:name ~data:sanitize
    | _ -> map
  in
  let parameter_sanitize_map =
    Sanitize.RootMap.to_alist sanitizers.roots
    |> List.fold ~init:String.Map.empty ~f:extract_parameter_sanitize
  in
  let parameter_taint_in_taint_out_map =
    Domains.BackwardState.fold
      Domains.BackwardState.KeyValue
      backward.taint_in_taint_out
      ~f:extract_sinks_by_parameter_name
      ~init:String.Map.empty
  in
  let check_each_sanitize ~key:name ~data =
    match data with
    | `Both (expected, actual) ->
        assert_equal
          ~cmp:Sanitize.equal
          ~printer:Sanitize.show
          ~msg:(Format.sprintf "Define %s Parameter %s" define_name name)
          expected
          actual
    | `Left expected ->
        assert_equal
          ~cmp:Sanitize.equal
          ~printer:Sanitize.show
          ~msg:(Format.sprintf "Define %s Parameter %s" define_name name)
          expected
          Sanitize.empty
    | `Right actual ->
        assert_equal
          ~cmp:Sanitize.equal
          ~printer:Sanitize.show
          ~msg:(Format.sprintf "Define %s Parameter %s" define_name name)
          Sanitize.empty
          actual
  in
  let expected_sinks =
    List.map ~f:(fun { name; sinks } -> name, sinks) parameter_sinks |> String.Map.of_alist_exn
  in
  let expected_parameter_generations =
    List.map ~f:(fun { name; sources } -> name, sources) parameter_generations
    |> String.Map.of_alist_exn
  in
  let expected_parameter_sources =
    List.map ~f:(fun { name; sources } -> name, sources) expected_parameter_sources
    |> String.Map.of_alist_exn
  in
  let expected_parameter_sanitizers =
    List.map ~f:(fun { name; sanitize } -> name, sanitize) parameter_sanitizers
    |> String.Map.of_alist_exn
  in
  (* Check sources. *)
  let returned_sources =
    Domains.ForwardState.read ~root:AccessPath.Root.LocalResult ~path:[] forward.generations
    |> Domains.ForwardState.Tree.collapse ~breadcrumbs:Features.BreadcrumbMayAlwaysSet.empty
    |> Domains.ForwardTaint.kinds
    |> List.map ~f:Sources.show
    |> String.Set.of_list
  in
  let expected_sources = List.map ~f:Sources.show returns |> String.Set.of_list in
  let assert_error { code; pattern } error =
    if code <> Error.Instantiated.code error then
      Format.sprintf
        "Expected error code %d for %s, but got %d"
        code
        define_name
        (Error.Instantiated.code error)
      |> assert_failure;
    let error_string = Error.Instantiated.description error in
    let regexp = Str.regexp pattern in
    if not (Str.string_match regexp error_string 0) then
      Format.sprintf
        "Expected error for %s to match %s, but got %s"
        define_name
        pattern
        error_string
      |> assert_failure
  in
  let assert_errors error_patterns errors =
    assert_equal
      (List.length error_patterns)
      (List.length errors)
      ~msg:(Format.sprintf "Number of errors for %s" define_name)
      ~printer:Int.to_string;
    List.iter2_exn ~f:assert_error error_patterns errors
  in
  assert_equal
    ~cmp:String.Set.equal
    ~printer:(fun set ->
      Format.sprintf
        "Returned sources %s: %s"
        define_name
        (Sexp.to_string [%message (set : String.Set.t)]))
    expected_sources
    returned_sources;

  (* Check sinks. *)
  assert_equal
    expected_sinks
    sink_taint_map
    ~cmp:(Map.equal (List.equal Sinks.equal))
    ~printer:(print_parameter_taint ~print_taint:Sinks.show);

  (* Check parameter generations. *)
  assert_equal
    expected_parameter_generations
    parameter_generation_taint_map
    ~cmp:(Map.equal (List.equal Sources.equal))
    ~printer:(print_parameter_taint ~print_taint:Sources.show);

  (* Check parameter sources. *)
  assert_equal
    expected_parameter_sources
    parameter_source_taint_map
    ~cmp:(Map.equal (List.equal Sources.equal))
    ~printer:(print_parameter_taint ~print_taint:Sources.show);

  let expected_tito =
    List.map ~f:(fun { name; titos } -> name, titos) parameter_titos |> String.Map.of_alist_exn
  in
  assert_equal
    expected_tito
    parameter_taint_in_taint_out_map
    ~cmp:(Map.equal (List.equal Sinks.equal))
    ~printer:(print_parameter_taint ~print_taint:Sinks.show);

  (* Check return sinks. *)
  let expected_return_sinks = List.map ~f:Sinks.show return_sinks |> String.Set.of_list in
  let actual_return_sinks =
    Domains.BackwardState.read ~root:AccessPath.Root.LocalResult ~path:[] backward.sink_taint
    |> Domains.BackwardState.Tree.collapse ~breadcrumbs:Features.BreadcrumbMayAlwaysSet.empty
    |> Domains.BackwardTaint.kinds
    |> List.map ~f:Sinks.show
    |> String.Set.of_list
  in
  assert_equal
    ~cmp:String.Set.equal
    ~printer:(fun set ->
      Format.sprintf
        "Return sinks %s: %s"
        define_name
        (Sexp.to_string [%message (set : String.Set.t)]))
    expected_return_sinks
    actual_return_sinks;

  (* Check sanitizers *)
  assert_equal ~cmp:Sanitize.equal ~printer:Sanitize.show global_sanitizer sanitizers.global;
  assert_equal ~cmp:Sanitize.equal ~printer:Sanitize.show parameters_sanitizer sanitizers.parameters;
  assert_equal
    ~cmp:Sanitize.equal
    ~printer:Sanitize.show
    return_sanitizer
    (Sanitize.RootMap.get AccessPath.Root.LocalResult sanitizers.roots);

  (* Check add_breadcrumbs_to_state *)
  assert_equal
    ~cmp:Model.AddBreadcrumbsToState.equal
    ~printer:Model.AddBreadcrumbsToState.show
    (expected_add_breadcrumbs_to_state
    |> List.map ~f:(fun name -> Features.Breadcrumb.SimpleVia name)
    |> List.map ~f:Features.BreadcrumbInterned.intern
    |> Model.AddBreadcrumbsToState.of_list)
    add_breadcrumbs_to_state;

  assert_equal
    (Map.length expected_parameter_sanitizers)
    (Map.length parameter_sanitize_map)
    ~printer:Int.to_string
    ~msg:(Format.sprintf "Define %s: List of parameter sanitizers differ in length." define_name);
  Core.Map.iter2 ~f:check_each_sanitize expected_parameter_sanitizers parameter_sanitize_map;

  (* Check errors *)
  let actual_errors =
    let to_analysis_error =
      Error.instantiate
        ~show_error_traces:true
        ~lookup:(PyrePysaApi.ReadOnly.relative_path_of_qualifier pyre_api)
    in
    get_errors callable
    |> List.map ~f:(Issue.to_error ~taint_configuration)
    |> List.map ~f:to_analysis_error
  in
  assert_errors errors actual_errors


let initial_models_source =
  {|
      def pysa._test_sink(arg: TaintSink[Test, Via[special_sink]]): ...
      def pysa._test_source() -> TaintSource[Test, Via[special_source]]: ...
      def pysa._tito( *x: TaintInTaintOut, **kw: TaintInTaintOut): ...
      def eval(arg: TaintSink[RemoteCodeExecution]): ...
      def pysa._user_controlled() -> TaintSource[UserControlled]: ...
      def pysa._cookies() -> TaintSource[Cookies]: ...
      def pysa._rce(argument: TaintSink[RemoteCodeExecution]): ...
      def pysa._sql(argument: TaintSink[SQL]): ...
      @SkipObscure
      def getattr(
          o: TaintInTaintOut[Via[object]],
          name: TaintSink[GetAttr],
          default: TaintInTaintOut[Via[default]] = ...,
      ): ...

      pysa._global_sink: TaintSink[Test] = ...
      pysa.ClassWithSinkAttribute.attribute: TaintSink[Test] = ...

      def pysa.copy(obj: TaintInTaintOut[Via[copy]]): ...

      @SkipOverrides
      def dict.__setitem__(self): ...
    |}
  |> Test.trim_extra_indentation


let get_initial_models ~context =
  let pyre_api =
    Test.ScratchProject.setup ~context []
    |> Test.ScratchProject.pyre_pysa_read_only_api
    |> PyrePysaApi.ReadOnly.from_pyre1_api
  in
  let { ModelParseResult.models; errors; _ } =
    ModelParser.parse
      ~pyre_api
      ~source:initial_models_source
      ~taint_configuration:TaintConfiguration.Heap.default
      ~source_sink_filter:None
      ~definitions:None
      ~stubs:
        ([]
        |> Interprocedural.Target.HashsetSharedMemory.from_heap
        |> Interprocedural.Target.HashsetSharedMemory.read_only)
      ~python_version:(ModelParser.PythonVersion.create ())
      ()
  in
  assert_bool
    (Format.sprintf
       "The models shouldn't have any parsing errors:\n%s."
       (List.map errors ~f:ModelVerificationError.display |> String.concat ~sep:"\n"))
    (List.is_empty errors);
  models


module TestEnvironment = struct
  type t = {
    static_analysis_configuration: Configuration.StaticAnalysis.t;
    taint_configuration: TaintConfiguration.Heap.t;
    taint_configuration_shared_memory: TaintConfiguration.SharedMemory.t;
    whole_program_call_graph: CallGraph.WholeProgramCallGraph.t;
    define_call_graphs: CallGraph.SharedMemory.t;
    get_define_call_graph:
      Interprocedural.Target.t -> Interprocedural.CallGraph.DefineCallGraph.t option;
    call_graph_fixpoint_state: CallGraphFixpoint.t;
    override_graph_heap: OverrideGraph.Heap.t;
    override_graph_shared_memory: OverrideGraph.SharedMemory.t;
    initial_callables: FetchCallables.t;
    stubs: Target.t list;
    initial_models: TaintFixpoint.SharedModels.t;
    model_query_results: ModelQueryExecution.ExecutionResult.t;
    pyre_api: PyrePysaApi.ReadOnly.t;
    class_interval_graph: ClassIntervalSetGraph.Heap.t;
    class_interval_graph_shared_memory: ClassIntervalSetGraph.SharedMemory.t;
    global_constants: GlobalConstants.SharedMemory.t;
    stubs_shared_memory_handle: Target.HashsetSharedMemory.t;
    callables_to_definitions_map: Interprocedural.CallablesSharedMemory.ReadWrite.t;
    callables_to_decorators_map: Interprocedural.CallableToDecoratorsMap.SharedMemory.t;
    type_of_expression_shared_memory: Interprocedural.TypeOfExpressionSharedMemory.t;
  }

  let cleanup
      {
        static_analysis_configuration = _;
        taint_configuration = _;
        taint_configuration_shared_memory = _;
        whole_program_call_graph = _;
        define_call_graphs;
        get_define_call_graph = _;
        call_graph_fixpoint_state = { CallGraphFixpoint.fixpoint; _ };
        override_graph_heap = _;
        override_graph_shared_memory;
        initial_callables = _;
        stubs = _;
        initial_models;
        model_query_results = _;
        pyre_api = _;
        class_interval_graph;
        class_interval_graph_shared_memory;
        global_constants;
        stubs_shared_memory_handle;
        callables_to_definitions_map;
        callables_to_decorators_map;
        type_of_expression_shared_memory = _;
      }
    =
    CallGraph.SharedMemory.cleanup define_call_graphs;
    CallGraphFixpoint.cleanup ~keep_models:false fixpoint;
    OverrideGraph.SharedMemory.cleanup override_graph_shared_memory;
    (* Clean up nitial_models, in case we didn't actually compute the fixpoint for that test. In
       tests where we do compute the fixpoint, it's fine to cleanup models twice. *)
    Taint.TaintFixpoint.SharedModels.cleanup ~clean_old:true initial_models;
    ClassIntervalSetGraph.SharedMemory.cleanup
      class_interval_graph_shared_memory
      class_interval_graph;
    Target.HashsetSharedMemory.cleanup stubs_shared_memory_handle;
    GlobalConstants.SharedMemory.cleanup global_constants;
    Interprocedural.CallablesSharedMemory.ReadWrite.cleanup callables_to_definitions_map;
    Interprocedural.CallableToDecoratorsMap.SharedMemory.cleanup callables_to_decorators_map
end

let set_up_decorator_preprocessing ~handle models =
  let decorator_actions =
    models
    >>| (fun models ->
          ModelParser.parse_model_modes
            ~path:(PyrePath.create_absolute handle)
            ~source:(Test.trim_extra_indentation models)
          |> ModelParser.decorator_actions_from_modes)
    |> Option.value ~default:Reference.SerializableMap.empty
  in
  PyrePysaLogic.DecoratorPreprocessing.setup_preprocessing
    { actions = decorator_actions; enable_discarding = true }


let initialize_pyre_and_fail_on_errors ~context ~handle ~source_content ~models_source =
  let configuration, pyre_api, errors =
    let project = Test.ScratchProject.setup ~context [handle, source_content] in
    set_up_decorator_preprocessing ~handle models_source;
    let _, errors = Test.ScratchProject.build_type_environment_and_postprocess project in
    ( Test.ScratchProject.configuration_of project,
      project |> Test.ScratchProject.pyre_pysa_read_only_api |> PyrePysaApi.ReadOnly.from_pyre1_api,
      errors )
  in
  (if not (List.is_empty errors) then
     let errors =
       errors
       |> List.map ~f:(fun error ->
              let error =
                PyrePysaLogic.Testing.AnalysisError.instantiate
                  ~show_error_traces:false
                  ~lookup:(PyrePysaApi.ReadOnly.relative_path_of_qualifier pyre_api)
                  error
              in
              Format.asprintf
                "%a:%s"
                Location.WithPath.pp
                (PyrePysaLogic.Testing.AnalysisError.Instantiated.location error)
                (PyrePysaLogic.Testing.AnalysisError.Instantiated.description error))
       |> String.concat ~sep:"\n"
     in
     failwithf "Pyre errors were found in `%s`:\n%s" handle errors ());
  configuration, pyre_api


let source_from_qualifier ~pyre_api qualifier =
  qualifier |> PyrePysaApi.ReadOnly.source_of_qualifier pyre_api |> Option.value_exn


let initialize
    ?(handle = "test.py")
    ?models_source
    ?(add_initial_models = true)
    ?find_missing_flows
    ?(taint_configuration = TaintConfiguration.Heap.default)
    ?(verify_empty_model_queries = true)
    ?model_path
    ?(maximum_target_depth = Configuration.StaticAnalysis.default_maximum_target_depth)
    ~context
    source_content
  =
  let configuration, pyre_api =
    initialize_pyre_and_fail_on_errors ~context ~handle ~source_content ~models_source
  in
  let taint_configuration_shared_memory =
    TaintConfiguration.SharedMemory.from_heap taint_configuration
  in
  let static_analysis_configuration =
    Configuration.StaticAnalysis.create
      ~maximum_target_depth
      ~higher_order_call_graph_max_iterations:
        Configuration.StaticAnalysis.default_higher_order_call_graph_max_iterations_in_tests
      configuration
      ?find_missing_flows
      ()
  in
  let qualifier = Reference.create (String.chop_suffix_exn handle ~suffix:".py") in
  let initial_callables_in_source =
    FetchCallables.from_qualifier ~configuration ~pyre_api ~qualifier
  in
  let stubs = FetchCallables.get_stubs initial_callables_in_source in
  let definitions = FetchCallables.get_definitions initial_callables_in_source in
  let class_hierarchy_graph = ClassHierarchyGraph.Heap.from_qualifier ~pyre_api ~qualifier in
  let stubs_shared_memory_handle = Target.HashsetSharedMemory.from_heap stubs in
  let scheduler = Test.mock_scheduler () in
  let scheduler_policy = Scheduler.Policy.legacy_fixed_chunk_count () in
  let qualifiers = PyrePysaApi.ReadOnly.explicit_qualifiers pyre_api in
  let all_initial_callables =
    (* Also include typeshed stubs so that we can build `qualifiers_defines` for them. *)
    Interprocedural.FetchCallables.from_qualifiers
      ~scheduler
      ~scheduler_policy
      ~configuration
      ~pyre_api
      ~qualifiers
  in
  let definitions_and_stubs =
    Interprocedural.FetchCallables.get all_initial_callables ~definitions:true ~stubs:true
  in
  let callables_to_definitions_map =
    Interprocedural.CallablesSharedMemory.ReadWrite.from_callables
      ~scheduler
      ~scheduler_policy
      ~pyre_api
      definitions_and_stubs
  in
  let type_of_expression_shared_memory =
    Interprocedural.TypeOfExpressionSharedMemory.create
      ~pyre_api
      ~callables_to_definitions_map:
        (Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
      ()
  in
  let user_models, model_query_results =
    let models_source =
      match models_source, add_initial_models with
      | Some source, true ->
          Some (Format.sprintf "%s\n%s" (Test.trim_extra_indentation source) initial_models_source)
      | None, true -> Some initial_models_source
      | models_source, _ -> models_source
    in
    match models_source with
    | None -> SharedModels.create (), ModelQueryExecution.ExecutionResult.create_empty ()
    | Some source ->
        let stubs_shared_memory = Target.HashsetSharedMemory.from_heap stubs in
        PyrePysaApi.ModelQueries.invalidate_cache pyre_api;
        let { ModelParseResult.models = regular_models; errors; queries } =
          ModelParser.parse
            ~pyre_api
            ?path:model_path
            ~source:(Test.trim_extra_indentation source)
            ~taint_configuration
            ~source_sink_filter:(Some taint_configuration.source_sink_filter)
            ~definitions:(Some (Target.HashSet.of_list definitions))
            ~stubs:(Target.HashsetSharedMemory.read_only stubs_shared_memory)
            ~python_version:(ModelParser.PythonVersion.create ())
            ()
        in
        assert_bool
          (Format.sprintf
             "The models shouldn't have any parsing errors:\n%s\nModels:\n%s"
             (List.map errors ~f:ModelVerificationError.display |> String.concat ~sep:"\n")
             source)
          (List.is_empty errors);

        let model_query_results =
          ModelQueryExecution.generate_models_from_queries
            ~pyre_api
            ~scheduler
            ~scheduler_policies:Configuration.SchedulerPolicies.empty
            ~class_hierarchy_graph
            ~callables_to_definitions_map:
              (Interprocedural.CallablesSharedMemory.ReadOnly.read_only
                 callables_to_definitions_map)
            ~source_sink_filter:(Some taint_configuration.source_sink_filter)
            ~verbose:false
            ~error_on_unexpected_models:true
            ~error_on_empty_result:verify_empty_model_queries
            ~definitions_and_stubs:(List.rev_append stubs definitions)
            ~stubs:(Target.HashsetSharedMemory.read_only stubs_shared_memory_handle)
            queries
        in
        let errors = ModelQueryExecution.ExecutionResult.get_errors model_query_results in
        ModelVerificationError.verify_models_and_dsl ~raise_exception:true errors;
        let models_from_queries =
          ModelQueryExecution.ExecutionResult.get_models model_query_results
        in
        let models =
          SharedModels.join_with_registry_sequential
            models_from_queries
            ~model_join:Model.join_user_models
            regular_models
        in
        let models =
          MissingFlow.add_obscure_models
            ~scheduler
            ~static_analysis_configuration
            ~callables_to_definitions_map:
              (Interprocedural.CallablesSharedMemory.ReadOnly.read_only
                 callables_to_definitions_map)
            ~stubs:(Target.HashSet.of_list stubs)
            ~initial_models:models
        in
        models, model_query_results
  in
  let initial_models =
    ClassModels.infer
      ~scheduler
      ~scheduler_policies:Configuration.SchedulerPolicies.empty
      ~pyre_api
      ~user_models:(SharedModels.read_only user_models)
    |> SharedModels.join_with_registry_sequential user_models ~model_join:Model.join_user_models
  in
  (* Overrides must be done first, as they influence the call targets. *)
  let { OverrideGraph.Heap.overrides = override_graph_heap; _ } =
    qualifier
    |> OverrideGraph.Heap.from_qualifier
         ~pyre_api
         ~skip_overrides_targets:(SharedModels.skip_overrides ~scheduler initial_models)
    |> OverrideGraph.Heap.cap_overrides
         ~analyze_all_overrides_targets:
           (SharedModels.analyze_all_overrides ~scheduler initial_models)
         ~maximum_overrides:(TaintConfiguration.maximum_overrides_to_analyze taint_configuration)
  in
  let override_graph_shared_memory = OverrideGraph.SharedMemory.from_heap override_graph_heap in
  let override_graph_shared_memory_read_only =
    Interprocedural.OverrideGraph.SharedMemory.read_only override_graph_shared_memory
  in

  let global_constants =
    GlobalConstants.Heap.from_qualifier
      ~pyre_api
      ~callables_to_definitions_map:
        (Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
      qualifier
    |> GlobalConstants.SharedMemory.from_heap
  in

  (* Initialize models *)
  (* The call graph building depends on initial models for global targets. *)
  let callables_to_decorators_map =
    Interprocedural.CallableToDecoratorsMap.SharedMemory.create
      ~callables_to_definitions_map:
        (Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
      ~scheduler
      ~scheduler_policy
      ~is_pyrefly:(PyrePysaApi.ReadOnly.is_pyrefly pyre_api)
      definitions
  in
  let skip_analysis_targets =
    initial_models
    |> SharedModels.skip_analysis ~scheduler
    |> Target.Set.elements
    |> Target.HashSet.of_list
  in
  let ({ CallGraph.SharedMemory.whole_program_call_graph; define_call_graphs } as call_graph) =
    CallGraphBuilder.build_whole_program_call_graph
      ~scheduler
      ~static_analysis_configuration
      ~pyre_api
      ~resolve_module_path:None
      ~override_graph:(Some override_graph_shared_memory_read_only)
      ~store_shared_memory:true
      ~attribute_targets:(SharedModels.object_targets initial_models)
      ~skip_analysis_targets
      ~check_invariants:true
      ~definitions
      ~callables_to_definitions_map:
        (Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
      ~callables_to_decorators_map:
        (Interprocedural.CallableToDecoratorsMap.SharedMemory.read_only callables_to_decorators_map)
      ~type_of_expression_shared_memory
      ~create_dependency_for:Interprocedural.CallGraph.AllTargetsUseCase.CallGraphDependency
  in
  let dependency_graph =
    DependencyGraph.build_whole_program_dependency_graph
      ~static_analysis_configuration
      ~prune:DependencyGraph.PruneMethod.None
      ~initial_callables:initial_callables_in_source
      ~call_graph:whole_program_call_graph
      ~overrides:override_graph_heap
      ~ignore_decorated_targets:false
  in
  let ({ CallGraphFixpoint.whole_program_call_graph; get_define_call_graph; _ } as
      call_graph_fixpoint_state)
    =
    CallGraphFixpoint.compute
      ~scheduler
      ~scheduler_policy
      ~static_analysis_configuration
      ~resolve_module_path:None
      ~pyre_api
      ~call_graph
      ~dependency_graph
      ~override_graph_shared_memory
      ~callables_to_definitions_map
      ~callables_to_decorators_map
      ~type_of_expression_shared_memory
      ~skip_analysis_targets
      ~called_when_parameter:(SharedModels.called_when_parameter ~scheduler initial_models)
  in
  let initial_models =
    MissingFlow.add_unknown_callee_models
      ~static_analysis_configuration
      ~call_graph:whole_program_call_graph
      ~initial_models
  in
  let class_interval_graph =
    ClassIntervalSetGraph.Heap.from_class_hierarchy class_hierarchy_graph
  in
  let class_interval_graph_shared_memory =
    ClassIntervalSetGraph.SharedMemory.from_heap class_interval_graph
  in
  {
    TestEnvironment.static_analysis_configuration;
    taint_configuration;
    taint_configuration_shared_memory;
    whole_program_call_graph;
    define_call_graphs;
    get_define_call_graph;
    call_graph_fixpoint_state;
    override_graph_heap;
    override_graph_shared_memory;
    initial_callables = initial_callables_in_source;
    model_query_results;
    stubs;
    initial_models;
    pyre_api;
    class_interval_graph;
    class_interval_graph_shared_memory;
    global_constants;
    stubs_shared_memory_handle;
    callables_to_definitions_map;
    callables_to_decorators_map;
    type_of_expression_shared_memory;
  }


type mismatch_file = {
  path: PyrePath.t;
  suffix: string;
  expected: string;
  actual: string;
}

let end_to_end_integration_test path context =
  let create_expected_and_actual_files ~suffix actual =
    let output_filename ~suffix ~initial =
      if initial then
        PyrePath.with_suffix path ~suffix
      else
        PyrePath.with_suffix path ~suffix:(suffix ^ ".actual")
    in
    let write_output ~suffix ?(initial = false) content =
      try output_filename ~suffix ~initial |> File.create ~content |> File.write with
      | CamlUnix.Unix_error _ ->
          failwith (Format.asprintf "Could not write `%s` file for %a" suffix PyrePath.pp path)
    in
    let remove_old_output ~suffix =
      try output_filename ~suffix ~initial:false |> PyrePath.unlink_if_exists with
      | Sys_error _ ->
          (* be silent *)
          ()
    in
    let get_expected ~suffix =
      try PyrePath.with_suffix path ~suffix |> File.create |> File.content with
      | CamlUnix.Unix_error _ -> None
    in
    match get_expected ~suffix with
    | None ->
        (* expected file does not exist, create it *)
        write_output ~suffix actual ~initial:true;
        None
    | Some expected ->
        if String.equal expected actual then (
          remove_old_output ~suffix;
          None)
        else (
          write_output ~suffix actual;
          Some { path; suffix; expected; actual })
  in
  let error_on_actual_files { path; suffix; expected; actual } =
    Printf.printf
      "%s"
      (Format.asprintf
         "Expectations differ for %s %s\n%a"
         suffix
         (PyrePath.show path)
         (Test.diff ~print:String.pp)
         (expected, actual))
  in
  let divergent_files, serialized_models =
    let source = File.create path |> File.content |> fun content -> Option.value_exn content in
    let models_source =
      try
        let model_path = PyrePath.with_suffix path ~suffix:".pysa" in
        File.create model_path |> File.content
      with
      | CamlUnix.Unix_error _ -> None
    in
    let taint_configuration =
      try
        let path = PyrePath.with_suffix path ~suffix:".config" in
        File.create path
        |> File.content
        |> Option.map ~f:(fun content ->
               JsonParsing.JsonAst.Json.from_string content
               |> function
               | Core.Result.Ok json ->
                   TaintConfiguration.from_json_list [path, json]
                   |> TaintConfiguration.exception_on_error
               | Core.Result.Error { message; location; _ } ->
                   TaintConfiguration.exception_on_error
                     (Core.Result.Error
                        [
                          TaintConfiguration.Error.create_with_location
                            ~path
                            ~kind:(TaintConfiguration.Error.InvalidJson message)
                            ~location;
                        ]))
      with
      | CamlUnix.Unix_error _ -> None
    in
    let add_initial_models = Option.is_none models_source && Option.is_none taint_configuration in
    let taint_configuration =
      taint_configuration |> Option.value ~default:TaintConfiguration.Heap.default
    in
    let handle = PyrePath.show path |> String.split ~on:'/' |> List.last_exn in
    let create_call_graph_files call_graph =
      let actual =
        Format.asprintf
          "@%s\nCall dependencies\n%s"
          "generated"
          (call_graph
          |> CallGraph.WholeProgramCallGraph.to_target_graph
          |> TargetGraph.to_json ~skip_empty_callees:true ~sorted:true
          |> Yojson.Safe.pretty_to_string)
      in
      create_expected_and_actual_files ~suffix:".cg" actual
    in
    let create_higher_order_call_graph_files call_graph_fixpoint_state =
      let content =
        call_graph_fixpoint_state.CallGraphFixpoint.fixpoint
        |> CallGraphFixpoint.analyzed_callables
        |> List.dedup_and_sort ~compare:Target.compare
        |> List.filter_map ~f:(fun callable ->
               match CallGraphFixpoint.get_model call_graph_fixpoint_state callable with
               | Some call_graph
                 when not (CallGraphBuilder.HigherOrderCallGraph.is_empty call_graph) ->
                   let json =
                     `Assoc
                       (("callable", `String (Target.show_pretty callable))
                       :: CallGraphBuilder.HigherOrderCallGraph.to_json_alist call_graph)
                   in
                   Some (Yojson.Safe.pretty_to_string json)
               | _ -> None)
        |> String.concat ~sep:"\n"
      in
      let actual = Format.asprintf "@%s\nHigher order call graphs\n%s" "generated" content in
      create_expected_and_actual_files ~suffix:".hofcg" actual
    in
    let create_overrides_files overrides =
      let actual =
        Format.asprintf
          "@%s\nOverrides\n%a"
          "generated"
          TargetGraph.pp
          (DependencyGraph.Reversed.to_target_graph
             (DependencyGraph.Reversed.from_overrides overrides))
      in
      create_expected_and_actual_files ~suffix:".overrides" actual
    in
    let {
      TestEnvironment.static_analysis_configuration;
      taint_configuration;
      taint_configuration_shared_memory;
      whole_program_call_graph;
      get_define_call_graph;
      pyre_api;
      override_graph_heap;
      override_graph_shared_memory;
      initial_models;
      initial_callables;
      stubs;
      class_interval_graph_shared_memory;
      global_constants;
      call_graph_fixpoint_state;
      callables_to_definitions_map;
      _;
    }
      =
      try
        initialize
          ~handle
          ?models_source
          ~add_initial_models
          ~taint_configuration
          ~verify_empty_model_queries:false
          ~context
          source
      with
      | ModelVerificationError.ModelVerificationErrors errors as exn ->
          Printf.printf "Unexpected model verification errors:\n";
          List.iter errors ~f:(fun error ->
              Printf.printf "%s\n" (ModelVerificationError.display error));
          raise exn
    in
    let scheduler = Test.mock_scheduler () in
    let entrypoints = SharedModels.entrypoints ~scheduler initial_models in
    let prune_method =
      match entrypoints with
      | [] -> Interprocedural.DependencyGraph.PruneMethod.Internals
      | entrypoints -> Interprocedural.DependencyGraph.PruneMethod.Entrypoints entrypoints
    in
    let { DependencyGraph.dependency_graph; callables_to_analyze; override_targets; _ } =
      DependencyGraph.build_whole_program_dependency_graph
        ~static_analysis_configuration
        ~prune:prune_method
        ~initial_callables
        ~call_graph:whole_program_call_graph
        ~overrides:override_graph_heap
        ~ignore_decorated_targets:true
    in
    let initial_models =
      SharedModels.initialize_for_parameterized_callables
        ~higher_order_call_graph_fixpoint:call_graph_fixpoint_state.CallGraphFixpoint.fixpoint
        initial_models
    in
    let fixpoint_state =
      TaintFixpoint.record_initial_models
        ~scheduler
        ~initial_models
        ~callables_to_analyze
        ~stubs
        ~override_targets
    in
    let override_graph_shared_memory_read_only =
      Interprocedural.OverrideGraph.SharedMemory.read_only override_graph_shared_memory
    in
    let fixpoint =
      TaintFixpoint.compute
        ~scheduler
        ~scheduler_policy:(Scheduler.Policy.legacy_fixed_chunk_count ())
        ~override_graph:override_graph_shared_memory_read_only
        ~dependency_graph
        ~skip_analysis_targets:
          (initial_models
          |> SharedModels.skip_analysis ~scheduler
          |> Target.Set.elements
          |> Target.HashSet.of_list)
        ~context:
          {
            TaintFixpoint.Context.taint_configuration = taint_configuration_shared_memory;
            pyre_api;
            class_interval_graph = class_interval_graph_shared_memory;
            get_define_call_graph;
            global_constants =
              Interprocedural.GlobalConstants.SharedMemory.read_only global_constants;
            type_of_expression_shared_memory =
              Interprocedural.TypeOfExpressionSharedMemory.create
                ~pyre_api
                ~callables_to_definitions_map:
                  (Interprocedural.CallablesSharedMemory.ReadOnly.read_only
                     callables_to_definitions_map)
                ();
            callables_to_definitions_map =
              Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map;
          }
        ~callables_to_analyze
        ~max_iterations:100
        ~error_on_max_iterations:true
        ~epoch:TaintFixpoint.Epoch.initial
        ~state:fixpoint_state
    in
    let resolve_module_path qualifier =
      PyrePysaApi.ReadOnly.relative_path_of_qualifier pyre_api qualifier
      >>| fun filename ->
      { RepositoryPath.filename = Some filename; path = PyrePath.create_absolute filename }
    in

    let divergent_files =
      [
        create_call_graph_files whole_program_call_graph;
        create_higher_order_call_graph_files call_graph_fixpoint_state;
        create_overrides_files override_graph_heap;
      ]
    in
    let serialized_models =
      let callables_to_definitions_map =
        Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map
      in
      callables_to_analyze
      |> List.rev_append (TaintFixpoint.SharedModels.targets initial_models)
      |> List.dedup_and_sort ~compare:Target.compare
      |> TaintReporting.fetch_and_externalize
           ~taint_configuration
           ~fixpoint_state:(TaintFixpoint.State.read_only fixpoint.TaintFixpoint.state)
           ~resolve_module_path
           ~resolve_callable_location:
             (Interprocedural.CallablesSharedMemory.ReadOnly.get_location_opt
                callables_to_definitions_map)
           ~override_graph:override_graph_shared_memory_read_only
           ~dump_override_models:true
           ~sorted:true
      |> List.map ~f:NewlineDelimitedJson.Line.to_json
      |> List.map ~f:(fun json -> Yojson.Safe.pretty_to_string ~std:true json ^ "\n")
      |> String.concat ~sep:""
    in
    let () = Memory.reset_shared_memory () in
    divergent_files, serialized_models
  in
  let divergent_files =
    create_expected_and_actual_files ~suffix:".models" ("@" ^ "generated\n" ^ serialized_models)
    :: divergent_files
    |> List.filter_opt
  in
  List.iter divergent_files ~f:error_on_actual_files;
  if not (List.is_empty divergent_files) then
    let message =
      List.map divergent_files ~f:(fun { path; suffix; _ } ->
          Format.asprintf "%a%s" PyrePath.pp path suffix)
      |> String.concat ~sep:", "
      |> Format.sprintf "Found differences in %s."
    in
    assert_bool message false


let find_pyre_source_code_root () =
  match Stdlib.Sys.getenv_opt "PYRE_CODE_ROOT" with
  | Some pyre_root_string -> PyrePath.create_absolute pyre_root_string
  | None -> (
      let current_directory = PyrePath.current_working_directory () in
      match
        PyrePath.search_upwards
          ~target:"source"
          ~target_type:PyrePath.FileType.Directory
          ~root:current_directory
      with
      | Some pyre_root -> pyre_root
      | None -> current_directory)


let end_to_end_test_paths relative_path =
  let file_filter name =
    String.is_suffix ~suffix:".py" name
    && (not (String.contains name '#'))
    && not (String.contains name '~')
  in
  find_pyre_source_code_root ()
  |> (fun root -> PyrePath.create_relative ~root ~relative:relative_path)
  |> fun root -> PyrePath.list ~file_filter ~root ()


let end_to_end_test_paths_found relative_path _ =
  if List.is_empty (end_to_end_test_paths relative_path) then
    assert_bool "No test paths to check." false


let setup_single_py_file
    ?(force_pyre1 = false)
    ?(requires_type_of_expressions = true)
    ~file_name
    ~context
    ~source
    ()
  =
  let test_module_name = Reference.create (String.chop_suffix_exn file_name ~suffix:".py") in
  let project =
    Test.ScratchPyrePysaProject.setup
      ~context
      ~force_pyre1
      ~requires_type_of_expressions
      [file_name, source]
  in
  let pyre_api = Test.ScratchPyrePysaProject.read_only_api project in
  test_module_name, pyre_api, Test.ScratchPyrePysaProject.configuration_of project
