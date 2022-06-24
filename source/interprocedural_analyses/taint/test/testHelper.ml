(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module TypeCheck = Analysis.TypeCheck
open Core
open OUnit2
open Analysis
module AnalysisError = Analysis.AnalysisError
open Ast
open Pyre
open Taint
open Interprocedural

type parameter_taint = {
  name: string;
  sinks: Sinks.t list;
}

type parameter_source_taint = {
  name: string;
  sources: Sources.t list;
}

type parameter_sanitize = {
  name: string;
  sanitize: Domains.Sanitize.t;
}

type error_expectation = {
  code: int;
  pattern: string;
}

type expectation = {
  kind: [ `Function | `Method | `Override | `Object | `PropertySetter ];
  define_name: string;
  source_parameters: parameter_source_taint list;
  sink_parameters: parameter_taint list;
  tito_parameters: parameter_taint list;
  returns: Sources.t list;
  errors: error_expectation list;
  global_sanitizer: Domains.Sanitize.t;
  parameters_sanitizer: Domains.Sanitize.t;
  return_sanitizer: Domains.Sanitize.t;
  parameter_sanitizers: parameter_sanitize list;
  analysis_modes: Model.ModeSet.t;
}

let outcome
    ~kind
    ?(source_parameters = [])
    ?(sink_parameters = [])
    ?(tito_parameters = [])
    ?(returns = [])
    ?(errors = [])
    ?(global_sanitizer = Domains.Sanitize.empty)
    ?(parameters_sanitizer = Domains.Sanitize.empty)
    ?(return_sanitizer = Domains.Sanitize.empty)
    ?(parameter_sanitizers = [])
    ?(analysis_modes = Model.ModeSet.empty)
    define_name
  =
  {
    kind;
    define_name;
    source_parameters;
    sink_parameters;
    tito_parameters;
    returns;
    errors;
    global_sanitizer;
    parameters_sanitizer;
    return_sanitizer;
    parameter_sanitizers;
    analysis_modes;
  }


let create_callable kind define_name =
  let name = Reference.create define_name in
  match kind with
  | `Method -> Target.create_method name
  | `Function -> Target.create_function name
  | `PropertySetter -> Target.create_property_setter name
  | `Override -> Target.create_override name
  | `Object -> Target.create_object name


let check_expectation
    ~get_model
    ~get_errors
    ~environment
    {
      kind;
      define_name;
      source_parameters;
      sink_parameters;
      tito_parameters;
      returns;
      errors;
      global_sanitizer;
      parameters_sanitizer;
      return_sanitizer;
      parameter_sanitizers;
      analysis_modes = expected_analysis_modes;
    }
  =
  let callable = create_callable kind define_name in
  let extract_sinks_by_parameter_name (root, sink_tree) sink_map =
    match AccessPath.Root.parameter_name root with
    | Some name ->
        let sinks =
          Domains.BackwardState.Tree.collapse ~transform:Fn.id sink_tree
          |> Domains.BackwardTaint.kinds
        in
        let sinks =
          String.Map.find sink_map name
          |> Option.value ~default:[]
          |> List.rev_append sinks
          |> List.dedup_and_sort ~compare:Sinks.compare
        in
        String.Map.set sink_map ~key:name ~data:sinks
    | _ -> sink_map
  in
  let extract_sources_by_parameter_name (root, source_tree) sink_map =
    match AccessPath.Root.parameter_name root with
    | Some name ->
        let sinks =
          Domains.ForwardState.Tree.collapse ~transform:Fn.id source_tree
          |> Domains.ForwardTaint.kinds
        in
        let sinks =
          String.Map.find sink_map name
          |> Option.value ~default:[]
          |> List.rev_append sinks
          |> List.dedup_and_sort ~compare:Sources.compare
        in
        String.Map.set sink_map ~key:name ~data:sinks
    | _ -> sink_map
  in
  let { Model.backward; forward; sanitizers; modes } =
    Option.value_exn
      ~message:(Format.asprintf "Model not found for %a" Target.pp callable)
      (get_model callable)
  in
  assert_equal ~printer:Model.ModeSet.show modes expected_analysis_modes;
  let sink_taint_map =
    Domains.BackwardState.fold
      Domains.BackwardState.KeyValue
      backward.sink_taint
      ~f:extract_sinks_by_parameter_name
      ~init:String.Map.empty
  in
  let parameter_source_taint_map =
    Domains.ForwardState.fold
      Domains.ForwardState.KeyValue
      forward.source_taint
      ~f:extract_sources_by_parameter_name
      ~init:String.Map.empty
  in
  let extract_parameter_sanitize map (root, sanitize) =
    match AccessPath.Root.parameter_name root with
    | Some name -> String.Map.set map ~key:name ~data:sanitize
    | _ -> map
  in
  let parameter_sanitize_map =
    Domains.SanitizeRootMap.to_alist sanitizers.roots
    |> List.fold ~init:String.Map.empty ~f:extract_parameter_sanitize
  in
  let parameter_taint_in_taint_out_map =
    Domains.BackwardState.fold
      Domains.BackwardState.KeyValue
      backward.taint_in_taint_out
      ~f:extract_sinks_by_parameter_name
      ~init:String.Map.empty
  in
  let print_list ~show list =
    list |> List.map ~f:show |> String.concat ~sep:", " |> Format.asprintf "[%s]"
  in
  let check_each_sink ~key:name ~data =
    match data with
    | `Both (expected, actual) ->
        assert_equal
          ~cmp:(List.equal Sinks.equal)
          ~printer:(print_list ~show:Sinks.show)
          ~msg:(Format.sprintf "Define %s Parameter %s" define_name name)
          expected
          actual
    | `Left expected ->
        assert_equal
          ~cmp:(List.equal Sinks.equal)
          ~printer:(print_list ~show:Sinks.show)
          ~msg:(Format.sprintf "Define %s Parameter %s" define_name name)
          expected
          []
    | `Right _ ->
        (* Okay, we may have outcomes we don't care about *)
        ()
  in
  let check_each_source_parameter ~key:name ~data =
    match data with
    | `Both (expected, actual) ->
        assert_equal
          ~cmp:(List.equal Sources.equal)
          ~printer:(print_list ~show:Sources.show)
          ~msg:(Format.sprintf "Define %s Parameter %s" define_name name)
          expected
          actual
    | `Left expected ->
        assert_equal
          ~cmp:(List.equal Sources.equal)
          ~printer:(print_list ~show:Sources.show)
          ~msg:(Format.sprintf "Define %s Parameter %s" define_name name)
          expected
          []
    | `Right _ ->
        (* Okay, we may have outcomes we don't care about *)
        ()
  in
  let check_each_sanitize ~key:name ~data =
    match data with
    | `Both (expected, actual) ->
        assert_equal
          ~cmp:Domains.Sanitize.equal
          ~printer:Domains.Sanitize.show
          ~msg:(Format.sprintf "Define %s Parameter %s" define_name name)
          expected
          actual
    | `Left expected ->
        assert_equal
          ~cmp:Domains.Sanitize.equal
          ~printer:Domains.Sanitize.show
          ~msg:(Format.sprintf "Define %s Parameter %s" define_name name)
          expected
          Domains.Sanitize.empty
    | `Right actual ->
        assert_equal
          ~cmp:Domains.Sanitize.equal
          ~printer:Domains.Sanitize.show
          ~msg:(Format.sprintf "Define %s Parameter %s" define_name name)
          Domains.Sanitize.empty
          actual
  in
  let expected_sinks =
    List.map ~f:(fun { name; sinks } -> name, sinks) sink_parameters |> String.Map.of_alist_exn
  in
  let expected_parameter_sources =
    List.map ~f:(fun { name; sources } -> name, sources) source_parameters
    |> String.Map.of_alist_exn
  in
  let expected_parameter_sanitizers =
    List.map ~f:(fun { name; sanitize } -> name, sanitize) parameter_sanitizers
    |> String.Map.of_alist_exn
  in
  (* Check sources. *)
  let returned_sources =
    Domains.ForwardState.read ~root:AccessPath.Root.LocalResult ~path:[] forward.source_taint
    |> Domains.ForwardState.Tree.collapse ~transform:Fn.id
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
    (Map.length expected_sinks)
    (Map.length sink_taint_map)
    ~printer:Int.to_string
    ~msg:(Format.sprintf "Define %s: List of sink tainted parameters differ in length." define_name);
  String.Map.iter2 ~f:check_each_sink expected_sinks sink_taint_map;

  (* Check parameter sources. *)
  assert_equal
    (Map.length expected_parameter_sources)
    (Map.length parameter_source_taint_map)
    ~printer:Int.to_string
    ~msg:
      (Format.sprintf "Define %s: List of source tainted parameters differ in length." define_name);
  String.Map.iter2
    ~f:check_each_source_parameter
    expected_parameter_sources
    parameter_source_taint_map;
  let expected_tito =
    List.map ~f:(fun { name; sinks } -> name, sinks) tito_parameters |> String.Map.of_alist_exn
  in
  assert_equal
    (Map.length expected_tito)
    (Map.length parameter_taint_in_taint_out_map)
    ~printer:Int.to_string
    ~msg:(Format.sprintf "Define %s: List of tito parameters differ in length." define_name);
  String.Map.iter2 ~f:check_each_sink expected_tito parameter_taint_in_taint_out_map;

  (* Check sanitizers *)
  assert_equal
    ~cmp:Domains.Sanitize.equal
    ~printer:Domains.Sanitize.show
    sanitizers.global
    global_sanitizer;
  assert_equal
    ~cmp:Domains.Sanitize.equal
    ~printer:Domains.Sanitize.show
    sanitizers.parameters
    parameters_sanitizer;
  assert_equal
    ~cmp:Domains.Sanitize.equal
    ~printer:Domains.Sanitize.show
    (Domains.SanitizeRootMap.get AccessPath.Root.LocalResult sanitizers.roots)
    return_sanitizer;

  assert_equal
    (Map.length expected_parameter_sanitizers)
    (Map.length parameter_sanitize_map)
    ~printer:Int.to_string
    ~msg:(Format.sprintf "Define %s: List of parameter sanitizers differ in length." define_name);
  String.Map.iter2 ~f:check_each_sanitize expected_parameter_sanitizers parameter_sanitize_map;

  (* Check errors *)
  let actual_errors =
    let ast_environment = TypeEnvironment.ReadOnly.ast_environment environment in
    let to_analysis_error =
      Error.instantiate
        ~show_error_traces:true
        ~lookup:(AstEnvironment.ReadOnly.get_relative ast_environment)
    in
    get_errors callable |> List.map ~f:Issue.to_error |> List.map ~f:to_analysis_error
  in
  assert_errors errors actual_errors


let initial_models_source =
  {|
      def _test_sink(arg: TaintSink[Test, Via[special_sink]]): ...
      def _test_source() -> TaintSource[Test, Via[special_source]]: ...
      def _tito( *x: TaintInTaintOut, **kw: TaintInTaintOut): ...
      def eval(arg: TaintSink[RemoteCodeExecution]): ...
      def _user_controlled() -> TaintSource[UserControlled]: ...
      def _cookies() -> TaintSource[Cookies]: ...
      def _rce(argument: TaintSink[RemoteCodeExecution]): ...
      def _sql(argument: TaintSink[SQL]): ...
      @SkipObscure
      def getattr(
          o: TaintInTaintOut[Via[object]],
          name: TaintSink[GetAttr],
          default: TaintInTaintOut[Via[default]] = ...,
      ): ...

      taint._global_sink: TaintSink[Test] = ...
      ClassWithSinkAttribute.attribute: TaintSink[Test] = ...

      def copy(obj: TaintInTaintOut[Via[copy]]): ...
    |}
  |> Test.trim_extra_indentation


let get_initial_models ~context =
  let global_resolution =
    Test.ScratchProject.setup ~context [] |> Test.ScratchProject.build_global_resolution
  in
  let { ModelParser.models; errors; _ } =
    ModelParser.parse
      ~resolution:
        (TypeCheck.resolution
           global_resolution
           (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
           (module TypeCheck.DummyContext))
      ~source:initial_models_source
      ~configuration:TaintConfiguration.default
      ~callables:None
      ~stubs:(Interprocedural.Target.HashSet.create ())
      ()
  in
  assert_bool
    (Format.sprintf
       "The models shouldn't have any parsing errors:\n%s."
       (List.map errors ~f:ModelVerificationError.display |> String.concat ~sep:"\n"))
    (List.is_empty errors);
  models


type test_environment = {
  static_analysis_configuration: Configuration.StaticAnalysis.t;
  whole_program_call_graph: CallGraph.WholeProgramCallGraph.t;
  define_call_graphs: CallGraph.DefineCallGraphSharedMemory.t;
  override_graph_heap: OverrideGraph.Heap.t;
  override_graph_shared_memory: OverrideGraph.SharedMemory.t;
  initial_callables: FetchCallables.t;
  stubs: Target.t list;
  initial_models: Registry.t;
  environment: TypeEnvironment.ReadOnly.t;
  class_interval_graph: ClassIntervalSetGraph.SharedMemory.t;
}

let set_up_decorator_inlining ~handle models =
  let decorators_to_skip =
    models
    >>| Analysis.InlineDecorator.decorators_to_skip ~path:(PyrePath.create_absolute handle)
    |> Option.value ~default:[]
  in
  List.iter decorators_to_skip ~f:(fun decorator ->
      Analysis.InlineDecorator.DecoratorsToSkip.add decorator decorator);
  Analysis.InlineDecorator.set_should_inline_decorators true


let initialize
    ?(handle = "test.py")
    ?models_source
    ?(add_initial_models = true)
    ?find_missing_flows
    ?(taint_configuration = TaintConfiguration.default)
    ?expected_dump_string
    ?(verify_model_queries = true)
    ~context
    source_content
  =
  let configuration, environment, errors =
    let project = Test.ScratchProject.setup ~context [handle, source_content] in
    set_up_decorator_inlining ~handle models_source;
    let { Test.ScratchProject.BuiltTypeEnvironment.type_environment; _ }, errors =
      Test.ScratchProject.build_type_environment_and_postprocess project
    in
    Test.ScratchProject.configuration_of project, type_environment, errors
  in
  let static_analysis_configuration =
    Configuration.StaticAnalysis.create configuration ?find_missing_flows ()
  in
  let ast_environment = TypeEnvironment.ReadOnly.ast_environment environment in
  let source =
    AstEnvironment.ReadOnly.get_processed_source
      ast_environment
      (Reference.create (String.chop_suffix_exn handle ~suffix:".py"))
    |> fun option -> Option.value_exn option
  in
  (if not (List.is_empty errors) then
     let errors =
       errors
       |> List.map ~f:(fun error ->
              let error =
                AnalysisError.instantiate
                  ~show_error_traces:false
                  ~lookup:(AstEnvironment.ReadOnly.get_real_path_relative ast_environment)
                  error
              in
              Format.asprintf
                "%a:%s"
                Location.WithPath.pp
                (AnalysisError.Instantiated.location error)
                (AnalysisError.Instantiated.description error))
       |> String.concat ~sep:"\n"
     in
     failwithf "Pyre errors were found in `%s`:\n%s" handle errors ());

  let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
  let resolution =
    TypeCheck.resolution
      global_resolution
      (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
      (module TypeCheck.DummyContext)
  in
  let initial_callables =
    Interprocedural.FetchCallables.from_source
      ~configuration
      ~resolution:global_resolution
      ~include_unit_tests:false
      ~source
  in
  let stubs = Interprocedural.FetchCallables.get_stubs initial_callables in
  let callables = Interprocedural.FetchCallables.get_callables initial_callables in
  let user_models, skip_overrides =
    let models_source =
      match models_source, add_initial_models with
      | Some source, true ->
          Some (Format.sprintf "%s\n%s" (Test.trim_extra_indentation source) initial_models_source)
      | None, true -> Some initial_models_source
      | models_source, _ -> models_source
    in
    match models_source with
    | None -> Registry.empty, Ast.Reference.Set.empty
    | Some source ->
        let { ModelParser.models; errors; skip_overrides; queries = rules } =
          ModelParser.parse
            ~resolution
            ~source:(Test.trim_extra_indentation source)
            ~configuration:taint_configuration
            ~callables:(Some (Target.HashSet.of_list callables))
            ~stubs:(Target.HashSet.of_list stubs)
            ()
        in
        assert_bool
          (Format.sprintf
             "The models shouldn't have any parsing errors:\n%s\nModels:\n%s"
             (List.map errors ~f:ModelVerificationError.display |> String.concat ~sep:"\n")
             source)
          (List.is_empty errors);

        let models_result =
          TaintModelQuery.ModelQuery.apply_all_rules
            ~resolution
            ~configuration:taint_configuration
            ~scheduler:(Test.mock_scheduler ())
            ~rule_filter:None
            ~rules
            ~callables:(List.rev_append stubs callables)
            ~stubs:(Target.HashSet.of_list stubs)
            ~environment
        in
        let models_and_names, errors = fst models_result, snd models_result in
        (match taint_configuration.dump_model_query_results_path, expected_dump_string with
        | Some path, Some expected_string ->
            TaintModelQuery.ModelQuery.DumpModelQueryResults.dump_to_file_and_string
              ~models_and_names
              ~path
            |> assert_equal ~cmp:String.equal expected_string
        | Some path, None ->
            TaintModelQuery.ModelQuery.DumpModelQueryResults.dump_to_file ~models_and_names ~path
        | None, Some expected_string ->
            TaintModelQuery.ModelQuery.DumpModelQueryResults.dump_to_string ~models_and_names
            |> assert_equal ~cmp:String.equal expected_string
        | None, None -> ());
        let verify = static_analysis_configuration.verify_models && verify_model_queries in
        ModelVerificationError.verify_models_and_dsl errors verify;
        let models =
          models_and_names
          |> TaintModelQuery.ModelQuery.ModelQueryRegistryMap.get_registry
               ~model_join:Model.join_user_models
          |> Registry.merge ~join:Model.join_user_models models
        in
        let models =
          MissingFlow.add_obscure_models
            ~static_analysis_configuration
            ~environment
            ~stubs:(Target.HashSet.of_list stubs)
            ~initial_models:models
        in

        models, skip_overrides
  in
  let inferred_models = ClassModels.infer ~environment ~user_models in
  let initial_models = Registry.merge ~join:Model.join_user_models inferred_models user_models in
  (* Overrides must be done first, as they influence the call targets. *)
  let override_graph_heap =
    OverrideGraph.Heap.from_source ~environment ~include_unit_tests:true ~source
    |> OverrideGraph.Heap.skip_overrides ~to_skip:skip_overrides
  in
  let override_graph_shared_memory = OverrideGraph.SharedMemory.from_heap override_graph_heap in

  (* Initialize models *)
  let () = TaintConfiguration.register taint_configuration in
  (* The call graph building depends on initial models for global targets. *)
  let { Interprocedural.CallGraph.whole_program_call_graph; define_call_graphs } =
    Interprocedural.CallGraph.build_whole_program_call_graph
      ~scheduler:(Test.mock_scheduler ())
      ~static_analysis_configuration
      ~environment
      ~override_graph:override_graph_shared_memory
      ~store_shared_memory:true
      ~attribute_targets:(Registry.object_targets initial_models)
      ~callables
  in
  let initial_models =
    MissingFlow.add_unknown_callee_models
      ~static_analysis_configuration
      ~call_graph:whole_program_call_graph
      ~initial_models
  in
  let class_hierarchy_graph = ClassHierarchyGraph.from_source ~environment ~source in
  let class_interval_graph =
    Interprocedural.ClassIntervalSetGraph.Heap.from_class_hierarchy class_hierarchy_graph
    |> Interprocedural.ClassIntervalSetGraph.SharedMemory.from_heap
  in
  {
    static_analysis_configuration;
    whole_program_call_graph;
    define_call_graphs;
    override_graph_heap;
    override_graph_shared_memory;
    initial_callables;
    stubs;
    initial_models;
    environment;
    class_interval_graph;
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
      | Unix.Unix_error _ ->
          failwith (Format.asprintf "Could not write `%s` file for %a" suffix PyrePath.pp path)
    in
    let remove_old_output ~suffix =
      try output_filename ~suffix ~initial:false |> PyrePath.show |> Sys.remove with
      | Sys_error _ ->
          (* be silent *)
          ()
    in
    let get_expected ~suffix =
      try PyrePath.with_suffix path ~suffix |> File.create |> File.content with
      | Unix.Unix_error _ -> None
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
      | Unix.Unix_error _ -> None
    in
    let taint_configuration =
      try
        let path = PyrePath.with_suffix path ~suffix:".config" in
        File.create path
        |> File.content
        |> Option.map ~f:(fun content ->
               Taint.TaintConfiguration.parse [path, Yojson.Safe.from_string content]
               |> Taint.TaintConfiguration.exception_on_error)
      with
      | Unix.Unix_error _ -> None
    in
    let add_initial_models = Option.is_none models_source && Option.is_none taint_configuration in
    let taint_configuration =
      taint_configuration |> Option.value ~default:Taint.TaintConfiguration.default
    in
    let handle = PyrePath.show path |> String.split ~on:'/' |> List.last_exn in
    let create_call_graph_files call_graph =
      let actual =
        Format.asprintf
          "@%s\nCall dependencies\n%a"
          "generated"
          TargetGraph.pp
          (CallGraph.WholeProgramCallGraph.to_target_graph call_graph)
      in
      create_expected_and_actual_files ~suffix:".cg" actual
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
      whole_program_call_graph;
      define_call_graphs;
      environment;
      override_graph_heap;
      override_graph_shared_memory;
      initial_models;
      initial_callables;
      stubs;
      class_interval_graph;
      _;
    }
      =
      initialize
        ~handle
        ?models_source
        ~add_initial_models
        ~taint_configuration
        ~verify_model_queries:false
        ~context
        source
    in
    let { DependencyGraph.dependency_graph; callables_to_analyze; override_targets; _ } =
      DependencyGraph.build_whole_program_dependency_graph
        ~prune:true
        ~initial_callables
        ~call_graph:whole_program_call_graph
        ~overrides:override_graph_heap
    in
    let fixpoint_state =
      Fixpoint.compute
        ~scheduler:(Test.mock_scheduler ())
        ~type_environment:environment
        ~override_graph:override_graph_shared_memory
        ~dependency_graph
        ~context:
          {
            Fixpoint.Context.type_environment = environment;
            class_interval_graph;
            define_call_graphs;
          }
        ~initial_callables:(FetchCallables.get_callables initial_callables)
        ~stubs
        ~override_targets
        ~callables_to_analyze
        ~initial_models
        ~max_iterations:100
        ~epoch:Fixpoint.Epoch.initial
    in
    let serialize_model callable : string =
      let externalization =
        let filename_lookup =
          Analysis.TypeEnvironment.ReadOnly.ast_environment environment
          |> Analysis.AstEnvironment.ReadOnly.get_relative
        in
        Taint.Reporting.fetch_and_externalize
          ~fixpoint_state
          ~filename_lookup
          ~override_graph:override_graph_shared_memory
          callable
        |> List.map ~f:(fun json -> Yojson.Safe.pretty_to_string ~std:true json ^ "\n")
        |> String.concat ~sep:""
      in
      externalization
    in

    let divergent_files =
      [create_call_graph_files whole_program_call_graph; create_overrides_files override_graph_heap]
    in
    let serialized_models =
      List.rev_append (Registry.targets initial_models) callables_to_analyze
      |> Target.Set.of_list
      |> Target.Set.elements
      |> List.map ~f:serialize_model
      |> List.sort ~compare:String.compare
      |> String.concat ~sep:""
    in
    let () = Fixpoint.cleanup fixpoint_state in
    let () = OverrideGraph.SharedMemory.cleanup override_graph_shared_memory override_graph_heap in
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


let end_to_end_test_paths relative_path =
  let file_filter name =
    String.is_suffix ~suffix:".py" name
    && (not (String.contains name '#'))
    && not (String.contains name '~')
  in
  PyrePath.current_working_directory ()
  |> (fun path ->
       PyrePath.search_upwards ~target:"source" ~target_type:PyrePath.FileType.Directory ~root:path
       |> Option.value ~default:path)
  |> (fun root -> PyrePath.create_relative ~root ~relative:relative_path)
  |> fun root -> PyrePath.list ~file_filter ~root ()


let end_to_end_test_paths_found relative_path _ =
  if List.is_empty (end_to_end_test_paths relative_path) then
    assert_bool "No test paths to check." false
