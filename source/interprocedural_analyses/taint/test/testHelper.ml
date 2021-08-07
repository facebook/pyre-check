(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
  sinks: Taint.Sinks.t list;
}

type parameter_source_taint = {
  name: string;
  sources: Taint.Sources.t list;
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
  tito_parameters: string list;
  returns: Taint.Sources.t list;
  errors: error_expectation list;
  obscure: bool option;
  sanitize: Taint.Result.Sanitize.t;
  analysis_modes: Taint.Result.ModeSet.t;
}

let outcome
    ~kind
    ?(source_parameters = [])
    ?(sink_parameters = [])
    ?(tito_parameters = [])
    ?(returns = [])
    ?(errors = [])
    ?obscure
    ?(sanitize = Taint.Result.Sanitize.empty)
    ?(analysis_modes = Taint.Result.ModeSet.empty)
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
    obscure;
    sanitize;
    analysis_modes;
  }


let get_model callable =
  let error =
    Base.Error.of_exn
      (OUnitTest.OUnit_failure (Format.asprintf "model not found for %a" Callable.pp callable))
  in
  let model =
    FixpointState.get_model callable |> Option.value_exn ?here:None ~error ?message:None
  in
  ( model
    |> AnalysisResult.get_model Taint.Result.kind
    |> Option.value ~default:Taint.Result.empty_model,
    model.is_obscure )


let create_callable kind define_name =
  let name = Reference.create define_name in
  match kind with
  | `Method -> Callable.create_method name
  | `Function -> Callable.create_function name
  | `PropertySetter -> Callable.create_property_setter name
  | `Override -> Callable.create_override name
  | `Object -> Callable.create_object name


let check_expectation
    ?(get_model = get_model)
    ~environment
    {
      define_name;
      source_parameters;
      sink_parameters;
      tito_parameters;
      returns;
      errors;
      kind;
      obscure;
      sanitize = expected_sanitize;
      analysis_modes = expected_analysis_modes;
    }
  =
  let callable = create_callable kind define_name in
  let open Taint.Result in
  let extract_sinks_by_parameter_name (root, sink_tree) sink_map =
    match AccessPath.Root.parameter_name root with
    | Some name ->
        let sinks =
          Domains.BackwardState.Tree.collapse ~transform:Fn.id sink_tree
          |> Domains.BackwardTaint.leaves
        in
        let sinks =
          String.Map.find sink_map name
          |> Option.value ~default:[]
          |> List.rev_append sinks
          |> List.dedup_and_sort ~compare:Taint.Sinks.compare
        in
        String.Map.set sink_map ~key:name ~data:sinks
    | _ -> sink_map
  in
  let extract_sources_by_parameter_name (root, source_tree) sink_map =
    match AccessPath.Root.parameter_name root with
    | Some name ->
        let sinks =
          Domains.ForwardState.Tree.collapse ~transform:Fn.id source_tree
          |> Domains.ForwardTaint.leaves
        in
        let sinks =
          String.Map.find sink_map name
          |> Option.value ~default:[]
          |> List.rev_append sinks
          |> List.dedup_and_sort ~compare:Taint.Sources.compare
        in
        String.Map.set sink_map ~key:name ~data:sinks
    | _ -> sink_map
  in
  let { backward; forward; sanitize; modes }, is_obscure = get_model callable in
  assert_equal ~printer:Taint.Result.ModeSet.show modes expected_analysis_modes;
  assert_equal ~printer:Taint.Result.Sanitize.show sanitize expected_sanitize;
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
  let extract_tito_parameter_name (root, taint) positions =
    let leafs =
      Domains.BackwardState.Tree.fold Domains.BackwardTaint.leaf taint ~f:List.cons ~init:[]
    in
    let get_tito_name parameter_name = function
      | Sinks.Attach -> parameter_name
      | Sinks.LocalReturn -> parameter_name
      | Sinks.ParameterUpdate n -> Format.sprintf "%s updates parameter %d" parameter_name n
      | _ -> failwith "not a tito sink"
    in
    match AccessPath.Root.parameter_name root with
    | Some name ->
        List.fold leafs ~init:positions ~f:(fun positions leaf ->
            String.Set.add positions (get_tito_name name leaf))
    | _ -> positions
  in
  let taint_in_taint_out_names =
    Domains.BackwardState.fold
      Domains.BackwardState.KeyValue
      backward.taint_in_taint_out
      ~f:extract_tito_parameter_name
      ~init:String.Set.empty
  in
  let check_each_sink ~key:name ~data =
    match data with
    | `Both (expected, actual) ->
        assert_equal
          ~cmp:(List.equal Taint.Sinks.equal)
          ~printer:(fun list -> Sexp.to_string [%message (list : Taint.Sinks.t list)])
          ~msg:(Format.sprintf "Define %s Parameter %s" define_name name)
          expected
          actual
    | `Left expected ->
        assert_equal
          ~cmp:(List.equal Taint.Sinks.equal)
          ~printer:(fun list -> Sexp.to_string [%message (list : Taint.Sinks.t list)])
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
          ~cmp:(List.equal Taint.Sources.equal)
          ~printer:(fun list -> Sexp.to_string [%message (list : Taint.Sources.t list)])
          ~msg:(Format.sprintf "Define %s Parameter %s" define_name name)
          expected
          actual
    | `Left expected ->
        assert_equal
          ~cmp:(List.equal Taint.Sources.equal)
          ~printer:(fun list -> Sexp.to_string [%message (list : Taint.Sources.t list)])
          ~msg:(Format.sprintf "Define %s Parameter %s" define_name name)
          expected
          []
    | `Right _ ->
        (* Okay, we may have outcomes we don't care about *)
        ()
  in
  let expected_sinks =
    List.map ~f:(fun { name; sinks } -> name, sinks) sink_parameters |> String.Map.of_alist_exn
  in
  let expected_parameter_sources =
    List.map ~f:(fun { name; sources } -> name, sources) source_parameters
    |> String.Map.of_alist_exn
  in
  (* Check obscure *)
  let () =
    match obscure with
    | None -> ()
    | Some obscure ->
        assert_equal
          obscure
          is_obscure
          ~msg:(Format.sprintf "Obscure for %s" define_name)
          ~printer:Bool.to_string
  in
  (* Check sources. *)
  let returned_sources =
    Domains.ForwardState.read ~root:AccessPath.Root.LocalResult ~path:[] forward.source_taint
    |> Domains.ForwardState.Tree.collapse ~transform:Fn.id
    |> Domains.ForwardTaint.leaves
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
  let expected_tito = tito_parameters |> String.Set.of_list in
  assert_equal
    ~cmp:String.Set.equal
    ~printer:(fun set -> Sexp.to_string [%message (set : String.Set.t)])
    ~msg:(Format.sprintf "Define %s Tito positions" define_name)
    expected_tito
    taint_in_taint_out_names;

  (* Check errors *)
  let actual_errors =
    FixpointState.get_result callable
    |> AnalysisResult.get_result Taint.Result.kind
    >>| List.map ~f:Flow.generate_error
    |> Option.value ~default:[]
  in
  let actual_errors =
    let ast_environment = TypeEnvironment.ReadOnly.ast_environment environment in
    List.map
      actual_errors
      ~f:
        (Error.instantiate
           ~show_error_traces:true
           ~lookup:(AstEnvironment.ReadOnly.get_relative ast_environment))
  in
  assert_errors errors actual_errors


let run_with_taint_models tests ~name =
  let set_up_taint_models ~context =
    let model_source =
      {|
      def _test_sink(arg: TaintSink[Test, Via[special_sink]]): ...
      def _test_source() -> TaintSource[Test, Via[special_source]]: ...
      def __tito( *x: TaintInTaintOut, **kw: TaintInTaintOut): ...
      def eval(arg: TaintSink[RemoteCodeExecution]): ...
      def __user_controlled() -> TaintSource[UserControlled]: ...
      def getattr(
          o: TaintInTaintOut[Via[object]],
          name: TaintSink[GetAttr],
          default: TaintInTaintOut[Via[default]] = ...,
      ): ...

      taint.__global_sink: TaintSink[Test] = ...
      ClassWithSinkAttribute.attribute: TaintSink[Test] = ...

      def copy(obj: TaintInTaintOut[Via[copy]]): ...
    |}
      |> Test.trim_extra_indentation
    in
    let global_resolution =
      Test.ScratchProject.setup ~context [] |> Test.ScratchProject.build_global_resolution
    in
    let { Taint.Model.models; errors; _ } =
      Model.parse
        ~resolution:
          (TypeCheck.resolution
             global_resolution
             (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
             (module TypeCheck.DummyContext))
        ~source:model_source
        ~configuration:TaintConfiguration.default
        ~functions:None
        ~stubs:(Interprocedural.Callable.HashSet.create ())
        Callable.Map.empty
    in
    assert_bool
      (Format.sprintf
         "The models shouldn't have any parsing errors: %s."
         (List.to_string errors ~f:Taint.Model.display_verification_error))
      (List.is_empty errors);
    Callable.Map.map models ~f:(AnalysisResult.make_model Taint.Result.kind)
    |> Interprocedural.FixpointAnalysis.record_initial_models ~functions:[] ~stubs:[]
  in
  let decorated_tests =
    List.map tests ~f:(fun (name, test) ->
        name
        >:: fun context ->
        let _ = set_up_taint_models ~context in
        test context)
  in
  Test.run (name >::: decorated_tests)


type test_environment = {
  callgraph: DependencyGraph.callgraph;
  overrides: DependencyGraph.t;
  callables_to_analyze: Callable.t list;
  initial_models_callables: Callable.t list;
  environment: TypeEnvironment.ReadOnly.t;
}

let type_environment_with_decorators_inlined ~configuration ~taint_configuration ~models environment
  =
  let read_only_environment = TypeEnvironment.read_only environment in
  let inferred_models = Model.infer_class_models ~environment:read_only_environment in
  let models =
    match models with
    | None -> inferred_models
    | Some source ->
        let resolution =
          TypeCheck.resolution
            (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
            (TypeEnvironment.ReadOnly.global_resolution read_only_environment)
            (module TypeCheck.DummyContext)
        in
        let { Taint.Model.models; _ } =
          Model.parse
            ~resolution
            ~source:(Test.trim_extra_indentation source)
            ~configuration:taint_configuration
            ~functions:None
            ~stubs:(Interprocedural.Callable.HashSet.create ())
            inferred_models
        in
        models
  in
  let decorators_to_skip =
    Callable.Map.map ~f:(AnalysisResult.make_model Taint.Result.kind) models
    |> Taint.Result.decorators_to_skip
  in
  DecoratorHelper.type_environment_with_decorators_inlined
    ~configuration
    ~scheduler:(Test.mock_scheduler ())
    ~recheck:Server.IncrementalCheck.recheck
    ~decorators_to_skip
    environment
  |> TypeEnvironment.read_only


let initialize
    ?(handle = "test.py")
    ?models
    ?(taint_configuration = TaintConfiguration.default)
    ~context
    source_content
  =
  let configuration, environment, errors =
    let project = Test.ScratchProject.setup ~context [handle, source_content] in
    let { Test.ScratchProject.BuiltTypeEnvironment.type_environment; _ }, errors =
      Test.ScratchProject.build_type_environment_and_postprocess
        ~call_graph_builder:(module Callgraph.NullBuilder)
        project
    in
    Test.ScratchProject.configuration_of project, type_environment, errors
  in
  let environment =
    type_environment_with_decorators_inlined ~configuration ~taint_configuration ~models environment
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
              AnalysisError.instantiate
                ~show_error_traces:false
                ~lookup:
                  (AstEnvironment.ReadOnly.get_real_path_relative ~configuration ast_environment)
                error
              |> AnalysisError.Instantiated.description)
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
  let callables, stubs =
    Service.StaticAnalysis.regular_and_filtered_callables
      ~configuration
      ~resolution:global_resolution
      ~source
    |> fst
    |> List.map ~f:(fun { Service.StaticAnalysis.callable; define; _ } ->
           (callable :> Callable.t), define.Node.value)
    |> List.partition_tf ~f:(fun (_callable, define) -> not (Statement.Define.is_stub define))
  in
  let callables = List.map ~f:fst callables in
  let stubs = List.map ~f:fst stubs in
  let initial_models, skip_overrides =
    let inferred_models = Model.infer_class_models ~environment in
    match models with
    | None -> inferred_models, Ast.Reference.Set.empty
    | Some source ->
        let { Taint.Model.models; errors; skip_overrides; queries = rules } =
          Model.parse
            ~resolution
            ~source:(Test.trim_extra_indentation source)
            ~configuration:taint_configuration
            ~functions:(Some (Callable.HashSet.of_list callables))
            ~stubs:(Callable.HashSet.of_list stubs)
            inferred_models
        in
        assert_bool
          (Format.sprintf
             "The models shouldn't have any parsing errors: %s."
             (List.to_string errors ~f:Taint.Model.display_verification_error))
          (List.is_empty errors);

        let models =
          TaintModelQuery.ModelQuery.apply_all_rules
            ~resolution
            ~configuration:taint_configuration
            ~scheduler:(Test.mock_scheduler ())
            ~rule_filter:None
            ~rules
            ~models
            ~callables:
              (List.filter_map (List.rev_append stubs callables) ~f:(function
                  | `Function _ as callable -> Some (callable :> Callable.real_target)
                  | `Method _ as callable -> Some (callable :> Callable.real_target)
                  | _ -> None))
            ~stubs:(Callable.HashSet.of_list stubs)
            ~environment
        in
        let remove_sinks models = Callable.Map.map ~f:Model.remove_sinks models in
        let add_obscure_sinks models =
          let add_obscure_sink models callable =
            let model =
              Callable.Map.find models callable
              |> Option.value ~default:Taint.Result.empty_model
              |> Model.add_obscure_sink ~resolution ~call_target:callable
              |> Model.remove_obscureness
            in
            Callable.Map.set models ~key:callable ~data:model
          in
          stubs
          |> List.filter ~f:(fun callable ->
                 Callable.Map.find models callable
                 >>| Model.is_obscure
                 |> Option.value ~default:true)
          |> List.fold ~init:models ~f:add_obscure_sink
        in
        let models =
          match taint_configuration.find_missing_flows with
          | Some Obscure -> models |> remove_sinks |> add_obscure_sinks
          | Some Type -> models |> remove_sinks
          | None -> models
        in
        models, skip_overrides
  in
  (* Overrides must be done first, as they influence the call targets. *)
  let overrides =
    let overrides =
      DependencyGraph.create_overrides ~environment ~source
      |> Reference.Map.filter_keys ~f:(fun override -> not (Set.mem skip_overrides override))
    in
    let _ = DependencyGraphSharedMemory.record_overrides overrides in
    DependencyGraph.from_overrides overrides
  in
  let callgraph =
    Service.StaticAnalysis.record_and_merge_call_graph
      ~environment
      ~call_graph:DependencyGraph.empty_callgraph
      ~source
  in

  let callables = List.rev_append (Callable.Map.keys overrides) callables in
  let callables_to_analyze = List.rev_append stubs callables in
  let initial_models_callables = Callable.Map.keys initial_models in
  (* Initialize models *)
  let () = TaintConfiguration.register taint_configuration in
  let () =
    let keys = FixpointState.KeySet.of_list callables_to_analyze in
    FixpointState.remove_new keys;
    FixpointState.remove_old keys;
    initial_models
    |> Callable.Map.map ~f:(AnalysisResult.make_model Taint.Result.kind)
    |> Interprocedural.FixpointAnalysis.record_initial_models ~functions:callables ~stubs
  in
  { callgraph; overrides; callables_to_analyze; initial_models_callables; environment }
