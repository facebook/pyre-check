(* Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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
  analysis_mode: Taint.Result.mode;
}

let outcome
    ~kind
    ?(source_parameters = [])
    ?(sink_parameters = [])
    ?(tito_parameters = [])
    ?(returns = [])
    ?(errors = [])
    ?obscure
    ?(analysis_mode = Taint.Result.Normal)
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
    analysis_mode;
  }


let get_model callable =
  let error =
    Base.Error.of_exn
      (OUnitTest.OUnit_failure (Format.asprintf "model not found for %a" Callable.pp callable))
  in
  let model = Fixpoint.get_model callable |> Option.value_exn ?here:None ~error ?message:None in
  ( model |> Result.get_model Taint.Result.kind |> Option.value ~default:Taint.Result.empty_model,
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
      analysis_mode = expected_analysis_mode;
    }
  =
  let callable = create_callable kind define_name in
  let open Taint.Result in
  let extract_sinks_by_parameter_name (root, sink_tree) sink_map =
    match AccessPath.Root.parameter_name root with
    | Some name ->
        let sinks = Domains.BackwardState.Tree.collapse sink_tree |> Domains.BackwardTaint.leaves in
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
        let sinks = Domains.ForwardState.Tree.collapse source_tree |> Domains.ForwardTaint.leaves in
        let sinks =
          String.Map.find sink_map name
          |> Option.value ~default:[]
          |> List.rev_append sinks
          |> List.dedup_and_sort ~compare:Taint.Sources.compare
        in
        String.Map.set sink_map ~key:name ~data:sinks
    | _ -> sink_map
  in
  let { backward; forward; mode }, is_obscure = get_model callable in
  assert_equal mode expected_analysis_mode;
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
    |> Domains.ForwardState.Tree.collapse
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
    Fixpoint.get_result callable
    |> Result.get_result Taint.Result.kind
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
      def __test_sink(arg: TaintSink[Test, Via[special_sink]]): ...
      def __test_source() -> TaintSource[Test, Via[special_source]]: ...
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
        Callable.Map.empty
    in
    assert_bool
      (Format.sprintf
         "The models shouldn't have any parsing errors: %s."
         (List.to_string errors ~f:ident))
      (List.is_empty errors);
    Callable.Map.map models ~f:(Interprocedural.Result.make_model Taint.Result.kind)
    |> Interprocedural.Analysis.record_initial_models ~functions:[] ~stubs:[]
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
  all_callables: Callable.t list;
  environment: TypeEnvironment.ReadOnly.t;
}

let initialize
    ?(handle = "test.py")
    ?models
    ?(taint_configuration = TaintConfiguration.default)
    ~context
    source_content
  =
  let configuration, ast_environment, environment, errors =
    let project = Test.ScratchProject.setup ~context [handle, source_content] in
    let { Test.ScratchProject.BuiltTypeEnvironment.type_environment; _ }, errors =
      Test.ScratchProject.build_type_environment_and_postprocess
        ~call_graph_builder:(module Taint.CallGraphBuilder)
        project
    in
    ( Test.ScratchProject.configuration_of project,
      TypeEnvironment.ast_environment type_environment |> AstEnvironment.read_only,
      type_environment,
      errors )
  in
  let source =
    AstEnvironment.ReadOnly.get_processed_source
      ast_environment
      (Reference.create (String.chop_suffix_exn handle ~suffix:".py"))
    |> fun option -> Option.value_exn option
  in
  let errors =
    let keep { AnalysisError.kind; _ } =
      match kind with
      (* TODO(T47874282): Don't filter these. *)
      | AnalysisError.NotCallable _ -> false
      | _ -> true
    in
    List.filter errors ~f:keep
  in
  ( if not (List.is_empty errors) then
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
      failwithf "Pyre errors were found in `%s`:\n%s" handle errors () );

  let environment = TypeEnvironment.read_only environment in
  let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
  let initial_models, skip_overrides =
    let inferred_models = Model.infer_class_models ~environment in
    match models with
    | None -> inferred_models, Reference.Set.empty
    | Some source ->
        let { Taint.Model.models; errors; skip_overrides } =
          Model.parse
            ~resolution:
              (TypeCheck.resolution
                 global_resolution
                 (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
                 (module TypeCheck.DummyContext))
            ~source:(Test.trim_extra_indentation source)
            ~configuration:taint_configuration
            inferred_models
        in
        assert_bool
          (Format.sprintf
             "The models shouldn't have any parsing errors: %s."
             (List.to_string errors ~f:ident))
          (List.is_empty errors);
        models, skip_overrides
  in
  (* Overrides must be done first, as they influence the call targets. *)
  let overrides =
    let overrides =
      DependencyGraph.create_overrides ~environment ~source
      |> Reference.Map.filter_keys ~f:(fun override -> not (Set.mem skip_overrides override))
    in
    Service.StaticAnalysis.record_overrides overrides;
    DependencyGraph.from_overrides overrides
  in
  let callgraph =
    Service.StaticAnalysis.record_and_merge_call_graph
      ~environment
      ~call_graph:DependencyGraph.empty_callgraph
      ~source
  in
  let callables, stubs =
    Service.StaticAnalysis.regular_and_filtered_callables ~resolution:global_resolution ~source
    |> fst
    |> List.map ~f:(fun (callable, define) -> (callable :> Callable.t), define.Node.value)
    |> List.partition_tf ~f:(fun (_callable, define) -> not (Statement.Define.is_stub define))
  in
  let callables = List.map ~f:fst callables |> List.rev_append (Callable.Map.keys overrides) in
  let stubs = List.map ~f:fst stubs in
  let all_callables = List.rev_append stubs callables in
  (* Initialize models *)
  let () = TaintConfiguration.register taint_configuration in
  let () =
    let keys = Fixpoint.KeySet.of_list all_callables in
    Fixpoint.remove_new keys;
    Fixpoint.remove_old keys;
    initial_models
    |> Callable.Map.map ~f:(Interprocedural.Result.make_model Taint.Result.kind)
    |> Interprocedural.Analysis.record_initial_models ~functions:callables ~stubs
  in
  { callgraph; overrides; all_callables; environment }
