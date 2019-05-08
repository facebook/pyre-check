(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

module TypeCheck = Analysis.TypeCheck

open Core
open OUnit2

open Analysis

module AnalysisError = Analysis.Error

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
  kind: [`Function | `Method | `Override | `Object];
  define_name: string;
  source_parameters: parameter_source_taint list;
  sink_parameters: parameter_taint list;
  tito_parameters: string list;
  returns: Taint.Sources.t list;
  errors: error_expectation list;
  obscure: bool option;
}


let outcome
    ~kind
    ?(source_parameters = [])
    ?(sink_parameters = [])
    ?(tito_parameters = [])
    ?(returns = [])
    ?(errors = [])
    ?obscure
    define_name =
  {
    kind;
    define_name;
    source_parameters;
    sink_parameters;
    tito_parameters;
    returns;
    errors;
    obscure;
  }


let get_model callable =
  let error =
    Base.Error.of_exn (
      OUnitTest.OUnit_failure (Format.asprintf "model not found for %a" Callable.pp callable)
    )
  in
  let model =
    Fixpoint.get_model callable
    |> Option.value_exn ?here:None ~error ?message:None
  in
  model
  |> Result.get_model Taint.Result.kind
  |> Option.value ~default:Taint.Result.empty_model,
  model.is_obscure


let create_callable kind define_name =
  let name = Reference.create define_name in
  match kind with
  | `Method -> Callable.create_method name
  | `Function -> Callable.create_function name
  | `Override -> Callable.create_override name
  | `Object -> Callable.create_object name


let check_expectation
    ?(get_model = get_model)
    {
      define_name;
      source_parameters;
      sink_parameters;
      tito_parameters;
      returns;
      errors;
      kind;
      obscure;
    }
  =
  let callable = create_callable kind define_name in
  let open Taint.Result in
  let extract_sinks_by_parameter_name sink_map (root, sink_tree) =
    match AccessPath.Root.parameter_name root with
    | Some name ->
        let sinks =
          Domains.BackwardState.Tree.collapse sink_tree
          |> Domains.BackwardTaint.leaves
        in
        let sinks =
          String.Map.find sink_map name
          |> Option.value ~default:[]
          |> List.rev_append sinks
          |> List.dedup_and_sort ~compare:Taint.Sinks.compare
        in
        String.Map.set sink_map ~key:name ~data:sinks
    | _ ->
        sink_map
  in
  let extract_sources_by_parameter_name sink_map (root, source_tree) =
    match AccessPath.Root.parameter_name root with
    | Some name ->
        let sinks =
          Domains.ForwardState.Tree.collapse source_tree
          |> Domains.ForwardTaint.leaves
        in
        let sinks =
          String.Map.find sink_map name
          |> Option.value ~default:[]
          |> List.rev_append sinks
          |> List.dedup_and_sort ~compare:Taint.Sources.compare
        in
        String.Map.set sink_map ~key:name ~data:sinks
    | _ ->
        sink_map
  in
  let backward, forward, is_obscure =
    let { backward; forward; _ }, is_obscure = get_model callable in
    backward, forward, is_obscure
  in
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
  let extract_tito_parameter_name positions (root, taint) =
    let leafs =
      Domains.BackwardState.Tree.fold
        Domains.BackwardTaint.leaf
        taint
        ~f:(Fn.flip List.cons)
        ~init:[]
    in
    let get_tito_name parameter_name = function
      | Sinks.LocalReturn -> parameter_name
      | Sinks.ParameterUpdate n -> Format.sprintf "%s updates parameter %d" parameter_name n
      | _ -> failwith "not a tito sink"
    in
    match AccessPath.Root.parameter_name root with
    | Some name ->
        List.fold leafs
          ~init:positions
          ~f:(fun positions leaf -> String.Set.add positions (get_tito_name name leaf))
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
          ~cmp:(List.equal ~equal:Taint.Sinks.equal)
          ~printer:(fun list -> Sexp.to_string [%message (list: Taint.Sinks.t list)])
          ~msg:(Format.sprintf "Define %s Parameter %s" define_name name)
          expected
          actual
    | `Left expected ->
        assert_equal
          ~cmp:(List.equal ~equal:Taint.Sinks.equal)
          ~printer:(fun list -> Sexp.to_string [%message (list: Taint.Sinks.t list)])
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
          ~cmp:(List.equal ~equal:Taint.Sources.equal)
          ~printer:(fun list -> Sexp.to_string [%message (list: Taint.Sources.t list)])
          ~msg:(Format.sprintf "Define %s Parameter %s" define_name name)
          expected
          actual
    | `Left expected ->
        assert_equal
          ~cmp:(List.equal ~equal:Taint.Sources.equal)
          ~printer:(fun list -> Sexp.to_string [%message (list: Taint.Sources.t list)])
          ~msg:(Format.sprintf "Define %s Parameter %s" define_name name)
          expected
          []
    | `Right _ ->
        (* Okay, we may have outcomes we don't care about *)
        ()
  in
  let expected_sinks =
    List.map ~f:(fun { name; sinks; } -> name, sinks) sink_parameters
    |> String.Map.of_alist_exn
  in
  let expected_parameter_sources =
    List.map ~f:(fun { name; sources; } -> name, sources) source_parameters
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
  let expected_sources =
    List.map ~f:Sources.show returns
    |> String.Set.of_list
  in
  let assert_error { code; pattern } error =
    if code <> Error.code error then
      Format.sprintf "Expected error code %d for %s, but got %d"
        code
        define_name
        (Error.code error)
      |> assert_failure;
    let error_string = Error.description ~show_error_traces:true error in
    let regexp = Str.regexp pattern in
    if not (Str.string_match regexp error_string 0) then
      Format.sprintf "Expected error for %s to match %s, but got %s"
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
        Format.sprintf "Returned sources %s: %s"
          define_name
          (Sexp.to_string [%message (set: String.Set.t)]))
    expected_sources
    returned_sources;

  (* Check sinks. *)
  assert_equal
    (Map.length expected_sinks)
    (Map.length sink_taint_map)
    ~printer:Int.to_string
    ~msg:(
      Format.sprintf
        "Define %s: List of sink tainted parameters differ in length."
        define_name
    );
  String.Map.iter2 ~f:check_each_sink expected_sinks sink_taint_map;

  (* Check parameter sources. *)
  assert_equal
    (Map.length expected_parameter_sources)
    (Map.length parameter_source_taint_map)
    ~printer:Int.to_string
    ~msg:(
      Format.sprintf
        "Define %s: List of source tainted parameters differ in length."
        define_name
    );
  String.Map.iter2
    ~f:check_each_source_parameter
    expected_parameter_sources
    parameter_source_taint_map;

  let expected_tito = tito_parameters |> String.Set.of_list in
  assert_equal
    ~cmp:String.Set.equal
    ~printer:(fun set -> Sexp.to_string [%message (set: String.Set.t)])
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
  assert_errors errors actual_errors


let run_with_taint_models tests =
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

      def eval(arg: TaintSink[RemoteCodeExecution]): ...

      def copy(obj: TaintInTaintOut[Via[copy]]): ...
    |}
    |> Test.trim_extra_indentation
  in
  let environment =
    Test.environment
      ~sources:(Test.typeshed_stubs () @ [Test.parse model_source])
      ()
  in
  let () =
    Model.parse
      ~resolution:(TypeCheck.resolution environment ())
      ~source:model_source
      ~configuration:TaintConfiguration.default
      Callable.Map.empty
    |> Callable.Map.map ~f:(Interprocedural.Result.make_model Taint.Result.kind)
    |> Interprocedural.Analysis.record_initial_models ~functions:[] ~stubs:[]
  in
  Test.run tests


type test_environment = {
  callgraph: DependencyGraph.callgraph;
  overrides: DependencyGraph.t;
  all_callables: Callable.t list;
  environment: (module Environment.Handler);
}


let initialize ?(qualifier = "test.py") ?models source_content =
  let source_content = Test.trim_extra_indentation source_content in
  let handle = File.Handle.create qualifier in
  let source =
    Test.parse ~qualifier:(Source.qualifier ~handle) ~handle:qualifier source_content
    |> Preprocessing.preprocess
  in
  let path =
    let path = Test.mock_path qualifier in
    File.create ~content:source_content path
    |> File.write;
    path
  in
  let configuration = Configuration.Analysis.create ~strict:true () in

  (* Parse sources. *)
  Ast.SharedMemory.Sources.remove ~handles:[handle];
  Service.Parser.parse_sources
    ~configuration
    ~scheduler:(Scheduler.mock ())
    ~preprocessing_state:None
    ~files:[File.create ~content:source_content path]
  |> ignore;

  let environment =
    let models =
      models
      >>| (fun model -> [Test.parse ~qualifier:(Reference.create qualifier) model])
      |> Option.value ~default:[]
    in
    Test.environment ~sources:(Test.typeshed_stubs () @ models) ~configuration ()
  in
  Service.Environment.populate
    ~configuration:Test.mock_configuration
    ~scheduler:(Scheduler.mock ())
    environment
    [source];

  let errors =
    TypeCheck.run ~configuration ~environment ~source
    |> List.filter ~f:(fun error -> AnalysisError.code error = 11)  (* Undefined types. *)
  in
  if not (List.is_empty errors) then
    begin
      let errors =
        List.map errors ~f:(AnalysisError.description ~show_error_traces:false)
        |> String.concat ~sep:"\n"
      in
      failwithf
        "Unable to construct callgraph for %s because of undefined types:\n%s"
        (File.Handle.show source.Source.handle)
        errors
        ()
    end;
  (* Overrides must be done first, as they influence the call targets. *)
  let overrides =
    let overrides = DependencyGraph.create_overrides ~environment ~source in
    Service.StaticAnalysis.record_overrides overrides;
    DependencyGraph.from_overrides overrides
  in
  let callgraph =
    Service.StaticAnalysis.record_and_merge_call_graph
      ~environment
      ~call_graph:DependencyGraph.empty_callgraph
      ~path:handle
      ~source
  in
  let callables, stubs =
    Service.StaticAnalysis.callables ~resolution:(TypeCheck.resolution environment ()) ~source
    |> List.map ~f:(fun (callable, define) -> (callable :> Callable.t), define.Node.value)
    |> List.partition_tf ~f:(fun (_callable, define) -> not (Statement.Define.is_stub define))
  in
  let callables =
    List.map ~f:fst callables
    |> List.rev_append (Callable.Map.keys overrides)
  in
  let stubs =
    List.map ~f:fst stubs
  in
  let all_callables = List.rev_append stubs callables in
  (* Initialize models *)
  let () =
    let keys = Fixpoint.KeySet.of_list all_callables in
    Fixpoint.remove_new keys;
    Fixpoint.remove_old keys;
    let initial_models =
      match models with
      | None -> Callable.Map.empty
      | Some source ->
          Model.parse
            ~resolution:(TypeCheck.resolution environment ())
            ~source:(Test.trim_extra_indentation source)
            ~configuration:TaintConfiguration.default
            Callable.Map.empty
    in
    initial_models
    |> Callable.Map.map ~f:(Interprocedural.Result.make_model Taint.Result.kind)
    |> Interprocedural.Analysis.record_initial_models ~functions:callables ~stubs:stubs
  in
  { callgraph; overrides; all_callables; environment }
