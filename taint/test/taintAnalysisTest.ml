(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis

module AnalysisError = Analysis.Error

open Ast
open Expression
open Pyre
open Taint

open Interprocedural
open TestHelper


type expect_fixpoint = {
  expect: expectation list;
  iterations: int;
}


type test_environment = {
  callgraph: DependencyGraph.callgraph;
  overrides: DependencyGraph.t;
  all_callables: Callable.t list;
  environment: (module Environment.Handler);
}

let create_call_graph ?(path = "test.py") source =
  let handle = File.Handle.create path in
  let source =
    Test.parse ~qualifier:(Access.create "qualifier") ~handle:path source
    |> Preprocessing.preprocess
  in
  let () = Ast.SharedMemory.Sources.remove ~handles:[handle] in
  let () = Ast.SharedMemory.Sources.add handle source in
  let environment = Test.environment () in
  Service.Environment.populate ~configuration:Test.mock_configuration environment [source];

  let configuration = Configuration.Analysis.create ~strict:true () in
  let errors =
    let { TypeCheck.Result.errors; _ } = TypeCheck.check ~configuration ~environment ~source in
    List.filter errors ~f:(fun error -> AnalysisError.code error = 11)  (* Undefined types. *)
  in
  if not (List.is_empty errors) then
    begin
      let errors =
        List.map errors ~f:(AnalysisError.description ~detailed:false)
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
  let all_callables =
    Service.StaticAnalysis.record_path_of_definitions ~path:handle ~source
    |> List.map ~f:(fun (callable, _define) -> (callable :> Callable.t))
    |> List.rev_append (Callable.Map.keys overrides)
  in
  let () =
    let add_initial_model callable =
      Fixpoint.add_predefined Fixpoint.Epoch.initial callable Result.empty_model
    in
    let keys = Fixpoint.KeySet.of_list all_callables in
    Fixpoint.remove_new keys;
    Fixpoint.remove_old keys;
    List.iter all_callables ~f:add_initial_model
  in
  { callgraph; overrides; all_callables; environment }


let assert_fixpoint ~source ~expect:{ iterations = expect_iterations; expect } =
  let scheduler = Scheduler.mock () in
  let { all_callables; callgraph; environment; overrides } = create_call_graph source in
  let dependencies =
    DependencyGraph.from_callgraph callgraph
    |> DependencyGraph.union overrides
    |> DependencyGraph.reverse
  in
  let analyses = [Taint.Analysis.abstract_kind] in
  let configuration = Configuration.Analysis.create () in
  let iterations =
    Analysis.compute_fixpoint
      ~configuration
      ~scheduler
      ~environment
      ~analyses
      ~dependencies
      ~all_callables
      Fixpoint.Epoch.initial
  in
  let read_analysis_result { define_name; kind; _ } =
    let call_target = TestHelper.create_callable kind define_name in
    Fixpoint.get_result call_target
    |> Result.get_result Taint.Result.kind
    >>| List.map ~f:Flow.generate_error
    >>| (fun result -> define_name, result)
  in
  let assert_error define { code; pattern } error =
    if code <> Error.code error then
      Format.sprintf "Expected error code %d for %s, but got %d"
        code
        define
        (Error.code error)
      |> assert_failure;
    let error_string = Error.description ~detailed:true error in
    let regexp = Str.regexp pattern in
    if not (Str.string_match regexp error_string 0) then
      Format.sprintf "Expected error for %s to match %s, but got %s"
        define
        pattern
        error_string
      |> assert_failure
  in
  let assert_errors (define1, error_patterns) (define2, errors) =
    if define1 <> define2 then
      Format.sprintf "Expected errors for %s, but found %s"
        define1
        define2
      |> assert_failure;
    assert_equal
      (List.length error_patterns)
      (List.length errors)
      ~msg:(Format.sprintf "Number of errors for %s" define1)
      ~printer:Int.to_string;
    List.iter2_exn ~f:(assert_error define1) error_patterns errors
  in
  let results = List.filter_map expect ~f:read_analysis_result in
  let expect_results =
    let create_result_patterns { define_name; errors; _ } = define_name, errors in
    List.map expect ~f:create_result_patterns
  in
  assert_bool "Callgraph is empty!" (Callable.RealMap.length callgraph > 0);
  assert_equal expect_iterations iterations ~printer:Int.to_string;
  List.iter ~f:check_expectation expect;
  List.iter2_exn expect_results results ~f:assert_errors


let test_fixpoint _ =
  assert_fixpoint
    ~source:
      {|
      def bar():
        return __testSource()

      def qux(arg):
        __testSink(arg)

      def bad(arg):
        qux(arg)

      def some_source():
        return bar()

      def match_flows():
        x = some_source()
        bad(x)

      def match_flows_multiple(arg):
        if arg:
          x = some_source()
        else:
          x = some_source()
        bad(x)

      def rce_problem():
        x = __userControlled()
        eval(x)

      class TestMethods:
        def method_source(self):
          return some_source()

        def method_sink(self, tainted):
          bad(tainted)

        def receiver_sink(self, not_tainted):
          bad(self.taint)

      def match_via_methods():
        x = TestMethods()
        taint = x.method_source()
        x.method_sink(taint)

      def no_match_via_methods():
        x = TestMethods()
        taint = x.method_source()
        x.taint = taint
        x.method_sink(5)

      def match_via_receiver():
        x = TestMethods()
        taint = x.method_source()
        x.taint = taint
        x.receiver_sink(5)

      def list_sink(list):
        __testSink(list[1])

      def list_match():
        x = [5, __testSource()]
        list_sink(x)

      def no_list_match():
        x = [__testSource(), 5]
        list_sink(x)

      def test_getattr_obj_no_match():
        obj = __userControlled()
        getattr(obj, 'foo')

      def test_getattr_field_match(some_obj):
        field = __userControlled()
        return getattr(some_obj, field)

      def deep_tito(tito, no_tito):
          x = { 'f': tito }
          y = { 'g': x }
          return y

      def test_deep_tito_no_match():
        obj = deep_tito(__userControlled(), __testSource())
        getattr('obj', obj.f.g)

      def test_deep_tito_match():
        obj = deep_tito(__userControlled(), __testSource())
        getattr('obj', obj.g.f)
      |}
    ~expect:{
      iterations = 4;
      expect = [
        {
          kind = `Function;
          define_name = "qualifier.rce_problem";
          returns = [];
          sink_parameters = [];
          tito_parameters = [];
          errors = [
            {
              code = 5001;
              pattern = ".*Possible shell injection.*Data from \\[UserControlled\\].*\\[RemoteCodeExecution\\].*";
            };
          ]
        };
        {
          kind = `Function;
          define_name = "qualifier.match_flows";
          returns = [];
          sink_parameters = [];
          tito_parameters = [];
          errors = [
            {
              code = 5002;
              pattern = ".*Test flow.*Data from \\[Test\\] source(s).* \\[Test\\] sink(s).*";
            }
          ];
        };
        {
          kind = `Function;
          define_name = "qualifier.match_flows_multiple";
          returns = [];
          sink_parameters = [];
          tito_parameters = [];
          errors = [
            {
              code = 5002;
              pattern = ".*Test flow.*Data from \\[Test\\] source(s).* \\[Test\\] sink(s).*";
            }
          ];
        };
        {
          kind = `Function;
          define_name = "qualifier.match_via_methods";
          returns = [];
          sink_parameters = [];
          tito_parameters = [];
          errors = [
            {
              code = 5002;
              pattern = ".*Test flow.*Data from \\[Test\\] source(s).* \\[Test\\] sink(s).*";
            };
          ]
        };
        {
          kind = `Function;
          define_name = "qualifier.no_match_via_methods";
          returns = [];
          sink_parameters = [];
          tito_parameters = [];
          errors = [];
        };
        {
          kind = `Function;
          define_name = "qualifier.match_via_receiver";
          returns = [];
          sink_parameters = [];
          tito_parameters = [];
          errors = [
            {
              code = 5002;
              pattern = ".*Test flow.*Data from \\[Test\\] source(s).* \\[Test\\] sink(s).*";
            };
          ]
        };
        {
          kind = `Function;
          define_name = "qualifier.qux";
          returns = [];
          sink_parameters = [
            { name = "arg"; sinks = [Taint.Sinks.Test] }
          ];
          tito_parameters = [];
          errors = [];
        };
        {
          kind = `Function;
          define_name = "qualifier.bad";
          returns = [];
          sink_parameters = [
            { name = "arg"; sinks = [Taint.Sinks.Test] }
          ];
          tito_parameters = [];
          errors = [];
        };
        {
          kind = `Function;
          define_name = "qualifier.bar";
          returns = [Sources.Test];
          sink_parameters = [];
          tito_parameters = [];
          errors = [];
        };
        {
          kind = `Function;
          define_name = "qualifier.some_source";
          returns = [Sources.Test];
          sink_parameters = [];
          tito_parameters = [];
          errors = [];
        };
        {
          kind = `Function;
          define_name = "qualifier.list_sink";
          returns = [];
          sink_parameters = [
            { name = "list"; sinks = [Taint.Sinks.Test] }
          ];
          tito_parameters = [];
          errors = [];
        };
        {
          kind = `Function;
          define_name = "qualifier.no_list_match";
          returns = [];
          sink_parameters = [];
          tito_parameters = [];
          errors = [];
        };
        {
          kind = `Function;
          define_name = "qualifier.list_match";
          returns = [];
          sink_parameters = [];
          tito_parameters = [];
          errors = [
            {
              code = 5002;
              pattern = ".*Test flow.*Data from \\[Test\\] source(s).* \\[Test\\] sink(s).*";
            };
          ]
        };
        {
          kind = `Function;
          define_name = "qualifier.test_getattr_obj_no_match";
          returns = [];
          sink_parameters = [];
          tito_parameters = [];
          errors = [];
        };
        {
          kind = `Function;
          define_name = "qualifier.test_getattr_field_match";
          returns = [];
          sink_parameters = [];
          tito_parameters = ["some_obj"];
          errors = [
            {
              code = 5010;
              pattern = ".*Attacker may control at least one argument to getattr(,)";
            };
          ]
        };
        {
          kind = `Function;
          define_name = "qualifier.deep_tito";
          returns = [];
          sink_parameters = [];
          tito_parameters = ["tito"];
          errors = [];
        };
        {
          kind = `Function;
          define_name = "qualifier.test_deep_tito_no_match";
          returns = [];
          sink_parameters = [];
          tito_parameters = [];
          errors = [];
        };
        {
          kind = `Function;
          define_name = "qualifier.test_deep_tito_match";
          returns = [];
          sink_parameters = [];
          tito_parameters = [];
          errors = [
            {
              code = 5010;
              pattern = ".*Attacker may control at least one argument to getattr(,)";
            };
          ]
        };
      ]
    }


let test_integration _ =
  TaintIntegrationTest.Files.dummy_dependency |> ignore;

  let test_paths =
    (* Shameful things happen here... *)
    Path.current_working_directory ()
    |> Path.show
    |> String.chop_suffix_exn ~suffix:"_build/default/taint/test"
    |> (fun root -> Path.create_absolute root)
    |> (fun root -> Path.create_relative ~root ~relative:"taint/test/integration/")
    |> (fun root -> Path.list ~file_filter:(String.is_suffix ~suffix:".py") ~root ())
  in
  let run_test path =
    let check_expectation ~suffix actual =
      let output_filename ~suffix =
        Path.show path
        |> (fun path -> path ^ suffix ^ ".actual")
      in
      let write_output ~suffix content =
        try
          output_filename ~suffix
          |> Path.create_absolute ~follow_symbolic_links:false
          |> File.create ~content
          |> File.write
        with Unix.Unix_error _ ->
          failwith (Format.asprintf "Could not write `%s` file for %a" suffix Path.pp path)
      in
      let remove_old_output ~suffix =
        try
          output_filename ~suffix
          |> Sys.remove
        with Sys_error _ ->
          (* be silent *)
          ()
      in
      let get_expected ~suffix =
        try
          Path.show path
          |> (fun path -> path ^ suffix)
          |> Path.create_absolute
          |> File.create
          |> File.content
          |> (fun content -> Option.value_exn content)
        with Unix.Unix_error _ ->
          ""
      in
      let expected = get_expected ~suffix in
      if String.equal expected actual then
        remove_old_output ~suffix
      else begin
        write_output ~suffix actual;
        Printf.printf "Expectations differ for %s %s\n" suffix (Path.show path);
        assert_bool
          (Format.asprintf
             "Expectations differ for %s %s\n%a"
             suffix
             (Path.show path)
             (Test.diff ~print:String.pp)
             (expected, actual))
          false
      end
    in
    let serialized_models =
      let source =
        File.create path
        |> File.content
        |> (fun content -> Option.value_exn content)
      in
      let handle =
        Path.show path
        |> String.split ~on:'/'
        |> List.last_exn
      in
      let check_call_graph_expectation call_graph =
        let dependencies = DependencyGraph.from_callgraph call_graph in
        let actual = Format.asprintf "Call dependencies\n%a" DependencyGraph.pp dependencies in
        check_expectation ~suffix:".cg" actual
      in
      let check_overrides_expectation overrides =
        let actual = Format.asprintf "Overrides\n%a" DependencyGraph.pp overrides in
        check_expectation ~suffix:".overrides" actual
      in
      let { callgraph; all_callables; environment; overrides } =
        create_call_graph ~path:handle source
      in
      let dependencies =
        DependencyGraph.from_callgraph callgraph
        |> DependencyGraph.union overrides
        |> DependencyGraph.reverse
      in
      check_call_graph_expectation callgraph;
      check_overrides_expectation overrides;
      Analysis.compute_fixpoint
        ~configuration:Test.mock_configuration
        ~scheduler:(Scheduler.mock ())
        ~environment
        ~analyses:[Taint.Analysis.abstract_kind]
        ~dependencies
        ~all_callables
        Fixpoint.Epoch.initial
      |> ignore;
      let serialized_model callable: string =
        let externalization =
          Interprocedural.Analysis.externalize Taint.Analysis.abstract_kind callable
          |> List.map ~f:(fun json -> Yojson.Safe.pretty_to_string ~std:true json ^ "\n")
          |> String.concat ~sep:""
        in
        externalization
      in
      List.map all_callables ~f:serialized_model
      |> List.sort ~compare:String.compare
      |> String.concat ~sep:""
    in
    check_expectation ~suffix:".models" serialized_models
  in
  List.iter test_paths ~f:run_test


let () =
  "taint">:::[
    "fixpoint">::test_fixpoint;
    "integration">::test_integration;
  ]
  |> TestHelper.run_with_taint_models
