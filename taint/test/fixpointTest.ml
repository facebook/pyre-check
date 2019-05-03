(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Pyre
open Taint

open Interprocedural
open TestHelper


type expect_fixpoint = {
  expect: expectation list;
  iterations: int;
}


let assert_fixpoint ?models source ~expect:{ iterations = expect_iterations; expect } =
  let scheduler = Scheduler.mock () in
  let { all_callables; callgraph; environment; overrides } =
    initialize
      ?models
      ~qualifier:"qualifier"
      source
  in

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
    let error_string = Error.description ~show_error_traces:true error in
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
  assert_equal ~msg:"Fixpoint iterations" expect_iterations iterations ~printer:Int.to_string;
  List.iter ~f:check_expectation expect;
  List.iter2_exn expect_results results ~f:assert_errors


let test_fixpoint _ =
  assert_fixpoint
    {|
      def bar():
        return __test_source()

      def qux(arg):
        __test_sink(arg)

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
        x = __user_controlled()
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
        __test_sink(list[1])

      def list_match():
        x = [5, __test_source()]
        list_sink(x)

      def no_list_match():
        x = [__test_source(), 5]
        list_sink(x)

      def test_getattr_obj_no_match():
        obj = __user_controlled()
        getattr(obj, 'foo')

      def test_getattr_field_match(some_obj):
        field = __user_controlled()
        return getattr(some_obj, field)

      def deep_tito(tito, no_tito):
          x = { 'f': tito }
          y = { 'g': x }
          return y

      def test_deep_tito_no_match():
        obj = deep_tito(__user_controlled(), __test_source())
        getattr('obj', obj.f.g)

      def test_deep_tito_match():
        obj = deep_tito(__user_controlled(), __test_source())
        getattr('obj', obj.g.f)
    |}
    ~expect:{
      iterations = 4;
      expect = [
        outcome
          ~kind:`Function
          ~errors:[
            {
              code = 5001;
              pattern = ".*Possible shell injection.*Data from \\[UserControlled\\].*\\[RemoteCodeExecution\\].*";
            };
          ]
          "qualifier.rce_problem";

        outcome
          ~kind:`Function
          ~errors:[
            {
              code = 5002;
              pattern = ".*Test flow.*Data from \\[Test\\] source(s).* \\[Test\\] sink(s).*";
            }
          ]
          "qualifier.match_flows";

        outcome
          ~kind:`Function
          ~errors:[
            {
              code = 5002;
              pattern = ".*Test flow.*Data from \\[Test\\] source(s).* \\[Test\\] sink(s).*";
            }
          ]
          "qualifier.match_flows_multiple";

        outcome
          ~kind:`Function
          ~errors:[
            {
              code = 5002;
              pattern = ".*Test flow.*Data from \\[Test\\] source(s).* \\[Test\\] sink(s).*";
            };
          ]
          "qualifier.match_via_methods";

        outcome
          ~kind:`Function
          ~errors:[]
          "qualifier.no_match_via_methods";

        outcome
          ~kind:`Function
          ~errors:[
            {
              code = 5002;
              pattern = ".*Test flow.*Data from \\[Test\\] source(s).* \\[Test\\] sink(s).*";
            };
          ]
          "qualifier.match_via_receiver";

        outcome
          ~kind:`Function
          ~sink_parameters:[
            { name = "arg"; sinks = [Taint.Sinks.Test] }
          ]
          ~errors:[]
          "qualifier.qux";

        outcome
          ~kind:`Function
          ~sink_parameters:[
            { name = "arg"; sinks = [Taint.Sinks.Test] }
          ]
          "qualifier.bad";

        outcome
          ~kind:`Function
          ~returns:[Sources.Test]
          ~errors:[]
          "qualifier.bar";

        outcome
          ~kind:`Function
          ~returns:[Sources.Test]
          "qualifier.some_source";

        outcome
          ~kind:`Function
          ~sink_parameters:[
            { name = "list"; sinks = [Taint.Sinks.Test] }
          ]
          "qualifier.list_sink";

        outcome
          ~kind:`Function
          ~errors:[]
          "qualifier.no_list_match";

        outcome
          ~kind:`Function
          ~errors:[
            {
              code = 5002;
              pattern = ".*Test flow.*Data from \\[Test\\] source(s).* \\[Test\\] sink(s).*";
            };
          ]
          "qualifier.list_match";

        outcome
          ~kind:`Function
          ~errors:[]
          "qualifier.test_getattr_obj_no_match";

        outcome
          ~kind:`Function
          ~tito_parameters:["some_obj"]
          ~errors:[
            {
              code = 5010;
              pattern = ".*Attacker may control at least one argument to getattr(,)";
            };
          ]
          "qualifier.test_getattr_field_match";

        outcome
          ~kind:`Function
          ~tito_parameters:["tito"]
          ~errors:[]
          "qualifier.deep_tito";

        outcome
          ~kind:`Function
          ~errors:[]
          "qualifier.test_deep_tito_no_match";

        outcome
          ~kind:`Function
          ~errors:[
            {
              code = 5010;
              pattern = ".*Attacker may control at least one argument to getattr(,)";
            };
          ]
          "qualifier.test_deep_tito_match";
      ]
    }


let test_combined_analysis _ =
  assert_fixpoint
    ~models:{|
      def qualifier.combined_model(x, y: TaintSink[Demo], z: TaintInTaintOut): ...
    |}
    {|
      def combined_model(x, y, z):
        __test_sink(x)
        return x or __user_controlled()
    |}
    ~expect:{
      expect = [
        outcome
          ~kind:`Function
          ~returns:[Sources.UserControlled]
          ~sink_parameters:[
            { name = "x"; sinks = [Sinks.Test] };
            { name = "y"; sinks = [Sinks.Demo] };
          ]
          ~tito_parameters:["x"; "z"]
          "qualifier.combined_model";
      ];
      iterations = 2;
    }


let test_skipped_analysis _ =
  assert_fixpoint
    ~models:{|
      def qualifier.skipped_model(x, y: TaintSink[Demo], z: TaintInTaintOut) -> SkipAnalysis: ...
    |}
    {|
      def skipped_model(x, y, z):
        __test_sink(x)
        return x or __user_controlled()
    |}
    ~expect:{
      expect = [
        outcome
          ~kind:`Function
          ~sink_parameters:[
            { name = "y"; sinks = [Sinks.Demo] };
          ]
          ~tito_parameters:["z"]
          "qualifier.skipped_model";
      ];
      iterations = 1;
    }


let test_sanitized_analysis _ =
  assert_fixpoint
    ~models:{|
      def qualifier.sanitized_model(x, y: TaintSink[Demo], z: TaintInTaintOut) -> Sanitize: ...
    |}
    {|
      def sanitized_model(x, y, z):
        eval(__user_controlled())
        __test_sink(x)
        return x or __user_controlled()
    |}
    ~expect:{
      expect = [
        outcome
          ~kind:`Function
          ~sink_parameters:[
            { name = "y"; sinks = [Sinks.Demo] };
          ]
          ~tito_parameters:["z"]
          ~errors:[
            {
              code = 5001;
              pattern = ".*";
            };
          ]
          "qualifier.sanitized_model";
      ];
      iterations = 1;
    }


let test_primed_source_analysis _ =
  assert_fixpoint
    ~models:{|
      def qualifier.primed_model(x, y: TaintSource[UserControlled]): ...
    |}
    {|
      def primed_model(x, y):
        eval(y)
    |}
    ~expect:{
      expect = [
        outcome
          ~kind:`Function
          ~source_parameters:[
            { name = "y"; sources = [Sources.UserControlled] }
          ]
          ~sink_parameters:[
            { name = "y"; sinks = [Sinks.RemoteCodeExecution] };
          ]
          ~errors:[
            {
              code = 5001;
              pattern = ".*Possible shell injection.*";
            };
          ]
          "qualifier.primed_model";
      ];
      iterations = 2;
    }


let test_primed_sink_analysis _ =
  assert_fixpoint
    ~models:{|
      def qualifier.primed_model(x, y: TaintSource[Test]) -> TaintSink[Test]: ...
    |}
    {|
      def primed_model(x, y):
        return y

      def no_flow(x, y):
        return x
    |}
    ~expect:{
      expect = [
        outcome
          ~kind:`Function
          ~returns:[Sources.Test]
          ~source_parameters:[
            { name = "y"; sources = [Sources.Test] }
          ]
          ~sink_parameters:[]  (* No backward prop on return sinks *)
          ~tito_parameters:["y"]
          ~errors:[
            {
              code = 5002;
              pattern = ".*Test.*";
            };
          ]
          "qualifier.primed_model";
      ];
      iterations = 2;
    }


let () =
  "taint">:::[
    "fixpoint">::test_fixpoint;
    "combined_analysis">::test_combined_analysis;
    "skipped_analysis">::test_skipped_analysis;
    "sanitized_analysis">::test_sanitized_analysis;
    "primed_source_analysis">::test_primed_source_analysis;
    "primed_sink_analysis">::test_primed_sink_analysis;
  ]
  |> TestHelper.run_with_taint_models
