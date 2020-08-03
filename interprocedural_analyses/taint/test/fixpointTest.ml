(* Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Taint
open Interprocedural
open TestHelper

type expect_fixpoint = {
  expect: expectation list;
  iterations: int;
}

let assert_fixpoint ?models ~context source ~expect:{ iterations = expect_iterations; expect } =
  let scheduler = Test.mock_scheduler () in
  let { all_callables; callgraph; environment; overrides } =
    initialize ?models ~handle:"qualifier.py" ~context source
  in
  let dependencies =
    DependencyGraph.from_callgraph callgraph
    |> DependencyGraph.union overrides
    |> DependencyGraph.reverse
  in
  let analyses = [Taint.Analysis.abstract_kind] in
  let iterations =
    Analysis.compute_fixpoint
      ~scheduler
      ~environment
      ~analyses
      ~dependencies
      ~filtered_callables:Callable.Set.empty
      ~all_callables
      Fixpoint.Epoch.initial
  in
  assert_bool "Callgraph is empty!" (Callable.RealMap.length callgraph > 0);
  assert_equal ~msg:"Fixpoint iterations" expect_iterations iterations ~printer:Int.to_string;
  List.iter ~f:(check_expectation ~environment) expect


let test_fixpoint context =
  assert_fixpoint
    ~context
    {|
      from builtins import __test_source, __test_sink, __user_controlled
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

      def getattr_obj_no_match():
        obj = __user_controlled()
        getattr(obj, 'foo')

      def getattr_field_match(some_obj):
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

      class Class:
        tainted = ...
        untainted = ...
        @property
        def property(self):
          return self.tainted

      def property_into_sink(input):
        c: Class = ...
        c.tainted = input
        __test_sink(c.property)

      def uses_property(c: Class):
        c.tainted = __test_source()
        return c.property

      def uses_property_but_no_taint(c: Class):
        c.untainted = __test_source()
        return c.property
    |}
    ~expect:
      {
        iterations = 4;
        expect =
          [
            outcome
              ~kind:`Function
              ~errors:
                [
                  {
                    code = 5001;
                    pattern =
                      ".*Possible shell injection.*Possible remote code execution due to \
                       \\[UserControlled\\].*\\[RemoteCodeExecution\\].*";
                  };
                ]
              "qualifier.rce_problem";
            outcome
              ~kind:`Function
              ~errors:
                [
                  {
                    code = 5002;
                    pattern = ".*Test flow.*Data from \\[Test\\] source(s).* \\[Test\\] sink(s).*";
                  };
                ]
              "qualifier.match_flows";
            outcome
              ~kind:`Function
              ~errors:
                [
                  {
                    code = 5002;
                    pattern = ".*Test flow.*Data from \\[Test\\] source(s).* \\[Test\\] sink(s).*";
                  };
                ]
              "qualifier.match_flows_multiple";
            outcome
              ~kind:`Function
              ~errors:
                [
                  {
                    code = 5002;
                    pattern = ".*Test flow.*Data from \\[Test\\] source(s).* \\[Test\\] sink(s).*";
                  };
                ]
              "qualifier.match_via_methods";
            outcome ~kind:`Function ~errors:[] "qualifier.no_match_via_methods";
            outcome
              ~kind:`Function
              ~errors:
                [
                  {
                    code = 5002;
                    pattern = ".*Test flow.*Data from \\[Test\\] source(s).* \\[Test\\] sink(s).*";
                  };
                ]
              "qualifier.match_via_receiver";
            outcome
              ~kind:`Function
              ~sink_parameters:[{ name = "arg"; sinks = [Taint.Sinks.NamedSink "Test"] }]
              ~errors:[]
              "qualifier.qux";
            outcome
              ~kind:`Function
              ~sink_parameters:[{ name = "arg"; sinks = [Taint.Sinks.NamedSink "Test"] }]
              "qualifier.bad";
            outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] ~errors:[] "qualifier.bar";
            outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.some_source";
            outcome
              ~kind:`Function
              ~sink_parameters:[{ name = "list"; sinks = [Taint.Sinks.NamedSink "Test"] }]
              "qualifier.list_sink";
            outcome ~kind:`Function ~errors:[] "qualifier.no_list_match";
            outcome
              ~kind:`Function
              ~errors:
                [
                  {
                    code = 5002;
                    pattern = ".*Test flow.*Data from \\[Test\\] source(s).* \\[Test\\] sink(s).*";
                  };
                ]
              "qualifier.list_match";
            outcome ~kind:`Function ~errors:[] "qualifier.getattr_obj_no_match";
            outcome
              ~kind:`Function
              ~tito_parameters:["some_obj"]
              ~errors:
                [
                  {
                    code = 5010;
                    pattern = ".*Attacker may control at least one argument to getattr(,)";
                  };
                ]
              "qualifier.getattr_field_match";
            outcome ~kind:`Function ~tito_parameters:["tito"] ~errors:[] "qualifier.deep_tito";
            outcome ~kind:`Function ~errors:[] "qualifier.test_deep_tito_no_match";
            outcome
              ~kind:`Function
              ~errors:
                [
                  {
                    code = 5010;
                    pattern = ".*Attacker may control at least one argument to getattr(,)";
                  };
                ]
              "qualifier.test_deep_tito_match";
            outcome
              ~kind:`Function
              ~sink_parameters:[{ name = "input"; sinks = [Sinks.NamedSink "Test"] }]
              "qualifier.property_into_sink";
            outcome ~kind:`Method ~tito_parameters:["self"] "qualifier.Class.property";
            outcome
              ~kind:`Function
              ~tito_parameters:[]
              ~returns:[Sources.NamedSource "Test"]
              "qualifier.uses_property";
            outcome
              ~kind:`Function
              ~tito_parameters:["c"]
              ~returns:[]
              "qualifier.uses_property_but_no_taint";
          ];
      }


let test_combined_analysis context =
  assert_fixpoint
    ~context
    ~models:
      {|
      def qualifier.combined_model(x, y: TaintSink[Demo], z: TaintInTaintOut): ...
    |}
    {|
      from builtins import __test_sink, __user_controlled
      def combined_model(x, y, z):
        __test_sink(x)
        return x or __user_controlled()
    |}
    ~expect:
      {
        expect =
          [
            outcome
              ~kind:`Function
              ~returns:[Sources.NamedSource "UserControlled"]
              ~sink_parameters:
                [
                  { name = "x"; sinks = [Sinks.NamedSink "Test"] };
                  { name = "y"; sinks = [Sinks.NamedSink "Demo"] };
                ]
              ~tito_parameters:["x"; "z"]
              "qualifier.combined_model";
          ];
        iterations = 2;
      }


let test_skipped_analysis context =
  assert_fixpoint
    ~context
    ~models:
      {|
      def qualifier.skipped_model(x, y: TaintSink[Demo], z: TaintInTaintOut) -> SkipAnalysis: ...
    |}
    {|
      from builtins import __test_sink, __user_controlled
      def skipped_model(x, y, z):
        __test_sink(x)
        return x or __user_controlled()
    |}
    ~expect:
      {
        expect =
          [
            outcome
              ~kind:`Function
              ~sink_parameters:[{ name = "y"; sinks = [Sinks.NamedSink "Demo"] }]
              ~tito_parameters:["z"]
              ~analysis_mode:Taint.Result.SkipAnalysis
              "qualifier.skipped_model";
          ];
        iterations = 1;
      }


let test_sanitized_analysis context =
  assert_fixpoint
    ~context
    ~models:
      {|
      def qualifier.sanitized_model(x, y: TaintSink[Demo], z: TaintInTaintOut) -> Sanitize: ...
    |}
    {|
      from builtins import __test_sink, __user_controlled
      def sanitized_model(x, y, z):
        eval(__user_controlled())
        __test_sink(x)
        return x or __user_controlled()
    |}
    ~expect:
      {
        expect =
          [
            outcome
              ~kind:`Function
              ~sink_parameters:[{ name = "y"; sinks = [Sinks.NamedSink "Demo"] }]
              ~tito_parameters:["z"]
              ~errors:[{ code = 5001; pattern = ".*" }]
              ~analysis_mode:Taint.Result.Sanitize
              "qualifier.sanitized_model";
          ];
        iterations = 1;
      }


let test_primed_source_analysis context =
  assert_fixpoint
    ~context
    ~models:{|
      def qualifier.primed_model(x, y: TaintSource[UserControlled]): ...
    |}
    {|
      def primed_model(x, y):
        eval(y)
    |}
    ~expect:
      {
        expect =
          [
            outcome
              ~kind:`Function
              ~source_parameters:[{ name = "y"; sources = [Sources.NamedSource "UserControlled"] }]
              ~sink_parameters:[{ name = "y"; sinks = [Sinks.NamedSink "RemoteCodeExecution"] }]
              ~errors:[{ code = 5001; pattern = ".*Possible shell injection.*" }]
              "qualifier.primed_model";
          ];
        iterations = 2;
      }


let test_primed_sink_analysis context =
  assert_fixpoint
    ~context
    ~models:
      {|
      def qualifier.primed_model(x, y: TaintSource[Test]) -> TaintSink[Test]: ...
    |}
    {|
      def primed_model(x, y):
        return y

      def no_flow(x, y):
        return x
    |}
    ~expect:
      {
        expect =
          [
            outcome
              ~kind:`Function
              ~returns:[Sources.NamedSource "Test"]
              ~source_parameters:[{ name = "y"; sources = [Sources.NamedSource "Test"] }]
              ~sink_parameters:[] (* No backward prop on return sinks *)
              ~tito_parameters:["y"]
              ~errors:[{ code = 5002; pattern = ".*Test.*" }]
              "qualifier.primed_model";
          ];
        iterations = 2;
      }


(* Check that overrides are propagated properly, sources, sinks, and whether a target is obscure or
   not. *)
let test_overrides context =
  assert_fixpoint
    ~context
    {|
      from builtins import __test_source, __test_sink, __user_controlled
      class Base:
        def split(self):
          pass

        def some_source(self):
          pass

        def some_sink(self, arg):
          pass

      class C(Base):
        def split(self): ...

        def some_sink(self, arg):
          __test_sink(arg)

      class D(C):
        def some_source(self):
          return __test_source()

        def some_sink(self, arg):
          eval(arg)

      class E(Base):
        def some_source(self):
          return __user_controlled()

      def test_obscure_override(b: Base):
        return b.split()

    |}
    ~expect:
      {
        iterations = 4;
        expect =
          [
            outcome ~kind:`Override ~obscure:true "qualifier.Base.split";
            outcome
              ~kind:`Function
              ~tito_parameters:["b"]
              ~errors:[]
              "qualifier.test_obscure_override";
            outcome
              ~kind:`Override
              ~returns:[Sources.NamedSource "Test"; Sources.NamedSource "UserControlled"]
              "qualifier.Base.some_source";
            outcome ~kind:`Method ~returns:[] "qualifier.Base.some_source";
            outcome ~kind:`Method "qualifier.Base.some_sink";
            outcome
              ~kind:`Override
              ~sink_parameters:
                [
                  {
                    name = "arg";
                    sinks = [Sinks.NamedSink "RemoteCodeExecution"; Sinks.NamedSink "Test"];
                  };
                ]
              "qualifier.Base.some_sink";
            outcome
              ~kind:`Method
              ~sink_parameters:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
              "qualifier.C.some_sink";
            outcome
              ~kind:`Override
              ~sink_parameters:
                [
                  {
                    name = "arg";
                    sinks = [Sinks.NamedSink "RemoteCodeExecution"; Sinks.NamedSink "Test"];
                  };
                ]
              "qualifier.C.some_sink";
            outcome
              ~kind:`Method
              ~sink_parameters:[{ name = "arg"; sinks = [Sinks.NamedSink "RemoteCodeExecution"] }]
              "qualifier.D.some_sink";
            outcome ~kind:`Method ~returns:[Sources.NamedSource "Test"] "qualifier.D.some_source";
            outcome
              ~kind:`Method
              ~returns:[Sources.NamedSource "UserControlled"]
              "qualifier.E.some_source";
          ];
      }


let () =
  [
    "fixpoint", test_fixpoint;
    "combined_analysis", test_combined_analysis;
    "skipped_analysis", test_skipped_analysis;
    "sanitized_analysis", test_sanitized_analysis;
    "primed_source_analysis", test_primed_source_analysis;
    "primed_sink_analysis", test_primed_sink_analysis;
    "overrides", test_overrides;
  ]
  |> TestHelper.run_with_taint_models ~name:"fixpoint"
