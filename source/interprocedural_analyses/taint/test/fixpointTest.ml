(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Taint
open Interprocedural
open TestHelper

type expect_fixpoint = {
  expect: expectation list;
  iterations: int;
}

let assert_fixpoint
    ?models_source
    ~context
    source
    ~expect:{ iterations = expect_iterations; expect }
  =
  let scheduler = Test.mock_scheduler () in
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
    initialize ?models_source ~handle:"qualifier.py" ~context source
  in
  let { DependencyGraph.dependency_graph; callables_to_analyze; override_targets; _ } =
    Interprocedural.DependencyGraph.build_whole_program_dependency_graph
      ~prune:false
      ~initial_callables
      ~call_graph:whole_program_call_graph
      ~overrides:override_graph_heap
  in
  let fixpoint_state =
    Fixpoint.compute
      ~scheduler
      ~type_environment:environment
      ~override_graph:override_graph_shared_memory
      ~dependency_graph
      ~context:
        {
          Fixpoint.Context.type_environment = environment;
          class_interval_graph;
          define_call_graphs;
        }
      ~initial_callables:(Interprocedural.FetchCallables.get_callables initial_callables)
      ~stubs
      ~override_targets
      ~callables_to_analyze
      ~initial_models
      ~max_iterations:100
      ~epoch:Fixpoint.Epoch.initial
  in

  assert_bool
    "Call graph is empty!"
    (not (CallGraph.WholeProgramCallGraph.is_empty whole_program_call_graph));
  assert_equal
    ~msg:"Fixpoint iterations"
    expect_iterations
    (Fixpoint.get_iterations fixpoint_state)
    ~printer:Int.to_string;
  let get_model = Fixpoint.get_model fixpoint_state in
  let get_errors = Fixpoint.get_result fixpoint_state in
  let () = List.iter ~f:(check_expectation ~environment ~get_model ~get_errors) expect in
  let () = Fixpoint.cleanup fixpoint_state in
  ()


let test_fixpoint context =
  assert_fixpoint
    ~context
    {|
      from builtins import _test_source, _test_sink, _user_controlled
      def bar():
        return _test_source()

      def qux(arg):
        _test_sink(arg)

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
        x = _user_controlled()
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
        _test_sink(list[1])

      def list_match():
        x = [5, _test_source()]
        list_sink(x)

      def no_list_match():
        x = [_test_source(), 5]
        list_sink(x)

      def getattr_obj_no_match():
        obj = _user_controlled()
        getattr(obj, 'foo')

      def getattr_field_match(some_obj):
        field = _user_controlled()
        return getattr(some_obj, field)

      def deep_tito(tito, no_tito):
          x = { 'f': tito }
          y = { 'g': x }
          return y

      def test_deep_tito_no_match():
        obj = deep_tito(_user_controlled(), _test_source())
        getattr('obj', obj.f.g)

      def test_deep_tito_match():
        obj = deep_tito(_user_controlled(), _test_source())
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
        _test_sink(c.property)

      def uses_property(c: Class):
        c.tainted = _test_source()
        return c.property

      def uses_property_but_no_taint(c: Class):
        c.untainted = _test_source()
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
              ~tito_parameters:[{ name = "some_obj"; sinks = [Sinks.LocalReturn] }]
              ~errors:
                [
                  {
                    code = 5010;
                    pattern = ".*Attacker may control at least one argument to getattr(,)";
                  };
                ]
              "qualifier.getattr_field_match";
            outcome
              ~kind:`Function
              ~tito_parameters:[{ name = "tito"; sinks = [Sinks.LocalReturn] }]
              ~errors:[]
              "qualifier.deep_tito";
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
            outcome
              ~kind:`Method
              ~tito_parameters:[{ name = "self"; sinks = [Sinks.LocalReturn] }]
              "qualifier.Class.property";
            outcome
              ~kind:`Function
              ~tito_parameters:[]
              ~returns:[Sources.NamedSource "Test"]
              "qualifier.uses_property";
            outcome
              ~kind:`Function
              ~tito_parameters:[{ name = "c"; sinks = [Sinks.LocalReturn] }]
              ~returns:[]
              "qualifier.uses_property_but_no_taint";
          ];
      }


let test_combined_analysis context =
  assert_fixpoint
    ~context
    ~models_source:
      {|
      def _test_sink(arg: TaintSink[Test, Via[special_sink]]): ...
      def _user_controlled() -> TaintSource[UserControlled]: ...
      def qualifier.combined_model(x, y: TaintSink[Demo], z: TaintInTaintOut): ...
    |}
    {|
      from builtins import _test_sink, _user_controlled
      def combined_model(x, y, z):
        _test_sink(x)
        return x or _user_controlled()
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
              ~tito_parameters:
                [
                  { name = "x"; sinks = [Sinks.LocalReturn] };
                  { name = "z"; sinks = [Sinks.LocalReturn] };
                ]
              "qualifier.combined_model";
          ];
        iterations = 2;
      }


let test_skipped_analysis context =
  assert_fixpoint
    ~context
    ~models_source:
      {|
      @SkipAnalysis
      def qualifier.skipped_model(x, y: TaintSink[Demo], z: TaintInTaintOut): ...
    |}
    {|
      from builtins import _test_sink, _user_controlled
      def skipped_model(x, y, z):
        _test_sink(x)
        return x or _user_controlled()
    |}
    ~expect:
      {
        expect =
          [
            outcome
              ~kind:`Function
              ~sink_parameters:[{ name = "y"; sinks = [Sinks.NamedSink "Demo"] }]
              ~tito_parameters:[{ name = "z"; sinks = [Sinks.LocalReturn] }]
              ~analysis_modes:(Model.ModeSet.singleton SkipAnalysis)
              "qualifier.skipped_model";
          ];
        iterations = 1;
      }


let test_sanitized_analysis context =
  assert_fixpoint
    ~context
    ~models_source:
      {|
      @Sanitize
      def qualifier.sanitized_model(x, y: TaintSink[Demo], z: TaintInTaintOut): ...
    |}
    {|
      from builtins import _test_sink, _user_controlled
      def sanitized_model(x, y, z):
        eval(_user_controlled())
        _test_sink(x)
        return x or _user_controlled()
    |}
    ~expect:
      {
        expect =
          [
            outcome
              ~kind:`Function
              ~sink_parameters:[{ name = "y"; sinks = [Sinks.NamedSink "Demo"] }]
              ~tito_parameters:[{ name = "z"; sinks = [Sinks.LocalReturn] }]
              ~errors:[{ code = 5001; pattern = ".*" }]
              ~global_sanitizer:
                { Taint.Domains.Sanitize.sources = Some All; sinks = Some All; tito = Some All }
              "qualifier.sanitized_model";
          ];
        iterations = 1;
      }


let test_primed_source_analysis context =
  assert_fixpoint
    ~context
    ~models_source:
      {|
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
    ~models_source:
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
              ~tito_parameters:[{ name = "y"; sinks = [Sinks.LocalReturn] }]
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
      from builtins import _test_source, _test_sink, _user_controlled
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
          _test_sink(arg)

      class D(C):
        def some_source(self):
          return _test_source()

        def some_sink(self, arg):
          eval(arg)

      class E(Base):
        def some_source(self):
          return _user_controlled()

      def test_obscure_override(b: Base):
        return b.split()

    |}
    ~expect:
      {
        iterations = 4;
        expect =
          [
            outcome
              ~kind:`Override
              ~analysis_modes:(Model.ModeSet.singleton Model.Mode.Obscure)
              "qualifier.Base.split";
            outcome
              ~kind:`Function
              ~tito_parameters:[{ name = "b"; sinks = [Sinks.LocalReturn] }]
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
  "fixpoint"
  >::: [
         "fixpoint" >:: test_fixpoint;
         "combined_analysis" >:: test_combined_analysis;
         "skipped_analysis" >:: test_skipped_analysis;
         "sanitized_analysis" >:: test_sanitized_analysis;
         "primed_source_analysis" >:: test_primed_source_analysis;
         "primed_sink_analysis" >:: test_primed_sink_analysis;
         "overrides" >:: test_overrides;
       ]
  |> Test.run
