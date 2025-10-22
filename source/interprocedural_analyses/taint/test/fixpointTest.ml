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
  let ({
         TestEnvironment.static_analysis_configuration;
         taint_configuration;
         taint_configuration_shared_memory;
         whole_program_call_graph;
         get_define_call_graph;
         global_constants;
         pyre_api;
         override_graph_heap;
         override_graph_shared_memory;
         initial_models;
         initial_callables;
         stubs;
         class_interval_graph_shared_memory;
         callables_to_definitions_map;
         type_of_expression_shared_memory;
         _;
       } as test_environment)
    =
    initialize ?models_source ~handle:"qualifier.py" ~context source
  in
  let { DependencyGraph.dependency_graph; callables_to_analyze; override_targets; _ } =
    DependencyGraph.build_whole_program_dependency_graph
      ~static_analysis_configuration
      ~prune:DependencyGraph.PruneMethod.None
      ~initial_callables
      ~call_graph:whole_program_call_graph
      ~overrides:override_graph_heap
      ~ignore_decorated_targets:true
  in
  let state =
    TaintFixpoint.record_initial_models
      ~scheduler:(Test.mock_scheduler ())
      ~callables_to_analyze
      ~stubs
      ~override_targets
      ~initial_models
  in
  let scheduler = Test.mock_scheduler () in
  let scheduler_policy = Scheduler.Policy.legacy_fixed_chunk_count () in
  let ({ TaintFixpoint.fixpoint_reached_iterations; _ } as fixpoint) =
    TaintFixpoint.compute
      ~scheduler
      ~scheduler_policy
      ~override_graph:
        (Interprocedural.OverrideGraph.SharedMemory.read_only override_graph_shared_memory)
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
          global_constants = Interprocedural.GlobalConstants.SharedMemory.read_only global_constants;
          type_of_expression_shared_memory;
          callables_to_definitions_map =
            Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map;
        }
      ~callables_to_analyze
      ~max_iterations:100
      ~error_on_max_iterations:true
      ~epoch:TaintFixpoint.Epoch.initial
      ~state
  in

  assert_bool
    "Call graph is empty!"
    (not (CallGraph.WholeProgramCallGraph.is_empty whole_program_call_graph));
  assert_equal
    ~msg:"Fixpoint iterations"
    expect_iterations
    fixpoint_reached_iterations
    ~printer:Int.to_string;
  let get_model =
    TaintFixpoint.State.ReadOnly.get_model
      (TaintFixpoint.State.read_only fixpoint.TaintFixpoint.state)
  in
  let get_errors callable =
    callable
    |> TaintFixpoint.State.ReadOnly.get_result (TaintFixpoint.State.read_only state)
    |> IssueHandle.SerializableMap.data
  in
  let () =
    List.iter ~f:(check_expectation ~pyre_api ~taint_configuration ~get_model ~get_errors) expect
  in
  TaintFixpoint.State.cleanup ~keep_models:false fixpoint.TaintFixpoint.state;
  TestEnvironment.cleanup test_environment


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
        iterations = 2;
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
              ~parameter_sinks:[{ name = "arg"; sinks = [Taint.Sinks.NamedSink "Test"] }]
              ~errors:[]
              "qualifier.qux";
            outcome
              ~kind:`Function
              ~parameter_sinks:[{ name = "arg"; sinks = [Taint.Sinks.NamedSink "Test"] }]
              "qualifier.bad";
            outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] ~errors:[] "qualifier.bar";
            outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "qualifier.some_source";
            outcome
              ~kind:`Function
              ~parameter_sinks:[{ name = "list"; sinks = [Taint.Sinks.NamedSink "Test"] }]
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
              ~parameter_titos:[{ name = "some_obj"; titos = [Sinks.LocalReturn] }]
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
              ~parameter_titos:[{ name = "tito"; titos = [Sinks.LocalReturn] }]
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
              ~parameter_sinks:[{ name = "input"; sinks = [Sinks.NamedSink "Test"] }]
              "qualifier.property_into_sink";
            outcome
              ~kind:`Method
              ~parameter_titos:[{ name = "self"; titos = [Sinks.LocalReturn] }]
              "qualifier.Class.property";
            outcome
              ~kind:`Function
              ~parameter_titos:[]
              ~returns:[Sources.NamedSource "Test"]
              "qualifier.uses_property";
            outcome
              ~kind:`Function
              ~parameter_titos:[{ name = "c"; titos = [Sinks.LocalReturn] }]
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
              ~parameter_sinks:
                [
                  { name = "x"; sinks = [Sinks.NamedSink "Test"] };
                  { name = "y"; sinks = [Sinks.NamedSink "Demo"] };
                ]
              ~parameter_titos:
                [
                  { name = "x"; titos = [Sinks.LocalReturn] };
                  { name = "z"; titos = [Sinks.LocalReturn] };
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
              ~parameter_sinks:[{ name = "y"; sinks = [Sinks.NamedSink "Demo"] }]
              ~parameter_titos:[{ name = "z"; titos = [Sinks.LocalReturn] }]
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
              ~parameter_sinks:[{ name = "y"; sinks = [Sinks.NamedSink "Demo"] }]
              ~parameter_titos:[{ name = "z"; titos = [Sinks.LocalReturn] }]
              ~errors:[{ code = 5001; pattern = ".*" }]
              ~global_sanitizer:Taint.Sanitize.all
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
              ~parameter_sources:[{ name = "y"; sources = [Sources.NamedSource "UserControlled"] }]
              ~parameter_sinks:[{ name = "y"; sinks = [Sinks.NamedSink "RemoteCodeExecution"] }]
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
              ~parameter_sources:[{ name = "y"; sources = [Sources.NamedSource "Test"] }]
              ~parameter_sinks:[{ name = "y"; sinks = [Sinks.NamedSink "Test"] }]
                (* Backward propagation on return sinks *)
              ~return_sinks:[Sinks.NamedSink "Test"]
              ~parameter_titos:[{ name = "y"; titos = [Sinks.LocalReturn] }]
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
        iterations = 2;
        expect =
          [
            outcome
              ~kind:`Override
              ~analysis_modes:(Model.ModeSet.singleton Model.Mode.Obscure)
              "qualifier.Base.split";
            outcome
              ~kind:`Function
              ~parameter_titos:[{ name = "b"; titos = [Sinks.LocalReturn] }]
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
              ~parameter_sinks:
                [
                  {
                    name = "arg";
                    sinks = [Sinks.NamedSink "RemoteCodeExecution"; Sinks.NamedSink "Test"];
                  };
                ]
              "qualifier.Base.some_sink";
            outcome
              ~kind:`Method
              ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "Test"] }]
              "qualifier.C.some_sink";
            outcome
              ~kind:`Override
              ~parameter_sinks:
                [
                  {
                    name = "arg";
                    sinks = [Sinks.NamedSink "RemoteCodeExecution"; Sinks.NamedSink "Test"];
                  };
                ]
              "qualifier.C.some_sink";
            outcome
              ~kind:`Method
              ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.NamedSink "RemoteCodeExecution"] }]
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
