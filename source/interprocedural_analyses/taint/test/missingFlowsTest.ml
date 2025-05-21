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
    ~missing_flows
    ~handle
    source
    ~expect:{ iterations = expect_iterations; expect }
  =
  let taint_configuration =
    TaintConfiguration.apply_missing_flows TaintConfiguration.Heap.default missing_flows
  in
  let ({
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
         _;
       } as test_environment)
    =
    initialize
      ?models_source
      ~find_missing_flows:missing_flows
      ~taint_configuration
      ~handle
      ~context
      source
  in
  let { DependencyGraph.dependency_graph; callables_to_analyze; override_targets; _ } =
    DependencyGraph.build_whole_program_dependency_graph
      ~static_analysis_configuration
      ~prune:DependencyGraph.PruneMethod.None
      ~initial_callables
      ~call_graph:whole_program_call_graph
      ~overrides:override_graph_heap
      ~decorator_resolution:Interprocedural.CallGraph.DecoratorResolution.Results.empty
  in
  let state =
    TaintFixpoint.record_initial_models
      ~scheduler:(Test.mock_scheduler ())
      ~initial_models
      ~callables_to_analyze
      ~stubs
      ~override_targets
  in
  let scheduler = Test.mock_scheduler () in
  let scheduler_policy = Scheduler.Policy.legacy_fixed_chunk_count () in
  let callables_to_definitions_map =
    Interprocedural.Target.CallablesSharedMemory.from_callables
      ~scheduler
      ~scheduler_policy
      ~pyre_api
      (Interprocedural.FetchCallables.get initial_callables ~definitions:true ~stubs:true)
  in
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
          global_constants =
            GlobalConstants.SharedMemory.create () |> GlobalConstants.SharedMemory.read_only;
          type_of_expression_shared_memory = Interprocedural.TypeOfExpressionSharedMemory.create ();
          decorator_inlined = false;
          callables_to_definitions_map =
            Interprocedural.Target.CallablesSharedMemory.read_only callables_to_definitions_map;
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
  let () = TaintFixpoint.State.cleanup ~keep_models:false fixpoint.TaintFixpoint.state in
  let () = TestEnvironment.cleanup test_environment in
  ()


let test_obscure context =
  assert_fixpoint
    ~context
    ~missing_flows:Configuration.MissingFlowKind.Obscure
    ~handle:"test_obscure.py"
    ~models_source:{|
      def test_obscure.obscure(x): ...
    |}
    {|
      from builtins import _test_source, _test_sink, _user_controlled

      def obscure(x): ...

      def to_obscure_x(x, y):
        obscure(x)

      def to_obscure_y(x, y):
        obscure(y)

      def direct_issue():
        obscure(_test_source())

      def user_controlled():
        return _user_controlled()

      def indirect_issue():
        to_obscure_x(user_controlled(), 0)

      def non_issue():
        to_obscure_y(user_controlled(), 0)
        _test_sink(_test_source())
    |}
    ~expect:
      {
        expect =
          [
            outcome
              ~kind:`Function
              ~parameter_sinks:[{ name = "x"; sinks = [Sinks.NamedSink "Obscure"] }]
              "test_obscure.to_obscure_x";
            outcome
              ~kind:`Function
              ~parameter_sinks:[{ name = "y"; sinks = [Sinks.NamedSink "Obscure"] }]
              "test_obscure.to_obscure_y";
            outcome
              ~kind:`Function
              ~parameter_sinks:[{ name = "x"; sinks = [Sinks.NamedSink "Obscure"] }]
              "test_obscure.obscure";
            outcome
              ~kind:`Function
              ~errors:
                [
                  {
                    code = 9001;
                    pattern =
                      ".*Obscure flow.*Data from \\[Test\\] source(s) may reach an obscure model.*";
                  };
                ]
              "test_obscure.direct_issue";
            outcome
              ~kind:`Function
              ~errors:
                [
                  {
                    code = 9001;
                    pattern =
                      ".*Obscure flow.*Data from \\[UserControlled\\] source(s) may reach an \
                       obscure model.*";
                  };
                ]
              "test_obscure.indirect_issue";
            outcome ~kind:`Function ~errors:[] "test_obscure.non_issue";
          ];
        iterations = 2;
      }


let test_type context =
  assert_fixpoint
    ~context
    ~missing_flows:Configuration.MissingFlowKind.Type
    ~handle:"test_type.py"
    {|
      from builtins import _test_source, _test_sink, _user_controlled

      def to_unknown_callee_x(x, y, f):
        f(x)

      def to_unknown_callee_y(x, y, f):
        f(y)

      def direct_issue(f):
        f(_test_source())

      def user_controlled():
        return _user_controlled()

      def indirect_issue(f):
        to_unknown_callee_x(user_controlled(), 0, f)

      def non_issue(f):
        to_unknown_callee_y(user_controlled(), 0, f)
        _test_sink(_test_source())
    |}
    ~expect:
      {
        expect =
          [
            outcome
              ~kind:`Function
              ~parameter_sinks:[{ name = "x"; sinks = [Sinks.NamedSink "UnknownCallee"] }]
              "test_type.to_unknown_callee_x";
            outcome
              ~kind:`Function
              ~parameter_sinks:[{ name = "y"; sinks = [Sinks.NamedSink "UnknownCallee"] }]
              "test_type.to_unknown_callee_y";
            outcome
              ~kind:`Function
              ~errors:
                [
                  {
                    code = 9002;
                    pattern =
                      ".*Unknown callee flow.*Data from \\[Test\\] source(s) may flow to an \
                       unknown callee.*";
                  };
                ]
              "test_type.direct_issue";
            outcome
              ~kind:`Function
              ~errors:
                [
                  {
                    code = 9002;
                    pattern =
                      ".*Unknown callee flow.*Data from \\[UserControlled\\] source(s) may flow to \
                       an unknown callee.*";
                  };
                ]
              "test_type.indirect_issue";
            outcome ~kind:`Function ~errors:[] "test_type.non_issue";
          ];
        iterations = 2;
      }


let () = "missingFlows" >::: ["obscure" >:: test_obscure; "type" >:: test_type] |> Test.run
