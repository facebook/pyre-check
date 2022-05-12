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
  let scheduler = Test.mock_scheduler () in
  let taint_configuration =
    TaintConfiguration.apply_missing_flows TaintConfiguration.default missing_flows
  in
  let {
    callables_to_analyze;
    callgraph;
    environment;
    overrides;
    initial_models;
    initial_callables;
    stubs;
    override_targets;
    _;
  }
    =
    initialize
      ?models_source
      ~find_missing_flows:(TaintConfiguration.missing_flows_kind_to_string missing_flows)
      ~taint_configuration
      ~handle
      ~context
      source
  in
  let dependency_graph =
    DependencyGraph.from_callgraph callgraph
    |> DependencyGraph.union overrides
    |> DependencyGraph.reverse
  in
  let fixpoint_state =
    Fixpoint.compute
      ~scheduler
      ~type_environment:environment
      ~context:{ Fixpoint.Analysis.environment }
      ~dependency_graph
      ~initial_callables
      ~stubs
      ~filtered_callables:Target.Set.empty
      ~override_targets
      ~callables_to_analyze
      ~initial_models
      ~max_iterations:100
      ~epoch:Fixpoint.Epoch.initial
  in
  assert_bool "Callgraph is empty!" (Target.Map.length callgraph > 0);
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


let test_obscure context =
  assert_fixpoint
    ~context
    ~missing_flows:TaintConfiguration.Obscure
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
              ~sink_parameters:[{ name = "x"; sinks = [Sinks.NamedSink "Obscure"] }]
              "test_obscure.to_obscure_x";
            outcome
              ~kind:`Function
              ~sink_parameters:[{ name = "y"; sinks = [Sinks.NamedSink "Obscure"] }]
              "test_obscure.to_obscure_y";
            outcome
              ~kind:`Function
              ~sink_parameters:[{ name = "x"; sinks = [Sinks.NamedSink "Obscure"] }]
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
    ~missing_flows:TaintConfiguration.Type
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
              ~sink_parameters:[{ name = "x"; sinks = [Sinks.NamedSink "UnknownCallee"] }]
              "test_type.to_unknown_callee_x";
            outcome
              ~kind:`Function
              ~sink_parameters:[{ name = "y"; sinks = [Sinks.NamedSink "UnknownCallee"] }]
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
