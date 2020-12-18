(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
    ?models
    ~context
    ~missing_flows
    source
    ~expect:{ iterations = expect_iterations; expect }
  =
  let scheduler = Test.mock_scheduler () in
  let taint_configuration =
    TaintConfiguration.apply_missing_flows TaintConfiguration.default missing_flows
  in
  let { all_callables; callgraph; environment; overrides } =
    initialize ?models ~taint_configuration ~handle:"qualifier.py" ~context source
  in
  let dependencies =
    DependencyGraph.from_callgraph callgraph
    |> DependencyGraph.union overrides
    |> DependencyGraph.reverse
  in
  let analyses = [TaintAnalysis.abstract_kind] in
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


let test_obscure context =
  assert_fixpoint
    ~context
    ~missing_flows:TaintConfiguration.Obscure
    ~models:{|
      def qualifier.obscure(x): ...
    |}
    {|
      from builtins import __test_source, __test_sink, __user_controlled

      def obscure(x): ...

      def to_obscure_x(x, y):
        obscure(x)

      def to_obscure_y(x, y):
        obscure(y)

      def direct_issue():
        obscure(__test_source())

      def user_controlled():
        return __user_controlled()

      def indirect_issue():
        to_obscure_x(user_controlled(), 0)

      def non_issue():
        to_obscure_y(user_controlled(), 0)
        __test_sink(__test_source())
    |}
    ~expect:
      {
        expect =
          [
            outcome
              ~kind:`Function
              ~sink_parameters:[{ name = "x"; sinks = [Sinks.NamedSink "Obscure"] }]
              "qualifier.to_obscure_x";
            outcome
              ~kind:`Function
              ~sink_parameters:[{ name = "y"; sinks = [Sinks.NamedSink "Obscure"] }]
              "qualifier.to_obscure_y";
            outcome
              ~kind:`Function
              ~sink_parameters:[{ name = "x"; sinks = [Sinks.NamedSink "Obscure"] }]
              "qualifier.obscure";
            outcome
              ~kind:`Function
              ~errors:
                [
                  {
                    code = 6002;
                    pattern =
                      ".*Obscure flow.*Data from \\[Test\\] source(s) may reach an obscure model.*";
                  };
                ]
              "qualifier.direct_issue";
            outcome
              ~kind:`Function
              ~errors:
                [
                  {
                    code = 6002;
                    pattern =
                      ".*Obscure flow.*Data from \\[UserControlled\\] source(s) may reach an \
                       obscure model.*";
                  };
                ]
              "qualifier.indirect_issue";
            outcome ~kind:`Function ~errors:[] "qualifier.non_issue";
          ];
        iterations = 2;
      }


let () = ["obscure", test_obscure] |> TestHelper.run_with_taint_models ~name:"missingFlows"
