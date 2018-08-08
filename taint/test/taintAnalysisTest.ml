(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Ast
open Expression

open Test
open Interprocedural

module Parallel = Hack_parallel.Std


let create_call_graph ?(test_file = "test_file") source =
  let handle = File.Handle.create test_file in
  let source = Test.parse source in
  let () = AstSharedMemory.add_source handle source in
  let environment = Test.environment () in
  Service.Environment.populate environment [source];
  TypeCheck.check configuration environment source |> ignore;
  let call_graph =
    Service.Analysis.record_and_merge_call_graph environment CallGraph.empty handle source
  in
  let () = Service.Analysis.record_overrides environment source in
  let callables =
    Service.Analysis.record_path_of_definitions handle source
    |> List.map ~f:Callable.make
  in
  call_graph, callables


let test_fixpoint _ =
  let source =
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
    |}
  in
  let call_graph, all_callables = create_call_graph source in
  let caller_map = CallGraph.reverse call_graph in
  let analyses = [Taint.Analysis.abstract_kind] in
  let iterations =
    Analysis.compute_fixpoint
      ~workers:None
      ~analyses
      ~caller_map
      ~all_callables
      Fixpoint.Epoch.initial
  in
  assert_bool "Callgraph is empty!" (Access.Map.length call_graph > 0);
  assert_equal 3 iterations ~printer:Int.to_string


let () =
  let model_source =
    {|
      def __testSink(arg: TaintSink[RemoteCodeExecution]): ...
      def __testSource() -> TaintSource[TestSource]: ...
    |}
    |> Test.trim_extra_indentation
  in
  Service.Analysis.add_models ~model_source;
  "taint">:::[
    "fixpoint">::test_fixpoint;
  ]
  |> run_test_tt_main
