(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Ast
open Expression
open Pyre
open Taint
open AccessPath
open Domains
open Model
open Result

open Test
open Interprocedural

module Parallel = Hack_parallel.Std


type parameter_taint = {
  position: int;
  sinks: Taint.Sinks.t list;
}


type model_expectation = {
  define_name: string;
  returns: Sources.t list;
  taint_sink_parameters: parameter_taint list;
  tito_parameters: int list;
}


type expect_fixpoint = {
  expect_models: model_expectation list;
  iterations: int;
  flow_errors: Error.t list
}


let create_call_graph ?(test_file = "test_file") source =
  let handle = File.Handle.create test_file in
  let source =
    Test.parse source
    |> Preprocessing.preprocess
  in
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


let create_model { define_name; returns; taint_sink_parameters; tito_parameters } =
  let expect_source_taint model source =
    let forward =
      let source_taint =
        ForwardState.assign_weak
          ~root:Root.LocalResult
          ~path:[]
          (ForwardTaint.singleton source
           |> ForwardState.make_leaf)
          model.forward.source_taint
      in
      Taint.Result.Forward.{ source_taint }
    in
    { model with forward }
  in
  let expect_sink_taint model { position; sinks } =
    let taint_sink_parameters model taint_sink_kind =
      let backward =
        let sink_taint =
          BackwardState.assign_weak
            ~root:(Root.Parameter { position })
            ~path:[]
            (BackwardTaint.singleton taint_sink_kind
             |> BackwardState.make_leaf)
            model.backward.sink_taint
        in
        { model.backward with sink_taint }
      in
      { model with backward }
    in
    List.fold sinks ~init:model ~f:taint_sink_parameters
  in
  let expect_taint_in_taint_out model position =
    let backward =
      let taint_in_taint_out =
        BackwardState.assign_weak
          ~root:(Root.Parameter { position })
          ~path:[]
          (BackwardTaint.singleton LocalReturn
           |> BackwardState.make_leaf)
          model.backward.taint_in_taint_out
      in
      { model.backward with taint_in_taint_out }
    in
    { model with backward }
  in
  let call_target = Callable.make_real (Access.create define_name) in
  Taint.Result.empty_model
  |> (fun model -> List.fold returns ~init:model ~f:expect_source_taint)
  |> (fun model -> List.fold taint_sink_parameters ~init:model ~f:expect_sink_taint)
  |> (fun model -> List.fold tito_parameters ~init:model ~f:expect_taint_in_taint_out)
  |> (fun model -> { call_target; model })


let assert_fixpoint ~source ~expect:{ iterations = expect_iterations; expect_models; _ } =
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
  let read_analysis_model { define_name; _ } =
    let call_target = Callable.make_real (Access.create define_name) in
    Fixpoint.get_model call_target
    >>= Result.get_model Taint.Result.kind
    >>| (fun model -> { call_target; model })
  in
  let models = List.filter_map expect_models ~f:read_analysis_model in
  let expect_models = List.map expect_models ~f:create_model in
  assert_bool "Callgraph is empty!" (Access.Map.length call_graph > 0);
  assert_equal expect_iterations iterations ~printer:Int.to_string;
  assert_equal expect_models models ~printer:(fun model -> Sexp.to_string [%message (model: Model.t list)])


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
      |}
    ~expect:{
      iterations = 3;
      flow_errors = [];
      expect_models = [
        {
          define_name = "match_flows";
          returns = [];
          taint_sink_parameters = [];
          tito_parameters = [];
        };
        {
          define_name = "qux";
          returns = [];
          taint_sink_parameters = [
            { position = 0; sinks = [Taint.Sinks.RemoteCodeExecution] }
          ];
          tito_parameters = [];
        };
        {
          define_name = "bad";
          returns = [];
          taint_sink_parameters = [
            { position = 0; sinks = [Taint.Sinks.RemoteCodeExecution] }
          ];
          tito_parameters = [];
        };
        {
          define_name = "bar";
          returns = [Sources.TestSource];
          taint_sink_parameters = [];
          tito_parameters = [];
        };
        {
          define_name = "some_source";
          returns = [Sources.TestSource];
          taint_sink_parameters = [];
          tito_parameters = [];
        }
      ]
    }


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
