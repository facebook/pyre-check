(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Pyre
open Statement
open Taint
open AccessPath
open Domains

open Interprocedural


let assert_source_model ~model_source ~call_target ~expect_taint =
  let () = Service.Analysis.add_models ~model_source in
  let expect_source_taint root =
    ForwardState.assign
      ~root
      ~path:[]
      (ForwardTaint.singleton TestSource
       |> ForwardState.make_leaf)
      ForwardState.empty
  in
  let expect_taint = expect_source_taint expect_taint in
  let call_target = Callable.make_real (Access.create call_target) in
  let taint_model = Fixpoint.get_model call_target >>= Result.get_model Taint.Result.kind in
  assert_equal
    ~printer:Taint.Result.show_call_model
    (Option.value_exn taint_model)
    {
      Taint.Result.empty_model with
      forward = { source_taint = expect_taint }
    }


let assert_sink_model ~model_source ~call_target ~expect_taint =
  let () = Service.Analysis.add_models ~model_source in
  let call_target = Callable.make_real (Access.create call_target) in
  let taint_model = Fixpoint.get_model call_target >>= Result.get_model Taint.Result.kind in
  let expect_parameter_taint =
    let assign_backward_taint position taint taint_sink_kind =
      BackwardState.assign
        ~root:(Root.Parameter { position })
        ~path:[]
        (BackwardTaint.singleton taint_sink_kind
         |> BackwardState.make_leaf)
        taint
    in
    let parameter_taint
        ({ Taint.Result.backward = { sink_taint; taint_in_taint_out}; _ } as taint)
        parameter =
      let backward =
        match parameter with
        | `SinkParameter position ->
            let sink_taint = assign_backward_taint position sink_taint TestSink in
            { taint.backward with sink_taint }
        | `TaintInTaintOutParameter position ->
            let taint_in_taint_out =
              assign_backward_taint
                position
                taint_in_taint_out LocalReturn
            in
            { taint.backward with taint_in_taint_out }
      in
      { taint with backward }
    in
    List.fold
      expect_taint
      ~init:Taint.Result.empty_model
      ~f:parameter_taint
  in
  assert_equal
    ~printer:Taint.Result.show_call_model
    expect_parameter_taint
    (Option.value_exn taint_model)


let test_source_models _ =
  assert_source_model
    ~model_source:"def taint() -> TaintSource[TestSource]: ..."
    ~call_target:"taint"
    ~expect_taint:Taint.AccessPath.Root.LocalResult


let test_sink_models _ =
  assert_sink_model
    ~model_source:"def sink(parameter: TaintSink[TestSink]): ..."
    ~call_target:"sink"
    ~expect_taint:[`SinkParameter 0];

  assert_sink_model
    ~model_source:"def sink(parameter0, parameter1: TaintSink[TestSink]): ..."
    ~call_target:"sink"
    ~expect_taint:[`SinkParameter 1];

  assert_sink_model
    ~model_source:"def sink(parameter0: TaintSink[TestSink], parameter1: TaintSink[TestSink]): ..."
    ~call_target:"sink"
    ~expect_taint:[`SinkParameter 0; `SinkParameter 1];

  let assert_not_tainted _ =
    try
      assert_sink_model
        ~model_source:"def sink(parameter0, parameter1: TaintSink[TestSink]): ..."
        ~call_target:"sink"
        ~expect_taint:[`SinkParameter 0]
    with
    | OUnitTest.OUnit_failure _ -> failwith "Parameter 0 should not be tainted"
  in
  assert_raises
    (Failure "Parameter 0 should not be tainted")
    assert_not_tainted;

  assert_sink_model
    ~model_source:"def tito(parameter: TaintInTaintOut[LocalReturn]): ..."
    ~call_target:"tito"
    ~expect_taint:[`TaintInTaintOutParameter 0]


let () =
  Service.Scheduler.mock () |> ignore;
  "taint_model">:::[
    "source_models">::test_source_models;
    "sink_models">::test_sink_models;
  ]
  |> run_test_tt_main
