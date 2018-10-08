(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Statement
open Taint
open AccessPath
open Domains
open Model
open Result

open Interprocedural


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


let assert_model ~model_source ~expect =
  let expect_source_taint model source =
    let forward =
      let source_taint =
        ForwardState.assign_weak
          ~root:Root.LocalResult
          ~path:[]
          (ForwardTaint.singleton source
           |> ForwardState.create_leaf)
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
             |> BackwardState.create_leaf)
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
           |> BackwardState.create_leaf)
          model.backward.taint_in_taint_out
      in
      { model.backward with taint_in_taint_out }
    in
    { model with backward }
  in
  let create_model { define_name; returns; taint_sink_parameters; tito_parameters } =
    let call_target = Callable.create_real (Access.create define_name) in
    Taint.Result.empty_model
    |> (fun model -> List.fold returns ~init:model ~f:expect_source_taint)
    |> (fun model -> List.fold taint_sink_parameters ~init:model ~f:expect_sink_taint)
    |> (fun model -> List.fold tito_parameters ~init:model ~f:expect_taint_in_taint_out)
    |> (fun model -> { call_target; model })
  in
  let expect_models = List.map expect ~f:create_model in
  let models =
    Test.trim_extra_indentation model_source
    |> (fun model_source -> Model.create ~model_source)
    |> Or_error.ok_exn
  in
  assert_equal
    ~printer:(fun models -> Sexp.to_string [%message (models: Model.t list)])
    expect_models
    models


let test_source_models _ =
  assert_model
    ~model_source:"def taint() -> TaintSource[Test]: ..."
    ~expect:[
      {
        define_name = "taint";
        returns = [Sources.Test];
        taint_sink_parameters = [];
        tito_parameters = [];
      };
    ]


let test_sink_models _ =
  assert_model
    ~model_source:
      {|
        def sink(parameter: TaintSink[Test]):
          ...
      |}
    ~expect:[
      {
        define_name = "sink";
        returns = [];
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] }
        ];
        tito_parameters = []
      }
    ];

  assert_model
    ~model_source:"def sink(parameter0, parameter1: TaintSink[Test]): ..."
    ~expect:[
      {
        define_name = "sink";
        returns = [];
        taint_sink_parameters = [
          { position = 0; sinks = [] };
          { position = 1; sinks = [Taint.Sinks.Test] }
        ];
        tito_parameters = []
      };
    ];

  assert_model
    ~model_source:"def sink(parameter0: TaintSink[Test], parameter1: TaintSink[Test]): ..."
    ~expect:[
      {
        define_name = "sink";
        returns = [];
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
          { position = 1; sinks = [Taint.Sinks.Test] }
        ];
        tito_parameters = []
      };
    ];

  assert_model
    ~model_source:"def sink(parameter0: TaintSink[Test], parameter1: TaintSink[Test]): ..."
    ~expect:[
      {
        define_name = "sink";
        returns = [];
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Test] };
          { position = 1; sinks = [Taint.Sinks.Test] }
        ];
        tito_parameters = []
      };
    ];

  assert_model
    ~model_source:"def thrift(parameter0: TaintSink[Thrift]) -> TaintSource[Thrift]: ..."
    ~expect:[
      {
        define_name = "thrift";
        returns = [Taint.Sources.Thrift];
        taint_sink_parameters = [
          { position = 0; sinks = [Taint.Sinks.Thrift] };
        ];
        tito_parameters = []
      };
    ]


let test_taint_in_taint_out_models _ =
  assert_model
    ~model_source:"def tito(parameter: TaintInTaintOut[LocalReturn]): ..."
    ~expect:[
      {
        define_name = "tito";
        returns = [];
        taint_sink_parameters = [];
        tito_parameters = [0]
      };
    ]


let test_invalid_models _ =
  let assert_invalid_model ~model_source ~expect =
    let error_message =
      match Model.create ~model_source with
      | Error error -> Base.Error.to_string_hum error
      | _ -> failwith "Invalid model should result in error"
    in
    assert_equal ~printer:ident expect error_message
  in

  assert_invalid_model
    ~model_source:"def sink(parameter: TaintSink[Unsupported]): ..."
    ~expect:"Unsupported taint sink Unsupported";

  assert_invalid_model
    ~model_source:"def sink(parameter: TaintSink[UserControlled]): ..."
    ~expect:"Unsupported taint sink UserControlled";

  assert_invalid_model
    ~model_source:"def source() -> TaintSource[Invalid]: ..."
    ~expect:"Unsupported taint source Invalid";

  assert_invalid_model
    ~model_source:"def source() -> TaintInTaintOut[Test]: ..."
    ~expect:{|"Unrecognized taint direction in return annotation: TaintInTaintOut"|};

  assert_invalid_model
    ~model_source:"def sink(parameter: InvalidTaintDirection[Test]): ..."
    ~expect:{|"Unrecognized taint direction in parameter annotation InvalidTaintDirection"|}


let () =
  "taint_model">:::[
    "source_models">::test_source_models;
    "sink_models">::test_sink_models;
    "taint_in_taint_out_models">::test_taint_in_taint_out_models;
    "invalid_models">::test_invalid_models;
  ]
  |> Test.run
