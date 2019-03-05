(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Taint
open Model

open TestHelper


let assert_model ~model_source ~expect =
  let resolution =
    Test.resolution
      ~sources:(Test.typeshed_stubs () @ [Test.parse model_source])
      ()
  in
  let models =
    Test.trim_extra_indentation model_source
    |> (fun model_source -> Model.create ~resolution ~model_source ())
    |> Or_error.ok_exn
  in
  let is_model callable { call_target; _ } =
    callable = call_target
  in
  let get_model callable =
    let message = Format.asprintf "Model %a missing" Interprocedural.Callable.pp callable in
    List.find ~f:(is_model callable) models
    |> Option.value_exn ?here:None ?error:None ~message
    |> (fun { model; _ } -> model)
  in
  List.iter ~f:(check_expectation ~get_model) expect


let test_source_models _ =
  assert_model
    ~model_source:"def taint() -> TaintSource[Test]: ..."
    ~expect:[
      {
        kind = `Function;
        define_name = "taint";
        returns = [Sources.Test];
        sink_parameters = [];
        tito_parameters = [];
        errors = [];
      };
    ];
  assert_model
    ~model_source:"os.environ: TaintSource[Test] = ..."
    ~expect:[
      {
        kind = `Object;
        define_name = "os.environ";
        returns = [Sources.Test];
        sink_parameters = [];
        tito_parameters = [];
        errors = [];
      };
    ];
  assert_model
    ~model_source:"django.http.Request.GET: TaintSource[Test] = ..."
    ~expect:[
      {
        kind = `Object;
        define_name = "django.http.Request.GET";
        returns = [Sources.Test];
        sink_parameters = [];
        tito_parameters = [];
        errors = [];
      };
    ];
  assert_model
    ~model_source:"def taint() -> TaintSource[Test, UserControlled]: ..."
    ~expect:[
      {
        kind = `Function;
        define_name = "taint";
        returns = [Sources.Test; Sources.UserControlled];
        sink_parameters = [];
        tito_parameters = [];
        errors = [];
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
        kind = `Function;
        define_name = "sink";
        returns = [];
        errors = [];
        sink_parameters = [
          { name = "parameter"; sinks = [Sinks.Test] }
        ];
        tito_parameters = []
      }
    ];

  assert_model
    ~model_source:"def sink(parameter0, parameter1: TaintSink[Test]): ..."
    ~expect:[
      {
        kind = `Function;
        define_name = "sink";
        returns = [];
        errors = [];
        sink_parameters = [
          { name = "parameter1"; sinks = [Sinks.Test] }
        ];
        tito_parameters = []
      };
    ];

  assert_model
    ~model_source:"def sink(parameter0: TaintSink[Test], parameter1: TaintSink[Test]): ..."
    ~expect:[
      {
        kind = `Function;
        define_name = "sink";
        returns = [];
        errors = [];
        sink_parameters = [
          { name = "parameter0"; sinks = [Sinks.Test] };
          { name = "parameter1"; sinks = [Sinks.Test] }
        ];
        tito_parameters = []
      };
    ];

  assert_model
    ~model_source:"def sink(parameter0: TaintSink[Test], parameter1: TaintSink[Test]): ..."
    ~expect:[
      {
        kind = `Function;
        define_name = "sink";
        returns = [];
        errors = [];
        sink_parameters = [
          { name = "parameter0"; sinks = [Sinks.Test] };
          { name = "parameter1"; sinks = [Sinks.Test] }
        ];
        tito_parameters = []
      };
    ];

  assert_model
    ~model_source:"def thrift(parameter0: TaintSink[Thrift]) -> TaintSource[Thrift]: ..."
    ~expect:[
      {
        kind = `Function;
        define_name = "thrift";
        returns = [Taint.Sources.Thrift];
        sink_parameters = [
          { name = "parameter0"; sinks = [Sinks.Thrift] };
        ];
        tito_parameters = [];
        errors = [];
      };
    ];

  assert_model
    ~model_source:"def xss(parameter: TaintSink[XSS]): ..."
    ~expect:[
      {
        kind = `Function;
        define_name = "xss";
        returns = [];
        sink_parameters = [
          { name = "parameter"; sinks = [Sinks.XSS] };
        ];
        tito_parameters = [];
        errors = [];
      };
    ];

  assert_model
    ~model_source:"def multiple(parameter: TaintSink[XSS, Thrift]): ..."
    ~expect:[
      {
        kind = `Function;
        define_name = "multiple";
        returns = [];
        sink_parameters = [
          { name = "parameter"; sinks = [Sinks.Thrift; Sinks.XSS] };
        ];
        tito_parameters = [];
        errors = [];
      };
    ]


let test_taint_in_taint_out_models _ =
  assert_model
    ~model_source:"def tito(parameter: TaintInTaintOut): ..."
    ~expect:[
      {
        kind = `Function;
        define_name = "tito";
        returns = [];
        errors = [];
        sink_parameters = [];
        tito_parameters = ["parameter"]
      };
    ]


let test_taint_in_taint_out_models_alternate _ =
  assert_model
    ~model_source:"def tito(parameter: TaintInTaintOut[LocalReturn]): ..."
    ~expect:[
      {
        kind = `Function;
        define_name = "tito";
        returns = [];
        errors = [];
        sink_parameters = [];
        tito_parameters = ["parameter"]
      };
    ]


let test_union_models _ =
  assert_model
    ~model_source:"def both(parameter: Union[TaintInTaintOut, TaintSink[XSS]]): ..."
    ~expect:[
      {
        kind = `Function;
        define_name = "both";
        returns = [];
        errors = [];
        sink_parameters = [
          { name = "parameter"; sinks = [Sinks.XSS] };
        ];
        tito_parameters = ["parameter"]
      };
    ]


let test_source_breadcrumbs _ =
  assert_model
    ~model_source:"def source() -> TaintSource[Test, Via[special]]: ..."
    ~expect:[
      {
        kind = `Function;
        define_name = "source";
        returns = [Sources.Test];
        errors = [];
        sink_parameters = [];
        tito_parameters = [];
      };
    ]


let test_sink_breadcrumbs _ =
  assert_model
    ~model_source:"def sink(parameter: TaintSink[Test, Via[special]]): ..."
    ~expect:[
      {
        kind = `Function;
        define_name = "sink";
        returns = [];
        errors = [];
        sink_parameters = [
          { name = "parameter"; sinks = [Sinks.Test] };
        ];
        tito_parameters = [];
      };
    ]


let test_tito_breadcrumbs _ =
  assert_model
    ~model_source:"def tito(parameter: TaintInTaintOut[Via[special]]): ..."
    ~expect:[
      {
        kind = `Function;
        define_name = "tito";
        returns = [];
        errors = [];
        sink_parameters = [];
        tito_parameters = ["parameter"];
      };
    ]


let test_invalid_models _ =
  let assert_invalid_model ~model_source ~expect =
    let resolution =
      Test.resolution
        ~sources:[
          Test.parse
            {|
              def sink(parameter) -> None: pass
              def source() -> None: pass
            |};
        ]
        ()
    in
    let error_message =
      match Model.create ~resolution ~model_source () with
      | Error error -> Base.Error.to_string_hum error
      | _ -> failwith "Invalid model should result in error"
    in
    assert_equal ~printer:ident expect error_message
  in

  assert_invalid_model
    ~model_source:"def sink(parameter: TaintSink[Unsupported]): ..."
    ~expect:"Unsupported taint sink `Unsupported`";

  assert_invalid_model
    ~model_source:"def sink(parameter: TaintSink[UserControlled]): ..."
    ~expect:"Unsupported taint sink `UserControlled`";

  assert_invalid_model
    ~model_source:"def sink(parameter: SkipAnalysis): ..."
    ~expect:{|"SkipAnalysis annotation must be in return position"|};

  assert_invalid_model
    ~model_source:"def sink(parameter: TaintSink[LocalReturn]): ..."
    ~expect:{|"Invalid TaintSink annotation `LocalReturn`"|};

  assert_invalid_model
    ~model_source:"def source() -> TaintSource[Invalid]: ..."
    ~expect:"Unsupported taint source `Invalid`";

  assert_invalid_model
    ~model_source:"def source() -> TaintInTaintOut: ..."
    ~expect:{|"Invalid return annotation: TaintInTaintOut"|};

  assert_invalid_model
    ~model_source:"def sink(parameter: TaintInTaintOut[Test]): ..."
    ~expect:{|"Invalid TaintInTaintOut annotation `Test`"|};

  assert_invalid_model
    ~model_source:"def sink(parameter: InvalidTaintDirection[Test]): ..."
    ~expect:{|"Unrecognized taint annotation `InvalidTaintDirection.__getitem__.(...)`"|};

  assert_invalid_model
    ~model_source:"def not_in_the_environment(parameter: InvalidTaintDirection[Test]): ..."
    ~expect:{|Modeled entity `not_in_the_environment` is not part of the environment!|};

  assert_invalid_model
    ~model_source:"def sink(): ..."
    ~expect:(
      "Model signature parameters for `sink` do not match implementation " ^
      "`typing.Callable(sink)[[Named(parameter, unknown)], None]`");

  assert_invalid_model
    ~model_source:"def sink(parameter: Any): ..."
    ~expect:{|"Unrecognized taint annotation `Any`"|}


let () =
  "taint_model">:::[
    "source_models">::test_source_models;
    "sink_models">::test_sink_models;
    "taint_in_taint_out_models">::test_taint_in_taint_out_models;
    "taint_union_models">::test_union_models;
    "test_source_breadcrumbs">::test_source_breadcrumbs;
    "test_sink_breadcrumbs">::test_sink_breadcrumbs;
    "test_tito_breadcrumbs">::test_tito_breadcrumbs;
    "invalid_models">::test_invalid_models;
  ]
  |> Test.run
