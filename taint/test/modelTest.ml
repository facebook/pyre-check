(* Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Pyre
open Core
open OUnit2
open Taint
open TestHelper
module Callable = Interprocedural.Callable

let assert_model ?source ~model_source ~expect () =
  let source =
    match source with
    | None -> model_source
    | Some source -> source
  in
  let resolution = Test.resolution ~sources:(Test.typeshed_stubs () @ [Test.parse source]) () in
  let configuration =
    TaintConfiguration.
      { sources = ["TestTest"]; sinks = ["TestSink"]; features = ["special"]; rules = [] }
  in
  let models =
    let source = Test.trim_extra_indentation model_source in
    Model.parse ~resolution ~source ~configuration Callable.Map.empty
  in
  let get_model callable =
    let message = Format.asprintf "Model %a missing" Interprocedural.Callable.pp callable in
    Callable.Map.find models callable |> Option.value_exn ?here:None ?error:None ~message, false
    (* obscure *)
  in
  List.iter ~f:(check_expectation ~get_model) expect


let test_source_models _ =
  assert_model
    ~model_source:"def taint() -> TaintSource[TestTest]: ..."
    ~expect:[outcome ~kind:`Function ~returns:[Sources.NamedSource "TestTest"] "taint"]
    ();
  assert_model
    ~model_source:"os.environ: TaintSource[TestTest] = ..."
    ~expect:[outcome ~kind:`Object ~returns:[Sources.NamedSource "TestTest"] "os.environ"]
    ();
  assert_model
    ~model_source:"django.http.Request.GET: TaintSource[TestTest] = ..."
    ~expect:
      [outcome ~kind:`Object ~returns:[Sources.NamedSource "TestTest"] "django.http.Request.GET"]
    ();
  assert_model
    ~model_source:"def taint() -> TaintSource[Test, UserControlled]: ..."
    ~expect:[outcome ~kind:`Function ~returns:[Sources.Test; Sources.UserControlled] "taint"]
    ();
  assert_model
    ~model_source:"os.environ: TaintSink[Test] = ..."
    ~expect:
      [ outcome
          ~kind:`Object
          ~sink_parameters:[{ name = "$global"; sinks = [Sinks.Test] }]
          "os.environ" ]
    ()


let test_sink_models _ =
  assert_model
    ~model_source:{|
        def sink(parameter: TaintSink[TestSink]):
          ...
      |}
    ~expect:
      [ outcome
          ~kind:`Function
          ~sink_parameters:[{ name = "parameter"; sinks = [Sinks.NamedSink "TestSink"] }]
          "sink" ]
    ();
  assert_model
    ~model_source:"def sink(parameter0, parameter1: TaintSink[Test]): ..."
    ~expect:
      [ outcome
          ~kind:`Function
          ~sink_parameters:[{ name = "parameter1"; sinks = [Sinks.Test] }]
          "sink" ]
    ();
  assert_model
    ~model_source:"def sink(parameter0: TaintSink[Test], parameter1: TaintSink[Test]): ..."
    ~expect:
      [ outcome
          ~kind:`Function
          ~sink_parameters:
            [ { name = "parameter0"; sinks = [Sinks.Test] };
              { name = "parameter1"; sinks = [Sinks.Test] } ]
          "sink" ]
    ();
  assert_model
    ~model_source:"def sink(parameter0: TaintSink[Test], parameter1: TaintSink[Test]): ..."
    ~expect:
      [ outcome
          ~kind:`Function
          ~sink_parameters:
            [ { name = "parameter0"; sinks = [Sinks.Test] };
              { name = "parameter1"; sinks = [Sinks.Test] } ]
          "sink" ]
    ();
  assert_model
    ~model_source:"def both(parameter0: TaintSink[Demo]) -> TaintSource[Demo]: ..."
    ~expect:
      [ outcome
          ~kind:`Function
          ~returns:[Sources.Demo]
          ~sink_parameters:[{ name = "parameter0"; sinks = [Sinks.Demo] }]
          "both" ]
    ();
  assert_model
    ~model_source:"def xss(parameter: TaintSink[XSS]): ..."
    ~expect:
      [outcome ~kind:`Function ~sink_parameters:[{ name = "parameter"; sinks = [Sinks.XSS] }] "xss"]
    ();
  assert_model
    ~model_source:"def multiple(parameter: TaintSink[XSS, Demo]): ..."
    ~expect:
      [ outcome
          ~kind:`Function
          ~sink_parameters:[{ name = "parameter"; sinks = [Sinks.Demo; Sinks.XSS] }]
          "multiple" ]
    ()


let test_class_models _ =
  assert_model
    ~source:
      {|
        class Sink:
          def Sink.method(parameter): ...
          def Sink.method_with_multiple_parameters(first, second): ...
      |}
    ~model_source:"class Sink(TaintSink[TestSink]): ..."
    ~expect:
      [ outcome
          ~kind:`Method
          ~sink_parameters:[{ name = "parameter"; sinks = [Sinks.NamedSink "TestSink"] }]
          "Sink.method";
        outcome
          ~kind:`Method
          ~sink_parameters:
            [ { name = "first"; sinks = [Sinks.NamedSink "TestSink"] };
              { name = "second"; sinks = [Sinks.NamedSink "TestSink"] } ]
          "Sink.method_with_multiple_parameters" ]
    ();
  assert_model
    ~source:
      {|
        class Source:
          def Source.method(parameter): ...
          def Source.method_with_multiple_parameters(first, second): ...
          Source.attribute = ...
      |}
    ~model_source:{|
        class Source(TaintSource[UserControlled]): ...
      |}
    ~expect:
      [ outcome ~kind:`Method ~returns:[Sources.UserControlled] "Source.method";
        outcome
          ~kind:`Method
          ~returns:[Sources.UserControlled]
          "Source.method_with_multiple_parameters" ]
    ();
  assert_model
    ~source:
      {|
        class AnnotatedSink:
          def AnnotatedSink.method(parameter: int) -> None: ...
      |}
    ~model_source:"class AnnotatedSink(TaintSink[TestSink]): ..."
    ~expect:
      [ outcome
          ~kind:`Method
          ~sink_parameters:[{ name = "parameter"; sinks = [Sinks.NamedSink "TestSink"] }]
          "AnnotatedSink.method" ]
    ();
  assert_model
    ~source:
      {|
         class AnnotatedSource:
          def AnnotatedSource.method(parameter: int) -> None: ...
      |}
    ~model_source:"class AnnotatedSource(TaintSource[UserControlled]): ..."
    ~expect:[outcome ~kind:`Method ~returns:[Sources.UserControlled] "AnnotatedSource.method"]
    ()


let test_taint_in_taint_out_models _ =
  assert_model
    ~model_source:"def tito(parameter: TaintInTaintOut): ..."
    ~expect:[outcome ~kind:`Function ~tito_parameters:["parameter"] "tito"]
    ()


let test_taint_in_taint_out_models_alternate _ =
  assert_model
    ~model_source:"def tito(parameter: TaintInTaintOut[LocalReturn]): ..."
    ~expect:[outcome ~kind:`Function ~tito_parameters:["parameter"] "tito"]
    ()


let test_taint_in_taint_out_update_models _ =
  assert_model
    ~model_source:"def update(self, arg1: TaintInTaintOut[Updates[self]]): ..."
    ~expect:[outcome ~kind:`Function ~tito_parameters:["arg1 updates parameter 0"] "update"]
    ();
  assert_model
    ~model_source:"def update(self, arg1, arg2: TaintInTaintOut[Updates[self, arg1]]): ..."
    ~expect:
      [ outcome
          ~kind:`Function
          ~tito_parameters:["arg2 updates parameter 0"; "arg2 updates parameter 1"]
          "update" ]
    ();
  assert_model
    ~model_source:"def update(self: TaintInTaintOut[LocalReturn, Updates[arg1]], arg1): ..."
    ~expect:[outcome ~kind:`Function ~tito_parameters:["self"; "self updates parameter 1"] "update"]
    ()


let test_union_models _ =
  assert_model
    ~model_source:"def both(parameter: Union[TaintInTaintOut, TaintSink[XSS]]): ..."
    ~expect:
      [ outcome
          ~kind:`Function
          ~sink_parameters:[{ name = "parameter"; sinks = [Sinks.XSS] }]
          ~tito_parameters:["parameter"]
          "both" ]
    ()


let test_source_breadcrumbs _ =
  assert_model
    ~model_source:"def source() -> TaintSource[Test, Via[special]]: ..."
    ~expect:[outcome ~kind:`Function ~returns:[Sources.Test] "source"]
    ()


let test_sink_breadcrumbs _ =
  assert_model
    ~model_source:"def sink(parameter: TaintSink[Test, Via[special]]): ..."
    ~expect:
      [ outcome
          ~kind:`Function
          ~sink_parameters:[{ name = "parameter"; sinks = [Sinks.Test] }]
          "sink" ]
    ()


let test_tito_breadcrumbs _ =
  assert_model
    ~model_source:"def tito(parameter: TaintInTaintOut[Via[special]]): ..."
    ~expect:[outcome ~kind:`Function ~tito_parameters:["parameter"] "tito"]
    ()


let test_attach_features _ =
  assert_model
    ~model_source:"def source() -> AttachToSource[Via[special]]: ..."
    ~expect:[outcome ~kind:`Function ~returns:[Sources.Attach] "source"]
    ();
  assert_model
    ~model_source:"def sink(arg: AttachToSink[Via[special]]): ..."
    ~expect:
      [outcome ~kind:`Function ~sink_parameters:[{ name = "arg"; sinks = [Sinks.Attach] }] "sink"]
    ();
  assert_model
    ~model_source:"def tito(arg: AttachToTito[Via[special]]): ..."
    ~expect:[outcome ~kind:`Function ~tito_parameters:["arg"] "tito"]
    ()


let test_invalid_models _ =
  let assert_invalid_model ?path ~model_source ~expect () =
    let resolution =
      Test.resolution
        ~sources:
          [ Test.parse
              {|
              unannotated_global = source()
              def sink(parameter) -> None: pass
              def sink_with_optional(parameter, firstOptional=1, secondOptional=2) -> None: pass
              def source() -> None: pass
              def function_with_args(normal_arg, __anonymous_arg, *args) -> None: pass
              def function_with_kwargs(normal_arg, **kwargs) -> None: pass
              def anonymous_only(__arg1, __arg2, __arg3) -> None: pass
              def anonymous_with_optional(__arg1, __arg2, __arg3=2) -> None: pass
              class C:
                unannotated_class_variable = source()
            |}
            |> Preprocessing.preprocess ]
        ()
    in
    let configuration =
      TaintConfiguration.
        {
          sources = ["A"; "B"];
          sinks = ["X"; "Y"];
          features = ["featureA"; "featureB"];
          rules = [];
        }
    in
    let error_message =
      let path = path >>| Path.create_absolute ~follow_symbolic_links:false in
      try
        Model.parse
          ~resolution
          ~configuration
          ?path
          ~source:(Test.trim_extra_indentation model_source)
          Callable.Map.empty
        |> ignore;
        "no failure"
      with
      | Failure message
      | Model.InvalidModel message ->
          message
    in
    assert_equal ~printer:ident expect error_message
  in
  let assert_valid_model ~model_source =
    assert_invalid_model ~model_source ~expect:"no failure" ()
  in
  assert_invalid_model
    ~model_source:"def sink(parameter: TaintSink[X, Unsupported]) -> TaintSource[A]: ..."
    ~expect:"Invalid model for `sink`: Unsupported taint sink `Unsupported`"
    ();
  assert_invalid_model
    ~model_source:"def sink(parameter: TaintSink[UserControlled]): ..."
    ~expect:"Invalid model for `sink`: Unsupported taint sink `UserControlled`"
    ();
  assert_invalid_model
    ~model_source:"def sink(parameter: SkipAnalysis): ..."
    ~expect:"Invalid model for `sink`: SkipAnalysis annotation must be in return position"
    ();
  assert_invalid_model
    ~model_source:"def sink(parameter: TaintSink[X, Y, LocalReturn]): ..."
    ~expect:"Invalid model for `sink`: Invalid TaintSink annotation `LocalReturn`"
    ();
  assert_invalid_model
    ~model_source:"def source() -> TaintSource[Invalid]: ..."
    ~expect:"Invalid model for `source`: Unsupported taint source `Invalid`"
    ();
  assert_invalid_model
    ~model_source:"def source() -> TaintInTaintOut: ..."
    ~expect:"Invalid model for `source`: Invalid return annotation: TaintInTaintOut"
    ();
  assert_invalid_model
    ~model_source:"def sink(parameter: TaintInTaintOut[Test]): ..."
    ~expect:"Invalid model for `sink`: Invalid TaintInTaintOut annotation `Test`"
    ();
  assert_invalid_model
    ~model_source:"def sink(parameter: InvalidTaintDirection[Test]): ..."
    ~expect:"Invalid model for `sink`: Unrecognized taint annotation `InvalidTaintDirection[Test]`"
    ();
  assert_invalid_model
    ~model_source:"def not_in_the_environment(parameter: InvalidTaintDirection[Test]): ..."
    ~expect:
      "Invalid model for `not_in_the_environment`: Modeled entity is not part of the environment!"
    ();
  assert_invalid_model
    ~model_source:"def sink(): ..."
    ~expect:
      "Invalid model for `sink`: Model signature parameters do not match implementation `def \
       sink(parameter: unknown) -> None: ...`. Reason(s): missing named parameters: `parameter`."
    ();
  assert_invalid_model
    ~model_source:"def sink_with_optional(): ..."
    ~expect:
      "Invalid model for `sink_with_optional`: Model signature parameters do not match \
       implementation `def sink_with_optional(parameter: unknown, firstOptional: unknown = ..., \
       secondOptional: unknown = ...) -> None: ...`. Reason(s): missing named parameters: \
       `parameter`."
    ();
  assert_valid_model ~model_source:"def sink_with_optional(parameter): ...";
  assert_valid_model ~model_source:"def sink_with_optional(parameter, firstOptional): ...";
  assert_valid_model
    ~model_source:"def sink_with_optional(parameter, firstOptional, secondOptional): ...";
  assert_invalid_model
    ~model_source:
      "def sink_with_optional(parameter, firstOptional, secondOptional, thirdOptional): ..."
    ~expect:
      "Invalid model for `sink_with_optional`: Model signature parameters do not match \
       implementation `def sink_with_optional(parameter: unknown, firstOptional: unknown = ..., \
       secondOptional: unknown = ...) -> None: ...`. Reason(s): unexpected named parameter: \
       `thirdOptional`."
    ();
  assert_invalid_model
    ~model_source:"def sink_with_optional(parameter, firstBad, secondBad): ..."
    ~expect:
      "Invalid model for `sink_with_optional`: Model signature parameters do not match \
       implementation `def sink_with_optional(parameter: unknown, firstOptional: unknown = ..., \
       secondOptional: unknown = ...) -> None: ...`. Reason(s): unexpected named parameter: \
       `firstBad`; unexpected named parameter: `secondBad`."
    ();
  assert_invalid_model
    ~model_source:"def sink_with_optional(parameter, *args): ..."
    ~expect:
      "Invalid model for `sink_with_optional`: Model signature parameters do not match \
       implementation `def sink_with_optional(parameter: unknown, firstOptional: unknown = ..., \
       secondOptional: unknown = ...) -> None: ...`. Reason(s): unexpected star parameter."
    ();
  assert_invalid_model
    ~model_source:"def sink_with_optional(parameter, **kwargs): ..."
    ~expect:
      "Invalid model for `sink_with_optional`: Model signature parameters do not match \
       implementation `def sink_with_optional(parameter: unknown, firstOptional: unknown = ..., \
       secondOptional: unknown = ...) -> None: ...`. Reason(s): unexpected star star parameter."
    ();
  assert_invalid_model
    ~model_source:"def sink_with_optional(__parameter): ..."
    ~expect:
      "Invalid model for `sink_with_optional`: Model signature parameters do not match \
       implementation `def sink_with_optional(parameter: unknown, firstOptional: unknown = ..., \
       secondOptional: unknown = ...) -> None: ...`. Reason(s): missing named parameters: \
       `parameter`; unexpected anonymous parameter: `__parameter`."
    ();
  assert_valid_model
    ~model_source:"def function_with_args(normal_arg, __random_name, named_arg, *args): ...";
  assert_valid_model ~model_source:"def function_with_args(normal_arg, __random_name, *args): ...";
  assert_valid_model
    ~model_source:"def function_with_args(normal_arg, __random_name, __random_name_2, *args): ...";
  assert_valid_model ~model_source:"def function_with_kwargs(normal_arg, **kwargs): ...";
  assert_valid_model ~model_source:"def function_with_kwargs(normal_arg, crazy_arg, **kwargs): ...";
  assert_valid_model ~model_source:"def anonymous_only(__a1, __a2, __a3): ...";
  assert_valid_model ~model_source:"def anonymous_with_optional(__a1, __a2): ...";
  assert_valid_model ~model_source:"def anonymous_with_optional(__a1, __a2, __a3=...): ...";
  assert_invalid_model
    ~model_source:"def sink(parameter: Any): ..."
    ~expect:"Invalid model for `sink`: Unrecognized taint annotation `Any`"
    ();
  assert_invalid_model
    ~path:"broken_model.pysa"
    ~model_source:"def sink(parameter: Any): ..."
    ~expect:
      "Invalid model for `sink` defined in `broken_model.pysa`: Unrecognized taint annotation `Any`"
    ();
  assert_invalid_model
    ~model_source:"def sink(parameter: TaintSink[Test, Via[bad_feature]]): ..."
    ~expect:"Invalid model for `sink`: Unrecognized Via annotation `bad_feature`"
    ();
  assert_invalid_model
    ~model_source:"def sink(parameter: TaintSink[Updates[self]]): ..."
    ~expect:"Invalid model for `sink`: No such parameter `self`"
    ();
  assert_valid_model ~model_source:"unannotated_global: TaintSink[Test]";
  assert_invalid_model
    ~model_source:"missing_global: TaintSink[Test]"
    ~expect:"Invalid model for `missing_global`: Modeled entity is not part of the environment!"
    ();
  assert_valid_model ~model_source:"C.unannotated_class_variable: TaintSink[Test]";
  assert_invalid_model
    ~model_source:"C.missing: TaintSink[Test]"
    ~expect:"Invalid model for `C.missing`: Modeled entity is not part of the environment!"
    ();
  assert_invalid_model
    ~model_source:
      {|
      class ClassSinkWithMethod(TaintSink[TestSink]):
          def method(self): ...
      |}
    ~expect:"Class models must have a body of `...`."
    ();

  (* Attach syntax. *)
  assert_invalid_model
    ~model_source:"def sink(parameter: AttachToSink): ..."
    ~expect:"Invalid model for `sink`: Unrecognized taint annotation `AttachToSink`"
    ();
  assert_invalid_model
    ~model_source:"def sink(parameter: AttachToSink[feature]): ..."
    ~expect:
      "Invalid model for `sink`: All parameters to `AttachToSink` must be of the form \
       `Via[feature]`."
    ();
  assert_invalid_model
    ~model_source:"def sink(parameter: AttachToTito[feature]): ..."
    ~expect:
      "Invalid model for `sink`: All parameters to `AttachToTito` must be of the form \
       `Via[feature]`."
    ();
  assert_invalid_model
    ~model_source:"def source() -> AttachToSource[feature]: ..."
    ~expect:
      "Invalid model for `source`: All parameters to `AttachToSource` must be of the form \
       `Via[feature]`."
    ();

  (* Multiple features. *)
  assert_valid_model
    ~model_source:"def sink(parameter: AttachToSink[Via[featureA, featureB]]): ...";

  (* Default values must be `...`. *)
  assert_invalid_model
    ~model_source:"def sink(parameter = TaintSink[Test]): ..."
    ~expect:
      "Invalid model for `sink`: Default values of parameters must be `...`. Did you mean to \
       write `parameter: TaintSink[Test]`?"
    ();
  assert_invalid_model
    ~model_source:"def sink(parameter = 1): ..."
    ~expect:
      "Invalid model for `sink`: Default values of parameters must be `...`. Did you mean to \
       write `parameter: 1`?"
    ()


let () =
  "taint_model"
  >::: [ "attach_features" >:: test_attach_features;
         "source_models" >:: test_source_models;
         "sink_models" >:: test_sink_models;
         "class_models" >:: test_class_models;
         "taint_in_taint_out_models" >:: test_taint_in_taint_out_models;
         "taint_in_taint_out_models_alternate" >:: test_taint_in_taint_out_models_alternate;
         "taint_in_taint_out_update_models" >:: test_taint_in_taint_out_update_models;
         "taint_union_models" >:: test_union_models;
         "test_source_breadcrumbs" >:: test_source_breadcrumbs;
         "test_sink_breadcrumbs" >:: test_sink_breadcrumbs;
         "test_tito_breadcrumbs" >:: test_tito_breadcrumbs;
         "invalid_models" >:: test_invalid_models ]
  |> Test.run
