(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Pyre
open Core
open OUnit2
open Test
open TestHelper
module Target = Interprocedural.Target
open Taint

let set_up_environment ?source ?rules ~context ~model_source () =
  let source =
    match source with
    | None -> model_source
    | Some source -> source
  in
  let project = ScratchProject.setup ~context ["test.py", source] in
  let configuration =
    let rules =
      match rules with
      | Some rules -> rules
      | None -> []
    in
    let named name = { AnnotationParser.name; kind = Named } in
    Taint.TaintConfiguration.
      {
        empty with
        sources =
          [
            named "TestTest";
            named "UserControlled";
            named "Test";
            named "Demo";
            { AnnotationParser.name = "WithSubkind"; kind = Parametric };
          ];
        sinks =
          [
            named "TestSink";
            named "OtherSink";
            named "Test";
            named "Demo";
            named "XSS";
            { AnnotationParser.name = "TestSinkWithSubkind"; kind = Parametric };
          ];
        transforms = [TaintTransform.Named "TestTransform"; TaintTransform.Named "DemoTransform"];
        features = ["special"];
        partial_sink_labels = String.Map.Tree.of_alist_exn ["Test", ["a"; "b"]];
        rules;
      }
  in
  let source = Test.trim_extra_indentation model_source in
  let resolution =
    let global_resolution = ScratchProject.build_global_resolution project in
    TypeCheck.resolution global_resolution (module TypeCheck.DummyContext)
  in

  let rule_filter =
    match rules with
    | Some rules -> Some (List.map rules ~f:(fun { Taint.TaintConfiguration.Rule.code; _ } -> code))
    | None -> None
  in
  let ({ ModelParser.errors; skip_overrides; _ } as parse_result) =
    ModelParser.parse
      ~resolution
      ?rule_filter
      ~source
      ~configuration
      ~callables:None
      ~stubs:(Target.HashSet.create ())
      ()
  in
  assert_bool
    (Format.sprintf
       "Models have parsing errors: %s"
       (List.to_string errors ~f:ModelVerificationError.display))
    (List.is_empty errors);

  let environment = ScratchProject.type_environment project in
  parse_result, environment, skip_overrides


let assert_model ?source ?rules ?expected_skipped_overrides ~context ~model_source ~expect () =
  let { ModelParser.models; _ }, environment, skip_overrides =
    set_up_environment ?source ?rules ~context ~model_source ()
  in
  begin
    match expected_skipped_overrides with
    | Some expected ->
        let expected_set = List.map expected ~f:Ast.Reference.create |> Ast.Reference.Set.of_list in
        assert_equal ~cmp:Ast.Reference.Set.equal expected_set skip_overrides
    | None -> ()
  end;
  let get_model = Registry.get models in
  let get_errors _ = [] in
  List.iter ~f:(check_expectation ~environment ~get_model ~get_errors) expect


let assert_invalid_model ?path ?source ?(sources = []) ~context ~model_source ~expect () =
  let source =
    match source with
    | Some source -> source
    | None ->
        {|
              from typing import overload, Union

              unannotated_global = source()
              def sink(parameter) -> None: pass
              def sink_with_optional(parameter, firstOptional=1, secondOptional=2) -> None: pass
              def source() -> None: pass
              def taint(x, y) -> None: pass
              def partial_sink(x, y) -> None: pass
              def function_with_args(normal_arg, __anonymous_arg, *args) -> None: pass
              def function_with_kwargs(normal_arg, **kwargs) -> None: pass
              def anonymous_only(__arg1, __arg2, __arg3) -> None: pass
              def anonymous_with_optional(__arg1, __arg2, __arg3=2) -> None: pass
              class C:
                unannotated_class_variable = source()
              def function_with_overloads(__key: str) -> Union[int, str]: ...
              @overload
              def function_with_overloads(__key: str, firstNamed: int) -> int: ...
              @overload
              def function_with_overloads(__key: str, secondNamed: str) -> str: ...
              def function_with_multiple_positions(a: int, b: int, c: int) -> Union[int, str]: ...
              @overload
              def function_with_multiple_positions(a: int, c: int) -> str: ...
              def function_with_positional_and_named(a: str, __x: str, __y: str, b: str) -> None: ...
            |}
  in
  let sources = ("test.py", source) :: sources in
  let resolution = ScratchProject.setup ~context sources |> ScratchProject.build_resolution in
  let configuration =
    TaintConfiguration.
      {
        empty with
        sources = List.map ~f:(fun name -> { AnnotationParser.name; kind = Named }) ["A"; "B"];
        sinks = List.map ~f:(fun name -> { AnnotationParser.name; kind = Named }) ["X"; "Y"; "Test"];
        features = ["featureA"; "featureB"];
        rules = [];
        partial_sink_labels = String.Map.Tree.of_alist_exn ["Test", ["a"; "b"]];
      }
  in
  let error_message =
    let path = path >>| PyrePath.create_absolute in
    ModelParser.parse
      ~resolution
      ~configuration
      ?path
      ~source:(Test.trim_extra_indentation model_source)
      ~callables:None
      ~stubs:(Target.HashSet.create ())
      ()
    |> fun { ModelParser.errors; _ } ->
    List.hd errors >>| ModelVerificationError.display |> Option.value ~default:"no failure"
  in
  assert_equal ~printer:ident expect error_message


let assert_queries ?source ?rules ~context ~model_source ~expect () =
  let { ModelParser.queries; _ }, _, _ =
    set_up_environment ?source ?rules ~context ~model_source ()
  in
  assert_equal
    ~cmp:
      (List.equal (fun left right -> ModelParser.Internal.ModelQuery.compare_rule left right = 0))
    ~printer:(List.to_string ~f:ModelParser.Internal.ModelQuery.show_rule)
    queries
    expect


let test_source_models context =
  let assert_model = assert_model ~context in
  assert_model
    ~model_source:"def test.taint() -> TaintSource[TestTest]: ..."
    ~expect:[outcome ~kind:`Function ~returns:[Sources.NamedSource "TestTest"] "test.taint"]
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
    ~model_source:"def test.taint() -> TaintSource[Test, UserControlled]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.NamedSource "Test"; Sources.NamedSource "UserControlled"]
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:"os.environ: TaintSink[Test] = ..."
    ~expect:
      [
        outcome
          ~kind:`Object
          ~sink_parameters:[{ name = "$global"; sinks = [Sinks.NamedSink "Test"] }]
          "os.environ";
      ]
    ();
  assert_model
    ~source:"def f(x: int): ..."
    ~model_source:"def test.f(x) -> TaintSource[Test, ViaValueOf[x]]: ..."
    ~expect:[outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "test.f"]
    ();
  assert_model
    ~source:"def f(x: int): ..."
    ~model_source:"def test.f() -> TaintSource[Test, ViaValueOf[x]]: ..."
    ~expect:[outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "test.f"]
    ();
  assert_model
    ~source:
      {|
    class C:
      @property
      def foo(self) -> int:
        return self.x
      @foo.setter
      def foo(self, value) -> None:
        self.x = value
    |}
    ~model_source:{|
      @property
      def test.C.foo(self) -> TaintSource[Test]: ...
    |}
    ~expect:[outcome ~kind:`Method ~returns:[Sources.NamedSource "Test"] "test.C.foo"]
    ();
  assert_model
    ~source:
      {|
    class C:
      @property
      def foo(self) -> int:
        return self.x
      @foo.setter
      def foo(self, value) -> None:
        self.x = value
    |}
    ~model_source:
      {|
      @foo.setter
      def test.C.foo(self, value) -> TaintSource[Test]: ...
    |}
    ~expect:[outcome ~kind:`PropertySetter ~returns:[Sources.NamedSource "Test"] "test.C.foo"]
    ();
  assert_model
    ~source:"def f(x: int): ..."
    ~model_source:"def test.f(x) -> AppliesTo[0, TaintSource[Test]]: ..."
    ~expect:[outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "test.f"]
    ();
  assert_model
    ~source:
      {|
        import abc
        class C:
          @abc.abstractproperty
          def foo(self) -> int:
            return self.x
          @foo.setter
          def foo(self, value) -> None:
            self.x = value
        |}
    ~model_source:{|
        @property
        def test.C.foo(self) -> TaintSource[Test]: ...
    |}
    ~expect:[outcome ~kind:`Method ~returns:[Sources.NamedSource "Test"] "test.C.foo"]
    ();
  assert_model
    ~source:"def f(x: int): ..."
    ~model_source:"def test.f(x) -> TaintSource[WithSubkind[Subkind]]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.ParametricSource { source_name = "WithSubkind"; subkind = "Subkind" }]
          "test.f";
      ]
    ();
  ();
  assert_model
    ~source:{|
      def foo(x, y): ...
    |}
    ~model_source:"def test.foo(y: TaintSource[Test]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~source_parameters:[{ name = "y"; sources = [Sources.NamedSource "Test"] }]
          "test.foo";
      ]
    ()


let test_global_sanitize context =
  let open Taint.Domains in
  let assert_model = assert_model ~context in
  let assert_invalid_model = assert_invalid_model ~context in
  assert_model
    ~model_source:{|
      @Sanitize
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~global_sanitizer:{ Sanitize.sources = Some All; sinks = Some All; tito = Some All }
          "test.taint";
      ]
    ();
  assert_invalid_model
    ~model_source:{|
      @Sanitize(TaintSource)
      def test.taint(x): ...
    |}
    ~expect:
      "`Sanitize(TaintSource)` is an invalid taint annotation: `TaintSource` is not supported \
       within `Sanitize()`. Did you mean to use `SanitizeSingleTrace(...)`?"
    ();
  assert_invalid_model
    ~model_source:{|
      @Sanitize(TaintSink)
      def test.taint(x): ...
    |}
    ~expect:
      "`Sanitize(TaintSink)` is an invalid taint annotation: `TaintSink` is not supported within \
       `Sanitize()`. Did you mean to use `SanitizeSingleTrace(...)`?"
    ();
  assert_model
    ~model_source:{|
      @Sanitize(TaintInTaintOut)
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~global_sanitizer:{ Sanitize.sources = None; sinks = None; tito = Some All }
          "test.taint";
      ]
    ();
  assert_invalid_model
    ~model_source:
      {|
      @Sanitize(TaintSource)
      @Sanitize(TaintInTaintOut)
      def test.taint(x): ...
    |}
    ~expect:
      "`Sanitize(TaintSource)` is an invalid taint annotation: `TaintSource` is not supported \
       within `Sanitize()`. Did you mean to use `SanitizeSingleTrace(...)`?"
    ();
  assert_invalid_model
    ~model_source:
      {|
      @Sanitize(TaintSource, TaintInTaintOut)
      def test.taint(x): ...
    |}
    ~expect:
      "`Sanitize(TaintSource, TaintInTaintOut)` is an invalid taint annotation: \
       `Sanitize[TaintSource]` is ambiguous here. Did you mean `Sanitize`?"
    ();
  assert_invalid_model
    ~model_source:{|
      @Sanitize(TaintSource[A])
      def test.taint(x): ...
    |}
    ~expect:
      "`Sanitize(TaintSource[A])` is an invalid taint annotation: `TaintSource` is not supported \
       within `Sanitize(...)`. Did you mean to use `SanitizeSingleTrace(...)`?"
    ();
  assert_invalid_model
    ~model_source:{|
      @Sanitize(TaintSource[A, B])
      def test.taint(x): ...
    |}
    ~expect:
      "`Sanitize(TaintSource[(A, B)])` is an invalid taint annotation: `TaintSource` is not \
       supported within `Sanitize(...)`. Did you mean to use `SanitizeSingleTrace(...)`?"
    ();
  assert_model
    ~model_source:
      {|
      @Sanitize(TaintInTaintOut[TaintSource[Test]])
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~global_sanitizer:
            {
              Sanitize.sources = None;
              sinks = None;
              tito =
                Some
                  (Specific
                     {
                       sanitized_tito_sources = Sources.Set.singleton (Sources.NamedSource "Test");
                       sanitized_tito_sinks = Sinks.Set.empty;
                     });
            }
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:
      {|
      @Sanitize(TaintInTaintOut[TaintSink[Test]])
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~global_sanitizer:
            {
              Sanitize.sources = None;
              sinks = None;
              tito =
                Some
                  (Specific
                     {
                       sanitized_tito_sources = Sources.Set.empty;
                       sanitized_tito_sinks = Sinks.Set.singleton (Sinks.NamedSink "Test");
                     });
            }
          "test.taint";
      ]
    ();
  assert_invalid_model
    ~model_source:
      {|
      @Sanitize(TaintSource[A], TaintInTaintOut[TaintSink[X]])
      def test.taint(x): ...
    |}
    ~expect:
      "`Sanitize(TaintSource[A], TaintInTaintOut[TaintSink[X]])` is an invalid taint annotation: \
       `TaintSource` is not supported within `Sanitize(...)`. Did you mean to use \
       `SanitizeSingleTrace(...)`?"
    ();
  assert_model
    ~model_source:
      {|
      @Sanitize(TaintInTaintOut[TaintSource[Test], TaintSink[Test]])
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~global_sanitizer:
            {
              Sanitize.sources = None;
              sinks = None;
              tito =
                Some
                  (Specific
                     {
                       sanitized_tito_sources = Sources.Set.singleton (Sources.NamedSource "Test");
                       sanitized_tito_sinks = Sinks.Set.singleton (Sinks.NamedSink "Test");
                     });
            }
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:
      {|
      @Sanitize
      @SkipDecoratorWhenInlining
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~global_sanitizer:{ Sanitize.sources = Some All; sinks = Some All; tito = Some All }
          ~analysis_modes:(Model.ModeSet.singleton SkipDecoratorWhenInlining)
          "test.taint";
      ]
    ()


let test_sanitize_single_trace context =
  let open Taint.Domains in
  let assert_model = assert_model ~context in
  let assert_invalid_model = assert_invalid_model ~context in
  assert_invalid_model
    ~model_source:{|
      @SanitizeSingleTrace
      def test.taint(x): ...
    |}
    ~expect:
      "`SanitizeSingleTrace()` is an invalid taint annotation: `SanitizeSingleTrace()` is \
       ambiguous. Did you mean `SanitizeSingleTrace(TaintSource)` or \
       `SanitizeSingleTrace(TaintSink)`?"
    ();
  assert_model
    ~model_source:{|
      @SanitizeSingleTrace(TaintSource)
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~global_sanitizer:{ Sanitize.sources = Some All; sinks = None; tito = None }
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:{|
      @SanitizeSingleTrace(TaintSink)
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~global_sanitizer:{ Sanitize.sources = None; sinks = Some All; tito = None }
          "test.taint";
      ]
    ();
  assert_invalid_model
    ~model_source:{|
      @SanitizeSingleTrace(TaintInTaintOut)
      def test.taint(x): ...
    |}
    ~expect:
      "`SanitizeSingleTrace(TaintInTaintOut)` is an invalid taint annotation: `TaintInTaintOut` is \
       not supported within `SanitizeSingleTrace(...)`. Did you mean to use `Sanitize(...)`?"
    ();
  assert_invalid_model
    ~model_source:
      {|
      @SanitizeSingleTrace(TaintSource)
      @SanitizeSingleTrace(TaintInTaintOut)
      def test.taint(x): ...
    |}
    ~expect:
      "`SanitizeSingleTrace(TaintInTaintOut)` is an invalid taint annotation: `TaintInTaintOut` is \
       not supported within `SanitizeSingleTrace(...)`. Did you mean to use `Sanitize(...)`?"
    ();
  assert_invalid_model
    ~model_source:
      {|
      @SanitizeSingleTrace(TaintSink)
      @SanitizeSingleTrace(TaintInTaintOut)
      def test.taint(x): ...
    |}
    ~expect:
      "`SanitizeSingleTrace(TaintInTaintOut)` is an invalid taint annotation: `TaintInTaintOut` is \
       not supported within `SanitizeSingleTrace(...)`. Did you mean to use `Sanitize(...)`?"
    ();
  assert_model
    ~model_source:
      {|
      @SanitizeSingleTrace(TaintSource[Test])
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~global_sanitizer:
            {
              Sanitize.sources =
                Some (Specific (Sources.Set.singleton (Sources.NamedSource "Test")));
              sinks = None;
              tito = None;
            }
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:
      {|
      @SanitizeSingleTrace(TaintSource[Test, UserControlled])
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~global_sanitizer:
            {
              Sanitize.sources =
                Some
                  (Specific
                     (Sources.Set.of_list
                        [Sources.NamedSource "UserControlled"; Sources.NamedSource "Test"]));
              sinks = None;
              tito = None;
            }
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:{|
      @SanitizeSingleTrace(TaintSink[Test])
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~global_sanitizer:
            {
              Sanitize.sources = None;
              sinks = Some (Specific (Sinks.Set.singleton (Sinks.NamedSink "Test")));
              tito = None;
            }
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:
      {|
      @SanitizeSingleTrace(TaintSink[TestSink, OtherSink])
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~global_sanitizer:
            {
              Sanitize.sources = None;
              sinks =
                Some
                  (Specific
                     (Sinks.Set.of_list [Sinks.NamedSink "TestSink"; Sinks.NamedSink "OtherSink"]));
              tito = None;
            }
          "test.taint";
      ]
    ();
  assert_invalid_model
    ~model_source:
      {|
      @SanitizeSingleTrace(TaintInTaintOut[TaintSource[A]])
      def test.taint(x): ...
    |}
    ~expect:
      "`SanitizeSingleTrace(TaintInTaintOut[TaintSource[A]])` is an invalid taint annotation: \
       `TaintInTaintOut` is not supported within `SanitizeSingleTrace(...)`. Did you mean to use \
       `Sanitize(...)`?"
    ();
  assert_invalid_model
    ~model_source:
      {|
      @SanitizeSingleTrace(TaintInTaintOut[TaintSink[X]])
      def test.taint(x): ...
    |}
    ~expect:
      "`SanitizeSingleTrace(TaintInTaintOut[TaintSink[X]])` is an invalid taint annotation: \
       `TaintInTaintOut` is not supported within `SanitizeSingleTrace(...)`. Did you mean to use \
       `Sanitize(...)`?"
    ();
  ()


let test_attribute_sanitize context =
  let open Taint.Domains in
  let assert_model = assert_model ~context in
  assert_model
    ~model_source:"django.http.Request.GET: Sanitize = ..."
    ~expect:
      [
        outcome
          ~kind:`Object
          ~global_sanitizer:{ Sanitize.sources = Some All; sinks = Some All; tito = Some All }
          "django.http.Request.GET";
      ]
    ();
  assert_model
    ~model_source:"django.http.Request.GET: Sanitize[TaintSource] = ..."
    ~expect:
      [
        outcome
          ~kind:`Object
          ~global_sanitizer:{ Sanitize.sources = Some All; sinks = None; tito = None }
          "django.http.Request.GET";
      ]
    ();
  assert_model
    ~model_source:"django.http.Request.GET: Sanitize[TaintSink] = ..."
    ~expect:
      [
        outcome
          ~kind:`Object
          ~global_sanitizer:{ Sanitize.sources = None; sinks = Some All; tito = None }
          "django.http.Request.GET";
      ]
    ();
  assert_model
    ~model_source:"django.http.Request.GET: Sanitize[TaintSource[Test]] = ..."
    ~expect:
      [
        outcome
          ~kind:`Object
          ~global_sanitizer:
            {
              Sanitize.sources =
                Some (Specific (Sources.Set.singleton (Sources.NamedSource "Test")));
              sinks = None;
              tito = None;
            }
          "django.http.Request.GET";
      ]
    ();
  assert_model
    ~model_source:"django.http.Request.GET: Sanitize[TaintSink[Test]] = ..."
    ~expect:
      [
        outcome
          ~kind:`Object
          ~global_sanitizer:
            {
              Sanitize.sources = None;
              sinks = Some (Specific (Sinks.Set.singleton (Sinks.NamedSink "Test")));
              tito = None;
            }
          "django.http.Request.GET";
      ]
    ();
  assert_model
    ~model_source:"django.http.Request.GET: Sanitize[TaintSource[Test, TestTest]] = ..."
    ~expect:
      [
        outcome
          ~kind:`Object
          ~global_sanitizer:
            {
              Sanitize.sources =
                Some
                  (Specific
                     (Sources.Set.of_list
                        [Sources.NamedSource "TestTest"; Sources.NamedSource "Test"]));
              sinks = None;
              tito = None;
            }
          "django.http.Request.GET";
      ]
    ();
  assert_model
    ~model_source:"django.http.Request.GET: Sanitize[TaintSink[Test, TestSink]] = ..."
    ~expect:
      [
        outcome
          ~kind:`Object
          ~global_sanitizer:
            {
              Sanitize.sources = None;
              sinks =
                Some
                  (Specific (Sinks.Set.of_list [Sinks.NamedSink "TestSink"; Sinks.NamedSink "Test"]));
              tito = None;
            }
          "django.http.Request.GET";
      ]
    ()


let test_parameter_sanitize context =
  let open Taint.Domains in
  let assert_model = assert_model ~context in
  let assert_invalid_model = assert_invalid_model ~context in
  assert_model
    ~model_source:{|
      def test.taint(x: Sanitize): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sanitizers:
            [
              {
                name = "x";
                sanitize = { Sanitize.sources = Some All; sinks = Some All; tito = Some All };
              };
            ]
          "test.taint";
      ]
    ();
  assert_invalid_model
    ~model_source:{|
      def test.taint(x: Sanitize[TaintSource]): ...
    |}
    ~expect:
      "`Sanitize[TaintSource]` is an invalid taint annotation: `Sanitize[TaintSource]` is \
       ambiguous here. Did you mean `Sanitize`?"
    ();
  assert_invalid_model
    ~model_source:{|
      def test.taint(x: Sanitize[TaintSink]): ...
    |}
    ~expect:
      "`Sanitize[TaintSink]` is an invalid taint annotation: `Sanitize[TaintSink]` is ambiguous \
       here. Did you mean `Sanitize`?"
    ();
  assert_model
    ~model_source:{|
      def test.taint(x: Sanitize[TaintInTaintOut]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sanitizers:
            [{ name = "x"; sanitize = { Sanitize.sources = None; sinks = None; tito = Some All } }]
          "test.taint";
      ]
    ();
  assert_invalid_model
    ~model_source:
      {|
      def test.taint(x: Sanitize[TaintSource], y: Sanitize[TaintInTaintOut]): ...
    |}
    ~expect:
      "`Sanitize[TaintSource]` is an invalid taint annotation: `Sanitize[TaintSource]` is \
       ambiguous here. Did you mean `Sanitize`?"
    ();
  assert_invalid_model
    ~model_source:{|
      def test.taint(x: Sanitize[TaintSource, TaintInTaintOut]): ...
    |}
    ~expect:
      "`Sanitize[(TaintSource, TaintInTaintOut)]` is an invalid taint annotation: \
       `Sanitize[TaintSource]` is ambiguous here. Did you mean `Sanitize`?"
    ();
  assert_model
    ~model_source:{|
      def test.taint(x: Sanitize[TaintSource[Test]]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sanitizers:
            [
              {
                name = "x";
                sanitize =
                  {
                    Sanitize.sources =
                      Some (Specific (Sources.Set.singleton (Sources.NamedSource "Test")));
                    sinks = None;
                    tito = None;
                  };
              };
            ]
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:
      {|
      def test.taint(x: Sanitize[TaintSource[Test, UserControlled]]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sanitizers:
            [
              {
                name = "x";
                sanitize =
                  {
                    Sanitize.sources =
                      Some
                        (Specific
                           (Sources.Set.of_list
                              [Sources.NamedSource "UserControlled"; Sources.NamedSource "Test"]));
                    sinks = None;
                    tito = None;
                  };
              };
            ]
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:
      {|
      def test.taint(x: Sanitize[TaintInTaintOut[TaintSource[Test]]]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sanitizers:
            [
              {
                name = "x";
                sanitize =
                  {
                    Sanitize.sources = None;
                    sinks = None;
                    tito =
                      Some
                        (Specific
                           {
                             sanitized_tito_sources =
                               Sources.Set.singleton (Sources.NamedSource "Test");
                             sanitized_tito_sinks = Sinks.Set.empty;
                           });
                  };
              };
            ]
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:{|
      def test.taint(x: Sanitize[TaintInTaintOut[TaintSink[Test]]]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sanitizers:
            [
              {
                name = "x";
                sanitize =
                  {
                    Sanitize.sources = None;
                    sinks = None;
                    tito =
                      Some
                        (Specific
                           {
                             sanitized_tito_sources = Sources.Set.empty;
                             sanitized_tito_sinks = Sinks.Set.singleton (Sinks.NamedSink "Test");
                           });
                  };
              };
            ]
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:
      {|
      def test.taint(x: Sanitize[TaintSource[Test], TaintInTaintOut[TaintSink[Test]]]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sanitizers:
            [
              {
                name = "x";
                sanitize =
                  {
                    Sanitize.sources =
                      Some (Specific (Sources.Set.singleton (Sources.NamedSource "Test")));
                    sinks = None;
                    tito =
                      Some
                        (Specific
                           {
                             sanitized_tito_sources = Sources.Set.empty;
                             sanitized_tito_sinks = Sinks.Set.singleton (Sinks.NamedSink "Test");
                           });
                  };
              };
            ]
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:
      {|
      def test.taint(x: Sanitize[TaintInTaintOut[TaintSource[Test], TaintSink[Test]]]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sanitizers:
            [
              {
                name = "x";
                sanitize =
                  {
                    Sanitize.sources = None;
                    sinks = None;
                    tito =
                      Some
                        (Specific
                           {
                             sanitized_tito_sources =
                               Sources.Set.singleton (Sources.NamedSource "Test");
                             sanitized_tito_sinks = Sinks.Set.singleton (Sinks.NamedSink "Test");
                           });
                  };
              };
            ]
          "test.taint";
      ]
    ()


let test_return_sanitize context =
  let open Taint.Domains in
  let assert_model = assert_model ~context in
  let assert_invalid_model = assert_invalid_model ~context in
  assert_model
    ~model_source:{|
      def test.taint(x) -> Sanitize: ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~return_sanitizer:{ Sanitize.sources = Some All; sinks = Some All; tito = Some All }
          "test.taint";
      ]
    ();
  assert_invalid_model
    ~model_source:{|
      def test.taint(x) -> Sanitize[TaintSource]: ...
    |}
    ~expect:
      "`Sanitize[TaintSource]` is an invalid taint annotation: `Sanitize[TaintSource]` is \
       ambiguous here. Did you mean `Sanitize`?"
    ();
  assert_invalid_model
    ~model_source:{|
      def test.taint(x) -> Sanitize[TaintSink]: ...
    |}
    ~expect:
      "`Sanitize[TaintSink]` is an invalid taint annotation: `Sanitize[TaintSink]` is ambiguous \
       here. Did you mean `Sanitize`?"
    ();
  assert_model
    ~model_source:{|
      def test.taint(x) -> Sanitize[TaintInTaintOut]: ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~return_sanitizer:{ Sanitize.sources = None; sinks = None; tito = Some All }
          "test.taint";
      ]
    ();
  assert_invalid_model
    ~model_source:{|
      def test.taint(x) -> Sanitize[TaintSource, TaintInTaintOut]: ...
    |}
    ~expect:
      "`Sanitize[(TaintSource, TaintInTaintOut)]` is an invalid taint annotation: \
       `Sanitize[TaintSource]` is ambiguous here. Did you mean `Sanitize`?"
    ();
  assert_model
    ~model_source:{|
      def test.taint(x) -> Sanitize[TaintSource[Test]]: ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~return_sanitizer:
            {
              Sanitize.sources =
                Some (Specific (Sources.Set.singleton (Sources.NamedSource "Test")));
              sinks = None;
              tito = None;
            }
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:
      {|
      def test.taint(x) -> Sanitize[TaintSource[Test, UserControlled]]: ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~return_sanitizer:
            {
              Sanitize.sources =
                Some
                  (Specific
                     (Sources.Set.of_list
                        [Sources.NamedSource "UserControlled"; Sources.NamedSource "Test"]));
              sinks = None;
              tito = None;
            }
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:
      {|
      def test.taint(x) -> Sanitize[TaintInTaintOut[TaintSource[Test]]]: ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~return_sanitizer:
            {
              Sanitize.sources = None;
              sinks = None;
              tito =
                Some
                  (Specific
                     {
                       sanitized_tito_sources = Sources.Set.singleton (Sources.NamedSource "Test");
                       sanitized_tito_sinks = Sinks.Set.empty;
                     });
            }
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:
      {|
      def test.taint(x) -> Sanitize[TaintInTaintOut[TaintSink[Test]]]: ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~return_sanitizer:
            {
              Sanitize.sources = None;
              sinks = None;
              tito =
                Some
                  (Specific
                     {
                       sanitized_tito_sources = Sources.Set.empty;
                       sanitized_tito_sinks = Sinks.Set.singleton (Sinks.NamedSink "Test");
                     });
            }
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:
      {|
      def test.taint(x) -> Sanitize[TaintSource[Test], TaintInTaintOut[TaintSink[Test]]]: ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~return_sanitizer:
            {
              Sanitize.sources =
                Some (Specific (Sources.Set.singleton (Sources.NamedSource "Test")));
              sinks = None;
              tito =
                Some
                  (Specific
                     {
                       sanitized_tito_sources = Sources.Set.empty;
                       sanitized_tito_sinks = Sinks.Set.singleton (Sinks.NamedSink "Test");
                     });
            }
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:
      {|
      def test.taint(x) -> Sanitize[TaintInTaintOut[TaintSource[Test], TaintSink[Test]]]: ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~return_sanitizer:
            {
              Sanitize.sources = None;
              sinks = None;
              tito =
                Some
                  (Specific
                     {
                       sanitized_tito_sources = Sources.Set.singleton (Sources.NamedSource "Test");
                       sanitized_tito_sinks = Sinks.Set.singleton (Sinks.NamedSink "Test");
                     });
            }
          "test.taint";
      ]
    ()


let test_parameters_sanitize context =
  let open Taint.Domains in
  let assert_model = assert_model ~context in
  let assert_invalid_model = assert_invalid_model ~context in
  assert_model
    ~model_source:{|
      @Sanitize(Parameters)
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameters_sanitizer:{ Sanitize.sources = Some All; sinks = Some All; tito = Some All }
          "test.taint";
      ]
    ();
  assert_invalid_model
    ~model_source:{|
      @Sanitize(Parameters[TaintSource])
      def test.taint(x): ...
    |}
    ~expect:
      "`Sanitize(Parameters[TaintSource])` is an invalid taint annotation: `Sanitize[TaintSource]` \
       is ambiguous here. Did you mean `Sanitize`?"
    ();
  assert_invalid_model
    ~model_source:{|
      @Sanitize(Parameters[TaintSink])
      def test.taint(x): ...
    |}
    ~expect:
      "`Sanitize(Parameters[TaintSink])` is an invalid taint annotation: `Sanitize[TaintSink]` is \
       ambiguous here. Did you mean `Sanitize`?"
    ();
  assert_model
    ~model_source:
      {|
      @Sanitize(Parameters[TaintInTaintOut])
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameters_sanitizer:{ Sanitize.sources = None; sinks = None; tito = Some All }
          "test.taint";
      ]
    ();
  assert_invalid_model
    ~model_source:
      {|
      @Sanitize(Parameters[TaintSource])
      @Sanitize(Parameters[TaintInTaintOut])
      def test.taint(x): ...
    |}
    ~expect:
      "`Sanitize(Parameters[TaintSource])` is an invalid taint annotation: `Sanitize[TaintSource]` \
       is ambiguous here. Did you mean `Sanitize`?"
    ();
  assert_invalid_model
    ~model_source:
      {|
      @Sanitize(Parameters[TaintSource, TaintInTaintOut])
      def test.taint(x): ...
    |}
    ~expect:
      "`Sanitize(Parameters[(TaintSource, TaintInTaintOut)])` is an invalid taint annotation: \
       `Sanitize[TaintSource]` is ambiguous here. Did you mean `Sanitize`?"
    ();
  assert_model
    ~model_source:
      {|
      @Sanitize(Parameters[TaintSource[Test]])
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameters_sanitizer:
            {
              Sanitize.sources =
                Some (Specific (Sources.Set.singleton (Sources.NamedSource "Test")));
              sinks = None;
              tito = None;
            }
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:
      {|
      @Sanitize(Parameters[TaintSource[Test, UserControlled]])
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameters_sanitizer:
            {
              Sanitize.sources =
                Some
                  (Specific
                     (Sources.Set.of_list
                        [Sources.NamedSource "UserControlled"; Sources.NamedSource "Test"]));
              sinks = None;
              tito = None;
            }
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:
      {|
      @Sanitize(Parameters[TaintInTaintOut[TaintSource[Test]]])
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameters_sanitizer:
            {
              Sanitize.sources = None;
              sinks = None;
              tito =
                Some
                  (Specific
                     {
                       sanitized_tito_sources = Sources.Set.singleton (Sources.NamedSource "Test");
                       sanitized_tito_sinks = Sinks.Set.empty;
                     });
            }
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:
      {|
      @Sanitize(Parameters[TaintInTaintOut[TaintSink[Test]]])
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameters_sanitizer:
            {
              Sanitize.sources = None;
              sinks = None;
              tito =
                Some
                  (Specific
                     {
                       sanitized_tito_sources = Sources.Set.empty;
                       sanitized_tito_sinks = Sinks.Set.singleton (Sinks.NamedSink "Test");
                     });
            }
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:
      {|
      @Sanitize(Parameters[TaintSource[Test], TaintInTaintOut[TaintSink[Test]]])
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameters_sanitizer:
            {
              Sanitize.sources =
                Some (Specific (Sources.Set.singleton (Sources.NamedSource "Test")));
              sinks = None;
              tito =
                Some
                  (Specific
                     {
                       sanitized_tito_sources = Sources.Set.empty;
                       sanitized_tito_sinks = Sinks.Set.singleton (Sinks.NamedSink "Test");
                     });
            }
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:
      {|
      @Sanitize(Parameters[TaintInTaintOut[TaintSource[Test], TaintSink[Test]]])
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameters_sanitizer:
            {
              Sanitize.sources = None;
              sinks = None;
              tito =
                Some
                  (Specific
                     {
                       sanitized_tito_sources = Sources.Set.singleton (Sources.NamedSource "Test");
                       sanitized_tito_sinks = Sinks.Set.singleton (Sinks.NamedSink "Test");
                     });
            }
          "test.taint";
      ]
    ()


let test_sink_models context =
  let assert_model = assert_model ~context in
  assert_model
    ~model_source:{|
        def test.sink(parameter: TaintSink[TestSink]):
          ...
      |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~sink_parameters:[{ name = "parameter"; sinks = [Sinks.NamedSink "TestSink"] }]
          "test.sink";
      ]
    ();
  assert_model
    ~model_source:"def test.sink(parameter0, parameter1: TaintSink[Test]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~sink_parameters:[{ name = "parameter1"; sinks = [Sinks.NamedSink "Test"] }]
          "test.sink";
      ]
    ();
  assert_model
    ~model_source:"def test.sink(parameter0: TaintSink[Test], parameter1: TaintSink[Test]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~sink_parameters:
            [
              { name = "parameter0"; sinks = [Sinks.NamedSink "Test"] };
              { name = "parameter1"; sinks = [Sinks.NamedSink "Test"] };
            ]
          "test.sink";
      ]
    ();
  assert_model
    ~model_source:"def test.sink(parameter0: TaintSink[Test], parameter1: TaintSink[Test]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~sink_parameters:
            [
              { name = "parameter0"; sinks = [Sinks.NamedSink "Test"] };
              { name = "parameter1"; sinks = [Sinks.NamedSink "Test"] };
            ]
          "test.sink";
      ]
    ();
  assert_model
    ~model_source:"def test.both(parameter0: TaintSink[Demo]) -> TaintSource[Demo]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.NamedSource "Demo"]
          ~sink_parameters:[{ name = "parameter0"; sinks = [Sinks.NamedSink "Demo"] }]
          "test.both";
      ]
    ();
  assert_model
    ~model_source:
      "def test.sink(parameter0: TaintSink[Test], parameter1: TaintSink[Test, \
       ViaValueOf[parameter0]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~sink_parameters:
            [
              { name = "parameter0"; sinks = [Sinks.NamedSink "Test"] };
              { name = "parameter1"; sinks = [Sinks.NamedSink "Test"] };
            ]
          "test.sink";
      ]
    ();
  assert_model
    ~model_source:"def test.xss(parameter: TaintSink[XSS]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~sink_parameters:[{ name = "parameter"; sinks = [Sinks.NamedSink "XSS"] }]
          "test.xss";
      ]
    ();
  assert_model
    ~model_source:"def test.multiple(parameter: TaintSink[XSS, Demo]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~sink_parameters:
            [{ name = "parameter"; sinks = [Sinks.NamedSink "Demo"; Sinks.NamedSink "XSS"] }]
          "test.multiple";
      ]
    ();
  assert_model
    ~model_source:"def test.multiple(parameter: AppliesTo[1, TaintSink[XSS, Demo]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~sink_parameters:
            [{ name = "parameter"; sinks = [Sinks.NamedSink "Demo"; Sinks.NamedSink "XSS"] }]
          "test.multiple";
      ]
    ();
  assert_model
    ~model_source:
      {|
        def test.sink(parameter: TaintSink[TestSinkWithSubkind[Subkind]]):
          ...
      |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~sink_parameters:
            [
              {
                name = "parameter";
                sinks =
                  [Sinks.ParametricSink { sink_name = "TestSinkWithSubkind"; subkind = "Subkind" }];
              };
            ]
          "test.sink";
      ]
    ();

  ()


let test_cross_repository_models context =
  let assert_model = assert_model ~context in
  assert_model
    ~source:{|
      def cross_repository_source(source_parameter): ...
    |}
    ~model_source:
      {|
      def test.cross_repository_source(
        source_parameter: CrossRepositoryTaint[
          TaintSource[UserControlled],
          'crossRepositorySource',
          'formal(0)',
          0,
        ]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~source_parameters:
            [{ name = "source_parameter"; sources = [Sources.NamedSource "UserControlled"] }]
          "test.cross_repository_source";
      ]
    ();
  assert_model
    ~source:{|
      def cross_repository_source(source_parameter): ...
    |}
    ~model_source:
      {|
      def test.cross_repository_source(
        source_parameter: CrossRepositoryTaint[
          TaintSource[UserControlled],
          'crossRepositorySource',
          'formal(0)',
          0,
          1,
        ]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~source_parameters:
            [{ name = "source_parameter"; sources = [Sources.NamedSource "UserControlled"] }]
          "test.cross_repository_source";
      ]
    ();
  assert_model
    ~source:{|
      def cross_repository_source(source_parameter): ...
    |}
    ~model_source:
      {|
      def test.cross_repository_source(
        source_parameter: CrossRepositoryTaintAnchor[
          TaintSource[UserControlled],
          'crossRepositorySource',
          'formal(0)',
        ]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~source_parameters:
            [{ name = "source_parameter"; sources = [Sources.NamedSource "UserControlled"] }]
          "test.cross_repository_source";
      ]
    ();
  assert_model
    ~source:{|
      def cross_repository_sink(x): ...
    |}
    ~model_source:
      {|
      def test.cross_repository_sink(
        x: CrossRepositoryTaintAnchor[
          TaintSink[Test],
          'crossRepositorySink',
          'formal(0)',
        ]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~sink_parameters:[{ name = "x"; sinks = [Sinks.NamedSink "Test"] }]
          "test.cross_repository_sink";
      ]
    ()


let test_class_models context =
  let assert_model = assert_model ~context in
  assert_model
    ~source:
      {|
        class Sink:
          def method(parameter): ...
          def method_with_multiple_parameters(first, second): ...
      |}
    ~model_source:"class test.Sink(TaintSink[TestSink]): ..."
    ~expect:
      [
        outcome
          ~kind:`Method
          ~sink_parameters:[{ name = "parameter"; sinks = [Sinks.NamedSink "TestSink"] }]
          "test.Sink.method";
        outcome
          ~kind:`Method
          ~sink_parameters:
            [
              { name = "first"; sinks = [Sinks.NamedSink "TestSink"] };
              { name = "second"; sinks = [Sinks.NamedSink "TestSink"] };
            ]
          "test.Sink.method_with_multiple_parameters";
      ]
    ();
  assert_model
    ~source:
      {|
        class Sink:
          def method(parameter): ...
          def method_with_multiple_parameters(first, second): ...
      |}
    ~model_source:"class test.Sink(TaintSink[TestSink], TaintSink[OtherSink]): ..."
    ~expect:
      [
        outcome
          ~kind:`Method
          ~sink_parameters:
            [
              {
                name = "parameter";
                sinks = [Sinks.NamedSink "OtherSink"; Sinks.NamedSink "TestSink"];
              };
            ]
          "test.Sink.method";
        outcome
          ~kind:`Method
          ~sink_parameters:
            [
              { name = "first"; sinks = [Sinks.NamedSink "OtherSink"; Sinks.NamedSink "TestSink"] };
              { name = "second"; sinks = [Sinks.NamedSink "OtherSink"; Sinks.NamedSink "TestSink"] };
            ]
          "test.Sink.method_with_multiple_parameters";
      ]
    ();
  assert_model
    ~source:{|
        class SinkAndSource:
          def method(parameter): ...
      |}
    ~model_source:"class test.SinkAndSource(TaintSink[TestSink], TaintSource[TestTest]): ..."
    ~expect:
      [
        outcome
          ~kind:`Method
          ~sink_parameters:[{ name = "parameter"; sinks = [Sinks.NamedSink "TestSink"] }]
          ~returns:[Sources.NamedSource "TestTest"]
          "test.SinkAndSource.method";
      ]
    ();

  assert_model
    ~source:
      {|
        class Source:
          def method(parameter): ...
          def method_with_multiple_parameters(first, second): ...
          Source.attribute = ...
      |}
    ~model_source:{|
        class test.Source(TaintSource[UserControlled]): ...
      |}
    ~expect:
      [
        outcome ~kind:`Method ~returns:[Sources.NamedSource "UserControlled"] "test.Source.method";
        outcome
          ~kind:`Method
          ~returns:[Sources.NamedSource "UserControlled"]
          "test.Source.method_with_multiple_parameters";
      ]
    ();
  assert_model
    ~source:
      {|
        class AnnotatedSink:
          def method(parameter: int) -> None: ...
      |}
    ~model_source:"class test.AnnotatedSink(TaintSink[TestSink]): ..."
    ~expect:
      [
        outcome
          ~kind:`Method
          ~sink_parameters:[{ name = "parameter"; sinks = [Sinks.NamedSink "TestSink"] }]
          "test.AnnotatedSink.method";
      ]
    ();
  assert_model
    ~source:
      {|
         class AnnotatedSource:
          def method(parameter: int) -> None: ...
      |}
    ~model_source:"class test.AnnotatedSource(TaintSource[UserControlled]): ..."
    ~expect:
      [
        outcome
          ~kind:`Method
          ~returns:[Sources.NamedSource "UserControlled"]
          "test.AnnotatedSource.method";
      ]
    ();
  assert_model
    ~source:
      {|
         class SourceWithDefault:
          def method(parameter: int = 1) -> None: ...
      |}
    ~model_source:"class test.SourceWithDefault(TaintSink[Test]): ..."
    ~expect:
      [
        outcome
          ~kind:`Method
          ~sink_parameters:[{ name = "parameter"; sinks = [Sinks.NamedSink "Test"] }]
          "test.SourceWithDefault.method";
      ]
    ();
  assert_model
    ~source:
      {|
         class Source:
           @classmethod
           def method(cls, parameter: int) -> None: ...
      |}
    ~model_source:"class test.Source(TaintSource[UserControlled]): ..."
    ~expect:
      [outcome ~kind:`Method ~returns:[Sources.NamedSource "UserControlled"] "test.Source.method"]
    ();
  assert_model
    ~source:
      {|
         class Source:
           @property
           def prop(self) -> int: ...
      |}
    ~model_source:"class test.Source(TaintSource[UserControlled]): ..."
    ~expect:
      [outcome ~kind:`Method ~returns:[Sources.NamedSource "UserControlled"] "test.Source.prop"]
    ();
  assert_model
    ~source:
      {|
        class SkipMe:
          def method(parameter): ...
          def method_with_multiple_parameters(first, second): ...
      |}
    ~model_source:"class test.SkipMe(SkipAnalysis): ..."
    ~expect:
      [
        outcome
          ~kind:`Method
          ~analysis_modes:(Model.ModeSet.singleton SkipAnalysis)
          "test.SkipMe.method";
        outcome
          ~kind:`Method
          ~analysis_modes:(Model.ModeSet.singleton SkipAnalysis)
          "test.SkipMe.method_with_multiple_parameters";
      ]
    ();
  (* Skip overrides do not generate methods. *)
  assert_model
    ~source:
      {|
        class SkipMe:
          def method(parameter): ...
          def method_with_multiple_parameters(first, second): ...
      |}
    ~model_source:"class test.SkipMe(SkipOverrides): ..."
    ~expected_skipped_overrides:
      ["test.SkipMe.method"; "test.SkipMe.method_with_multiple_parameters"]
    ~expect:[]
    ()


let test_taint_in_taint_out_models context =
  assert_model
    ~context
    ~model_source:"def test.tito(parameter: TaintInTaintOut): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~tito_parameters:[{ name = "parameter"; sinks = [Sinks.LocalReturn] }]
          "test.tito";
      ]
    ();
  assert_model
    ~context
    ~model_source:"def test.tito(parameter: AppliesTo[1, TaintInTaintOut]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~tito_parameters:[{ name = "parameter"; sinks = [Sinks.LocalReturn] }]
          "test.tito";
      ]
    ()


let test_taint_in_taint_out_models_alternate context =
  assert_model
    ~context
    ~model_source:"def test.tito(parameter: TaintInTaintOut[LocalReturn]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~tito_parameters:[{ name = "parameter"; sinks = [Sinks.LocalReturn] }]
          "test.tito";
      ]
    ()


let test_taint_in_taint_out_transform context =
  assert_model
    ~context
    ~model_source:"def test.tito(parameter: TaintInTaintOut[Transform[TestTransform]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~tito_parameters:
            [
              {
                name = "parameter";
                sinks =
                  [
                    Sinks.Transform
                      {
                        local =
                          TaintTransforms.of_named_transforms [TaintTransform.Named "TestTransform"];
                        global = TaintTransforms.empty;
                        base = Sinks.LocalReturn;
                      };
                  ];
              };
            ]
          "test.tito";
      ]
    ()


let test_skip_analysis context =
  assert_model
    ~context
    ~model_source:{|
      @SkipAnalysis
      def test.taint(x): ...
    |}
    ~expect:
      [outcome ~kind:`Function ~analysis_modes:(Model.ModeSet.singleton SkipAnalysis) "test.taint"]
    ()


let test_skip_inlining_decorator context =
  let assert_model = assert_model ~context in
  assert_model
    ~model_source:{|
      @SkipDecoratorWhenInlining
      def test.my_decorator(f): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~analysis_modes:(Model.ModeSet.singleton SkipDecoratorWhenInlining)
          "test.my_decorator";
      ]
    ();
  ()


let test_taint_in_taint_out_update_models context =
  let assert_model = assert_model ~context in
  assert_model
    ~model_source:"def test.update(self, arg1: TaintInTaintOut[Updates[self]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~tito_parameters:[{ name = "arg1"; sinks = [Sinks.ParameterUpdate 0] }]
          "test.update";
      ]
    ();
  assert_model
    ~model_source:"def test.update(self, arg1, arg2: TaintInTaintOut[Updates[self, arg1]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~tito_parameters:
            [{ name = "arg2"; sinks = [Sinks.ParameterUpdate 0; Sinks.ParameterUpdate 1] }]
          "test.update";
      ]
    ();
  assert_model
    ~model_source:"def test.update(self: TaintInTaintOut[LocalReturn, Updates[arg1]], arg1): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~tito_parameters:[{ name = "self"; sinks = [Sinks.LocalReturn; Sinks.ParameterUpdate 1] }]
          "test.update";
      ]
    ()


let test_union_models context =
  assert_model
    ~context
    ~model_source:"def test.both(parameter: Union[TaintInTaintOut, TaintSink[XSS]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~sink_parameters:[{ name = "parameter"; sinks = [Sinks.NamedSink "XSS"] }]
          ~tito_parameters:[{ name = "parameter"; sinks = [Sinks.LocalReturn] }]
          "test.both";
      ]
    ()


let test_source_breadcrumbs context =
  assert_model
    ~context
    ~model_source:"def test.source() -> TaintSource[Test, Via[special]]: ..."
    ~expect:[outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "test.source"]
    ();
  assert_model
    ~context
    ~model_source:
      "def test.source() -> TaintSource[Test, ViaDynamicFeature[NotSpecifiedInConfig]]: ..."
    ~expect:[outcome ~kind:`Function ~returns:[Sources.NamedSource "Test"] "test.source"]
    ()


let test_sink_breadcrumbs context =
  assert_model
    ~context
    ~model_source:"def test.sink(parameter: TaintSink[Test, Via[special]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~sink_parameters:[{ name = "parameter"; sinks = [Sinks.NamedSink "Test"] }]
          "test.sink";
      ]
    ()


let test_tito_breadcrumbs context =
  assert_model
    ~context
    ~model_source:"def test.tito(parameter: TaintInTaintOut[Via[special]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~tito_parameters:[{ name = "parameter"; sinks = [Sinks.LocalReturn] }]
          "test.tito";
      ]
    ()


let test_attach_features context =
  let assert_model = assert_model ~context in
  assert_model
    ~model_source:"def test.source() -> AttachToSource[Via[special]]: ..."
    ~expect:[outcome ~kind:`Function ~returns:[Sources.Attach] "test.source"]
    ();
  assert_model
    ~model_source:"def test.sink(arg: AttachToSink[Via[special]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~sink_parameters:[{ name = "arg"; sinks = [Sinks.Attach] }]
          "test.sink";
      ]
    ();
  assert_model
    ~model_source:"def test.tito(arg: AttachToTito[Via[special]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~tito_parameters:[{ name = "arg"; sinks = [Sinks.Attach] }]
          "test.tito";
      ]
    ()


let test_partial_sinks context =
  let assert_model = assert_model ~context in
  assert_model
    ~model_source:"def test.partial_sink(x: PartialSink[Test[a]], y: PartialSink[Test[b]]): ..."
    ~expect:
      [
        outcome
          ~sink_parameters:
            [
              { name = "x"; sinks = [Sinks.PartialSink { kind = "Test"; label = "a" }] };
              { name = "y"; sinks = [Sinks.PartialSink { kind = "Test"; label = "b" }] };
            ]
          ~kind:`Function
          "test.partial_sink";
      ]
    ()


let test_invalid_models context =
  let assert_invalid_model ?path ?source ?sources ~model_source ~expect () =
    assert_invalid_model ?path ?source ?sources ~context ~model_source ~expect ()
  in
  let assert_valid_model ?path ?source ?sources ~model_source () =
    assert_invalid_model ?path ?source ?sources ~model_source ~expect:"no failure" ()
  in
  assert_invalid_model ~model_source:"1 + import" ~expect:"Syntax error." ();
  assert_invalid_model ~model_source:"import foo" ~expect:"Unexpected statement." ();
  assert_invalid_model
    ~model_source:"def test.sink(parameter: TaintSink[X, Unsupported]) -> TaintSource[A]: ..."
    ~expect:
      "`TaintSink[(X, Unsupported)]` is an invalid taint annotation: Unsupported taint sink \
       `Unsupported`"
    ();
  assert_invalid_model
    ~model_source:"def test.sink(parameter: TaintSink[UserControlled]): ..."
    ~expect:
      "`TaintSink[UserControlled]` is an invalid taint annotation: Unsupported taint sink \
       `UserControlled`"
    ();
  assert_invalid_model
    ~model_source:"def test.sink(parameter: SkipAnalysis): ..."
    ~expect:
      "`SkipAnalysis` is an invalid taint annotation: Failed to parse the given taint annotation."
    ();
  assert_invalid_model
    ~model_source:"def test.sink(parameter: TaintSink[X, Y, LocalReturn]): ..."
    ~expect:
      "`TaintSink[(X, Y, LocalReturn)]` is an invalid taint annotation: Unsupported taint sink \
       `LocalReturn`"
    ();
  assert_invalid_model
    ~model_source:"def test.source() -> TaintSource[Invalid]: ..."
    ~expect:
      "`TaintSource[Invalid]` is an invalid taint annotation: Unsupported taint source `Invalid`"
    ();
  assert_invalid_model
    ~model_source:"def test.source() -> TaintInTaintOut: ..."
    ~expect:"Invalid model for `test.source`: Invalid return annotation `TaintInTaintOut`."
    ();
  assert_invalid_model
    ~model_source:"def test.sink(parameter: TaintInTaintOut[Test]): ..."
    ~expect:
      "`TaintInTaintOut[Test]` is an invalid taint annotation: Unsupported taint in taint out \
       specification `Test`"
    ();
  assert_invalid_model
    ~model_source:"def test.sink(parameter: TaintInTaintOut[Transform[Invalid]]): ..."
    ~expect:
      "`TaintInTaintOut[Transform[Invalid]]` is an invalid taint annotation: Unsupported transform \
       `Invalid`"
    ();
  assert_invalid_model
    ~model_source:"def test.sink(parameter: InvalidTaintDirection[Test]): ..."
    ~expect:
      "`InvalidTaintDirection[Test]` is an invalid taint annotation: Failed to parse the given \
       taint annotation."
    ();
  assert_invalid_model
    ~model_source:"def test.partial_sink(x: PartialSink[Test[first]], y: PartialSink[Test[b]]): ..."
    ~expect:
      "`PartialSink[Test[first]]` is an invalid taint annotation: Unrecognized label `first` for \
       partial sink `Test` (choices: `a, b`)"
    ();
  assert_invalid_model
    ~model_source:
      "def test.partial_sink(x: PartialSink[Test[a]], y: PartialSink[Test[second]]): ..."
    ~expect:
      "`PartialSink[Test[second]]` is an invalid taint annotation: Unrecognized label `second` for \
       partial sink `Test` (choices: `a, b`)"
    ();
  assert_invalid_model
    ~model_source:"def test.partial_sink(x: PartialSink[X[a]], y: PartialSink[X[b]]): ..."
    ~expect:"`PartialSink[X[a]]` is an invalid taint annotation: Unrecognized partial sink `X`."
    ();
  assert_invalid_model
    ~model_source:"def test.sink(parameter: TaintSource.foo(A)): ..."
    ~expect:
      {|`TaintSource.foo(A)` is an invalid taint annotation: Failed to parse the given taint annotation.|}
    ();

  (* Test invalid model queries. *)
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        find = "functions",
        where = name.matches("foo"),
        model = Returns(TaintSource[Test])
      )
    |}
    ~expect:
      {|The model query arguments at `{ Expression.Call.Argument.name = (Some find); value = "functions" }, { Expression.Call.Argument.name = (Some where); value = name.matches("foo") }, { Expression.Call.Argument.name = (Some model);
  value = Returns(TaintSource[Test]) }` are invalid: expected a name, find, where and model clause.|}
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        where = name.matches("foo"),
        model = Returns(TaintSource[Test])
      )
    |}
    ~expect:
      {|The model query arguments at `{ Expression.Call.Argument.name = (Some name); value = "invalid_model" }, { Expression.Call.Argument.name = (Some where); value = name.matches("foo") }, { Expression.Call.Argument.name = (Some model);
  value = Returns(TaintSource[Test]) }` are invalid: expected a name, find, where and model clause.|}
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        model = Returns(TaintSource[Test])
      )
    |}
    ~expect:
      {|The model query arguments at `{ Expression.Call.Argument.name = (Some name); value = "invalid_model" }, { Expression.Call.Argument.name = (Some find); value = "functions" }, { Expression.Call.Argument.name = (Some model);
  value = Returns(TaintSource[Test]) }` are invalid: expected a name, find, where and model clause.|}
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = name.matches("foo")
      )
    |}
    ~expect:
      {|The model query arguments at `{ Expression.Call.Argument.name = (Some name); value = "invalid_model" }, { Expression.Call.Argument.name = (Some find); value = "functions" }, { Expression.Call.Argument.name = (Some where); value = name.matches("foo") }` are invalid: expected a name, find, where and model clause.|}
    ();
  assert_invalid_model
    ~source:{|
      @d("1")
      def foo(x):
        ...
    |}
    ~model_source:
      {|
      ModelQuery(
        name = "same_name",
        find = "functions",
        where = Decorator(arguments.contains("1"), name.matches("d")),
        model = Returns(TaintSource[A])
      )
      ModelQuery(
        name = "same_name",
        find = "functions",
        where = Decorator(arguments.contains("1"), name.matches("d")),
        model = Returns(TaintSource[A])
      )
    |}
    ~expect:
      "Multiple model queries have the same name `same_name`. Model\n\
      \   query names should be unique within each file."
    ();
  assert_invalid_model
    ~source:{|
      @d("1")
      def foo(x):
        ...
    |}
    ~model_source:
      {|
      ModelQuery(
        name = "same_name",
        find = "functions",
        where = Decorator(arguments.contains("1"), name.matches("d")),
        model = Returns(TaintSource[A])
      )
      ModelQuery(
        name = "different_name",
        find = "functions",
        where = Decorator(arguments.contains("1"), name.matches("d")),
        model = Returns(TaintSource[A])
      )
      ModelQuery(
        name = "same_name",
        find = "functions",
        where = Decorator(arguments.contains("1"), name.matches("d")),
        model = Returns(TaintSource[A])
      )
    |}
    ~expect:
      "Multiple model queries have the same name `same_name`. Model\n\
      \   query names should be unique within each file."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "attributes",
        where = name.matches("foo"),
        model = Returns(TaintSource[Test])
      )
    |}
    ~expect:
      "`Returns` is not a valid model for model queries with find clause of kind `attributes`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "attributes",
        where = name.matches("foo"),
        model = NamedParameter(name="x", taint = TaintSource[Test, Via[foo]])
      )
    |}
    ~expect:
      "`NamedParameter` is not a valid model for model queries with find clause of kind \
       `attributes`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "attributes",
        where = name.matches("foo"),
        model = AllParameters(TaintSource[Test])
      )
    |}
    ~expect:
      "`AllParameters` is not a valid model for model queries with find clause of kind \
       `attributes`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = return_annotation.is_annotated_type(),
        model = AttributeModel(TaintSource[Test])
      )
    |}
    ~expect:
      "`AttributeModel` is not a valid model for model queries with find clause of kind \
       `functions`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = return_annotation.is_annotated_type(),
        model = AttributeModel(TaintSource[Test])
      )
    |}
    ~expect:
      "`AttributeModel` is not a valid model for model queries with find clause of kind `methods`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "attributes",
        where = any_parameter.annotation.is_annotated_type(),
        model = AttributeModel(TaintSource[Test])
      )
    |}
    ~expect:
      "`any_parameter.annotation.is_annotated_type` is not a valid constraint for model queries \
       with find clause of kind `attributes`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "attributes",
        where = AnyOf(
          parent.matches("foo"),
          any_parameter.annotation.is_annotated_type()
        ),
        model = AttributeModel(TaintSource[Test])
      )
    |}
    ~expect:
      "`any_parameter.annotation.is_annotated_type` is not a valid constraint for model queries \
       with find clause of kind `attributes`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "attributes",
        where = AnyOf(
          parent.matches("foo"),
          any_parameter.annotation.equals("int")
        ),
        model = AttributeModel(TaintSource[Test])
      )
    |}
    ~expect:
      "`any_parameter.annotation.equals` is not a valid constraint for model queries with find \
       clause of kind `attributes`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "attributes",
        where = AnyOf(
          parent.matches("foo"),
          any_parameter.annotation.matches("int")
        ),
        model = AttributeModel(TaintSource[Test])
      )
    |}
    ~expect:
      "`any_parameter.annotation.matches` is not a valid constraint for model queries with find \
       clause of kind `attributes`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "attributes",
        where = AnyOf(
          parent.matches("foo"),
          return_annotation.equals("int")
        ),
        model = AttributeModel(TaintSource[Test])
      )
    |}
    ~expect:
      "`return_annotation.equals` is not a valid constraint for model queries with find clause of \
       kind `attributes`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "attributes",
        where = AnyOf(
          parent.matches("foo"),
          return_annotation.matches("str")
        ),
        model = AttributeModel(TaintSource[Test])
      )
    |}
    ~expect:
      "`return_annotation.matches` is not a valid constraint for model queries with find clause of \
       kind `attributes`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "attributes",
        where = Decorator(name.matches("app.route")),
        model = AttributeModel(TaintSource[Test])
      )
    |}
    ~expect:
      "`Decorator` is not a valid constraint for model queries with find clause of kind \
       `attributes`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = parent.matches("foo"),
        model = Returns(TaintSource[Test])
      )
    |}
    ~expect:
      "`parent.matches` is not a valid constraint for model queries with find clause of kind \
       `functions`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = type_annotation.equals("int"),
        model = Returns(TaintSource[Test])
      )
    |}
    ~expect:
      "`type_annotation.equals` is not a valid constraint for model queries with find clause of \
       kind `functions`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = type_annotation.equals("int"),
        model = Returns(TaintSource[Test])
      )
    |}
    ~expect:
      "`type_annotation.equals` is not a valid constraint for model queries with find clause of \
       kind `methods`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = type_annotation.matches("int"),
        model = Returns(TaintSource[Test])
      )
    |}
    ~expect:
      "`type_annotation.matches` is not a valid constraint for model queries with find clause of \
       kind `functions`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = type_annotation.matches("int"),
        model = Returns(TaintSource[Test])
      )
    |}
    ~expect:
      "`type_annotation.matches` is not a valid constraint for model queries with find clause of \
       kind `methods`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = type_annotation.is_annotated_type(),
        model = Returns(TaintSource[Test])
      )
    |}
    ~expect:
      "`type_annotation.is_annotated_type` is not a valid constraint for model queries with find \
       clause of kind `functions`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = type_annotation.is_annotated_type(),
        model = Returns(TaintSource[Test])
      )
    |}
    ~expect:
      "`type_annotation.is_annotated_type` is not a valid constraint for model queries with find \
       clause of kind `methods`."
    ();

  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = parent.extends("foo", is_transitive=foobar),
        model = ReturnModel(TaintSource[Test])
      )
    |}
    ~expect:"The Extends is_transitive must be either True or False, got: `foobar`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = parent.extends("foo", foobar),
        model = ReturnModel(TaintSource[Test])
      )
    |}
    ~expect:
      "Unsupported arguments for callee `parent.extends`: `{ Expression.Call.Argument.name = None; \
       value = \"foo\" }, { Expression.Call.Argument.name = None; value = foobar }`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = parent.matches("foo", is_transitive=foobar),
        model = ReturnModel(TaintSource[Test])
      )
    |}
    ~expect:
      "Unsupported arguments for callee `parent.matches`: `{ Expression.Call.Argument.name = None; \
       value = \"foo\" }, { Expression.Call.Argument.name = (Some is_transitive); value = foobar \
       }`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = name.foo("foo"),
        model = ReturnModel(TaintSource[Test])
      )
    |}
    ~expect:"`name.foo(\"foo\")` is not a valid name clause."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = name.matches(foobar, 1, 2),
        model = ReturnModel(TaintSource[Test])
      )
    |}
    ~expect:
      "Unsupported arguments for callee `name.matches`: `{ Expression.Call.Argument.name = None; \
       value = foobar }, { Expression.Call.Argument.name = None; value = 1 }, { \
       Expression.Call.Argument.name = None; value = 2 }`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = name.equals(foobar, 1, 2),
        model = ReturnModel(TaintSource[Test])
      )
    |}
    ~expect:
      "Unsupported arguments for callee `name.equals`: `{ Expression.Call.Argument.name = None; \
       value = foobar }, { Expression.Call.Argument.name = None; value = 1 }, { \
       Expression.Call.Argument.name = None; value = 2 }`."
    ();

  assert_valid_model
    ~model_source:"def test.partial_sink(x: PartialSink[Test[a]], y: PartialSink[Test[b]]): ..."
    ();
  assert_valid_model ~model_source:"def test.sink(): ..." ();
  assert_valid_model ~model_source:"def test.sink_with_optional(): ..." ();
  assert_valid_model ~model_source:"def test.sink_with_optional(parameter): ..." ();
  assert_valid_model ~model_source:"def test.sink_with_optional(parameter, firstOptional): ..." ();
  assert_valid_model
    ~model_source:"def test.sink_with_optional(parameter, firstOptional, secondOptional): ..."
    ();
  assert_invalid_model
    ~model_source:
      "def test.sink_with_optional(parameter, firstOptional, secondOptional, thirdOptional): ..."
    ~expect:
      "Model signature parameters for `test.sink_with_optional` do not match implementation `def \
       sink_with_optional(parameter: unknown, firstOptional: unknown = ..., secondOptional: \
       unknown = ...) -> None: ...`. Reason: unexpected named parameter: `thirdOptional`."
    ();
  assert_invalid_model
    ~model_source:"def test.sink_with_optional(parameter, firstBad, secondBad): ..."
    ~expect:
      "Model signature parameters for `test.sink_with_optional` do not match implementation `def \
       sink_with_optional(parameter: unknown, firstOptional: unknown = ..., secondOptional: \
       unknown = ...) -> None: ...`. Reason: unexpected named parameter: `firstBad`."
    ();
  assert_invalid_model
    ~model_source:"def test.sink_with_optional(parameter, *args): ..."
    ~expect:
      "Model signature parameters for `test.sink_with_optional` do not match implementation `def \
       sink_with_optional(parameter: unknown, firstOptional: unknown = ..., secondOptional: \
       unknown = ...) -> None: ...`. Reason: unexpected star parameter."
    ();
  assert_invalid_model
    ~model_source:"def test.sink_with_optional(parameter, **kwargs): ..."
    ~expect:
      "Model signature parameters for `test.sink_with_optional` do not match implementation `def \
       sink_with_optional(parameter: unknown, firstOptional: unknown = ..., secondOptional: \
       unknown = ...) -> None: ...`. Reason: unexpected star star parameter."
    ();
  assert_invalid_model
    ~model_source:"def test.sink_with_optional(__parameter): ..."
    ~expect:
      "Model signature parameters for `test.sink_with_optional` do not match implementation `def \
       sink_with_optional(parameter: unknown, firstOptional: unknown = ..., secondOptional: \
       unknown = ...) -> None: ...`. Reason: unexpected positional only parameter: `__parameter`."
    ();
  assert_invalid_model
    ~model_source:"def test.function_with_args(normal_arg, __random_name, named_arg, *args): ..."
    ~expect:
      "Model signature parameters for `test.function_with_args` do not match implementation `def \
       function_with_args(normal_arg: unknown, unknown, *(unknown)) -> None: ...`. Reason: \
       unexpected named parameter: `named_arg`."
    ();
  assert_valid_model
    ~model_source:"def test.function_with_args(normal_arg, __random_name, *args): ..."
    ();
  assert_invalid_model
    ~model_source:"def test.function_with_args(normal_arg, __random_name, *, named_arg, *args): ..."
    ~expect:
      "Model signature parameters for `test.function_with_args` do not match implementation `def \
       function_with_args(normal_arg: unknown, unknown, *(unknown)) -> None: ...`. Reason: \
       unexpected named parameter: `named_arg`."
    ();
  assert_valid_model
    ~model_source:
      "def test.function_with_args(normal_arg, __random_name, __random_name_2, *args): ..."
    ();
  assert_valid_model ~model_source:"def test.function_with_kwargs(normal_arg, **kwargs): ..." ();
  assert_invalid_model
    ~model_source:"def test.function_with_kwargs(normal_arg, crazy_arg, **kwargs): ..."
    ~expect:
      "Model signature parameters for `test.function_with_kwargs` do not match implementation `def \
       function_with_kwargs(normal_arg: unknown, **(unknown)) -> None: ...`. Reason: unexpected \
       named parameter: `crazy_arg`."
    ();
  assert_valid_model ~model_source:"def test.function_with_overloads(__key): ..." ();
  assert_valid_model ~model_source:"def test.function_with_overloads(firstNamed): ..." ();
  assert_valid_model ~model_source:"def test.function_with_overloads(secondNamed): ..." ();
  assert_invalid_model
    ~model_source:"def test.function_with_overloads(unknownNamed): ..."
    ~expect:
      "Model signature parameters for `test.function_with_overloads` do not match implementation \
       `def function_with_overloads(str) -> Union[int, str]: ...`. Reason: unexpected named \
       parameter: `unknownNamed`."
    ();
  assert_invalid_model
    ~model_source:"def test.function_with_overloads(firstNamed, secondNamed): ..."
    ~expect:
      "Model signature parameters for `test.function_with_overloads` do not match implementation \
       `def function_with_overloads(str) -> Union[int, str]: ...`. Reasons:\n\
       unexpected named parameter: `secondNamed` in overload `(str) -> Union[int, str]`\n\
       unexpected named parameter: `firstNamed` in overload `(str) -> Union[int, str]`\n\
       unexpected named parameter: `secondNamed` in overload `(str, firstNamed: int) -> int`\n\
       unexpected named parameter: `firstNamed` in overload `(str, secondNamed: str) -> str`"
    ();
  assert_invalid_model
    ~model_source:"def test.function_with_multiple_positions(c): ..."
    ~expect:
      "Model signature parameters for `test.function_with_multiple_positions` do not match \
       implementation `def function_with_multiple_positions(a: int, b: int, c: int) -> Union[int, \
       str]: ...`. Reason: invalid position for named parameter `c` (0 not in {1, 2})."
    ();
  assert_invalid_model
    ~model_source:"def test.function_with_positional_and_named(__x): ..."
    ~expect:
      "Model signature parameters for `test.function_with_positional_and_named` do not match \
       implementation `def function_with_positional_and_named(a: str, str, str, b: str) -> None: \
       ...`. Reason: unexpected positional only parameter: `__x` at position: 0 (0 not in {1, 2})."
    ();
  assert_valid_model
    ~model_source:"def test.function_with_positional_and_named(a, __random_name): ..."
    ();
  assert_valid_model
    ~model_source:"def test.function_with_positional_and_named(a, __random_name, b): ..."
    ();
  assert_invalid_model
    ~model_source:"def test.function_with_positional_and_named(a, __x, b, __y): ..."
    ~expect:
      "Model signature parameters for `test.function_with_positional_and_named` do not match \
       implementation `def function_with_positional_and_named(a: str, str, str, b: str) -> None: \
       ...`. Reason: unexpected positional only parameter: `__y` at position: 3 (3 not in {1, 2})."
    ();
  assert_valid_model
    ~model_source:"def test.function_with_kwargs(normal_arg, *, crazy_arg, **kwargs): ..."
    ();
  assert_valid_model ~model_source:"def test.anonymous_only(__a1, __a2, __a3): ..." ();
  assert_invalid_model
    ~model_source:"def test.anonymous_only(parameter: Any): ..."
    ~expect:
      "Model signature parameters for `test.anonymous_only` do not match implementation `def \
       anonymous_only(unknown, unknown, unknown) -> None: ...`. Reason: unexpected named \
       parameter: `parameter`."
    ();
  assert_valid_model ~model_source:"def test.anonymous_with_optional(__a1, __a2): ..." ();
  assert_valid_model ~model_source:"def test.anonymous_with_optional(__a1, __a2, __a3=...): ..." ();
  assert_invalid_model
    ~model_source:"def test.sink(parameter: Any): ..."
    ~expect:"`Any` is an invalid taint annotation: Failed to parse the given taint annotation."
    ();
  assert_invalid_model
    ~path:"broken_model.pysa"
    ~model_source:"def test.sink(parameter: Any): ..."
    ~expect:
      "broken_model.pysa:1: `Any` is an invalid taint annotation: Failed to parse the given taint \
       annotation."
    ();
  assert_invalid_model
    ~model_source:"def test.sink(parameter: TaintSink[Test, Via[bad_feature]]): ..."
    ~expect:
      "`TaintSink[(Test, Via[bad_feature])]` is an invalid taint annotation: Unrecognized Via \
       annotation `bad_feature`"
    ();
  assert_invalid_model
    ~model_source:"def test.sink(parameter: TaintSink[Updates[self]]): ..."
    ~expect:"`TaintSink[Updates[self]]` is an invalid taint annotation: No such parameter `self`"
    ();
  assert_valid_model ~model_source:"test.unannotated_global: TaintSink[Test]" ();
  assert_invalid_model
    ~model_source:"test.missing_global: TaintSink[Test]"
    ~expect:"Module `test` does not define `test.missing_global`."
    ();
  assert_invalid_model
    ~source:{|
      class C:
        a: int = 1
     |}
    ~model_source:"test.C.b: TaintSink[Test] = ..."
    ~expect:"Class `test.C` has no attribute `b`."
    ();
  assert_valid_model ~model_source:"test.C.unannotated_class_variable: TaintSink[Test]" ();
  assert_invalid_model
    ~model_source:"test.C.missing: TaintSink[Test]"
    ~expect:"Class `test.C` has no attribute `missing`."
    ();
  assert_invalid_model
    ~model_source:"test.C().unannotated_class_variable: TaintSink[Test]"
    ~expect:
      "Invalid identifier: `test.C().unannotated_class_variable`. Expected a fully-qualified name."
    ();
  assert_invalid_model
    ~model_source:"test.C.unannotated_class_variable: Foo"
    ~expect:"`Foo` is an invalid taint annotation: Unsupported annotation for attributes"
    ();
  assert_invalid_model
    ~model_source:
      {|
      class test.ClassSinkWithMethod(TaintSink[TestSink]):
          def method(self): ...
      |}
    ~expect:"Class model for `test.ClassSinkWithMethod` must have a body of `...`."
    ();
  assert_invalid_model
    ~model_source:{|
      def foo(): TaintSource[A]
    |}
    ~expect:"Callable model for `foo` must have a body of `...`."
    ();
  (* Attach syntax. *)
  assert_invalid_model
    ~model_source:"def test.sink(parameter: AttachToSink): ..."
    ~expect:
      "`AttachToSink` is an invalid taint annotation: Failed to parse the given taint annotation."
    ();
  assert_invalid_model
    ~model_source:"def test.sink(parameter: AttachToSink[feature]): ..."
    ~expect:
      "`AttachToSink[feature]` is an invalid taint annotation: All parameters to `AttachToSink` \
       must be of the form `Via[feature]`."
    ();
  assert_invalid_model
    ~model_source:"def test.sink(parameter: AttachToTito[feature]): ..."
    ~expect:
      "`AttachToTito[feature]` is an invalid taint annotation: All parameters to `AttachToTito` \
       must be of the form `Via[feature]`."
    ();
  assert_invalid_model
    ~model_source:"def test.source() -> AttachToSource[feature]: ..."
    ~expect:
      "`AttachToSource[feature]` is an invalid taint annotation: All parameters to \
       `AttachToSource` must be of the form `Via[feature]`."
    ();

  (* Multiple features. *)
  assert_valid_model
    ~model_source:"def test.sink(parameter: AttachToSink[Via[featureA, featureB]]): ..."
    ();

  (* Default values must be `...`. *)
  assert_invalid_model
    ~model_source:"def test.sink(parameter = TaintSink[Test]): ..."
    ~expect:
      "Default values of `test.sink`'s parameters must be `...`. Did you mean to write `parameter: \
       TaintSink[Test]`?"
    ();
  assert_invalid_model
    ~model_source:"def test.sink(parameter = 1): ..."
    ~expect:
      "Default values of `test.sink`'s parameters must be `...`. Did you mean to write `parameter: \
       1`?"
    ();

  (* ViaValueOf models must specify existing parameters. *)
  assert_invalid_model
    ~model_source:
      "def test.sink(parameter) -> TaintSource[Test, ViaValueOf[nonexistent_parameter]]: ..."
    ~expect:
      "`TaintSource[(Test, ViaValueOf[nonexistent_parameter])]` is an invalid taint annotation: No \
       such parameter `nonexistent_parameter`"
    ();
  assert_invalid_model
    ~source:
      {|
    class C:
      @property
      def foo(self) -> int:
        return self.x
      @foo.setter
      def foo(self, value) -> None:
        self.x = value
    |}
    ~model_source:
      {|
      @property
      def test.C.foo(self, value) -> TaintSource[Test]: ...
    |}
    ~expect:
      "Model signature parameters for `test.C.foo` do not match implementation `(self: C) -> int`. \
       Reason: unexpected named parameter: `value`."
    ();
  assert_valid_model
    ~source:
      {|
    class C:
      @property
      def foo(self) -> int:
        return self.x
      @foo.setter
      def foo(self, value: int) -> None:
        self.x = value
    |}
    ~model_source:{|
      @foo.setter
      def test.C.foo(self) -> TaintSource[A]: ...
    |}
    ();
  assert_invalid_model
    ~model_source:
      {|
      @decorated
      def accidental_decorator_passed_in() -> TaintSource[Test]: ...
    |}
    ~expect:
      "Unexpected decorators found when parsing model for `accidental_decorator_passed_in`: \
       `decorated`."
    ();
  assert_invalid_model
    ~source:
      {|
      class C:
        @property
        def foo(self) -> int: ...
        @foo.setter
        def foo(self, value) -> None: ...
    |}
    ~model_source:
      {|
      @wrong_name.setter
      def test.C.foo(self, value: TaintSink[Test]): ...
    |}
    ~expect:"Unexpected decorators found when parsing model for `test.C.foo`: `wrong_name.setter`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      @custom_property
      def accidental_decorator_passed_in() -> TaintSource[Test]: ...
    |}
    ~expect:
      "Unexpected decorators found when parsing model for `accidental_decorator_passed_in`: \
       `custom_property`. If you're looking to model a custom property decorator, use the \
       @property decorator."
    ();

  assert_valid_model
    ~source:
      {|
      class C:
        @property
        def foo(self) -> int: ...
        @foo.setter
        def foo(self, value) -> None: ...
    |}
    ~model_source:
      {|
      @foo.setter
      def test.C.foo(self, value: TaintSink[Test]): ...
    |}
    ();
  assert_invalid_model
    ~model_source:
      {|
      def unittest.TestCase.assertIsNotNone(self, x: TaintSink[Test]): ...
    |}
    ~expect:
      "The modelled function `unittest.TestCase.assertIsNotNone` is an imported function, please \
       model `unittest.case.TestCase.assertIsNotNone` directly."
    ();
  assert_invalid_model
    ~model_source:
      {|
        def test.sink(parameter: TaintSink[Test, Via[a-feature]]):
          ...
    |}
    ~expect:
      "`TaintSink[(Test, Via[a.__sub__(feature)])]` is an invalid taint annotation: Invalid \
       expression for breadcrumb: (Expression.Expression.Call\n\
      \   { Expression.Call.callee = a.__sub__;\n\
      \     arguments = [{ Expression.Call.Argument.name = None; value = feature }]\n\
      \     })"
    ();
  assert_invalid_model
    ~source:"def partial_sink(x, y) -> None: ..."
    ~model_source:
      "def test.partial_sink(x: PartialSink[Nonexistent[a]], y: PartialSink[Nonexistent[b]]): ..."
    ~expect:
      "`PartialSink[Nonexistent[a]]` is an invalid taint annotation: Unrecognized partial sink \
       `Nonexistent`."
    ();
  assert_invalid_model
    ~source:"def f(parameter): ..."
    ~model_source:"def test.f(parameter: CrossRepositoryTaint[TaintSource[UserControlled]]): ..."
    ~expect:
      "`CrossRepositoryTaint[TaintSource[UserControlled]]` is an invalid taint annotation: Cross \
       repository taint must be of the form CrossRepositoryTaint[taint, canonical_name, \
       canonical_port, producer_id, trace_length]."
    ();
  assert_invalid_model
    ~source:"def f(parameter): ..."
    ~model_source:
      "def test.f(parameter: CrossRepositoryTaint[TaintSource[UserControlled], \
       some_canonical_name, 'formal(0)', 0]): ..."
    ~expect:
      "`CrossRepositoryTaint[(TaintSource[UserControlled], some_canonical_name, \"formal(0)\", \
       0)]` is an invalid taint annotation: Cross repository taint must be of the form \
       CrossRepositoryTaint[taint, canonical_name, canonical_port, producer_id, trace_length]."
    ();
  assert_invalid_model
    ~source:"def f(parameter): ..."
    ~model_source:
      "def test.f(parameter: CrossRepositoryTaint[TaintSource[UserControlled], \
       'some_canonical_name', 0, 0]): ..."
    ~expect:
      "`CrossRepositoryTaint[(TaintSource[UserControlled], \"some_canonical_name\", 0, 0)]` is an \
       invalid taint annotation: Cross repository taint must be of the form \
       CrossRepositoryTaint[taint, canonical_name, canonical_port, producer_id, trace_length]."
    ();
  assert_invalid_model
    ~source:"def f(parameter): ..."
    ~model_source:
      "def test.f(parameter: CrossRepositoryTaint[TaintSource[UserControlled], 'canonical_name', \
       'formal(x)', 0, 'oh']): ..."
    ~expect:
      "`CrossRepositoryTaint[(TaintSource[UserControlled], \"canonical_name\", \"formal(x)\", 0, \
       \"oh\")]` is an invalid taint annotation: Cross repository taint must be of the form \
       CrossRepositoryTaint[taint, canonical_name, canonical_port, producer_id, trace_length]."
    ();
  (* Ensure that we're verifying models against the undecorated signature. *)
  assert_valid_model
    ~source:
      {|
    from typing import Callable
    def decorate(f: Callable[[int], int]) -> Callable[[], int]:
      def g() -> int:
        return f(42)
      return g
    @decorate
    def foo(parameter: int) -> int:
      return parameter
    |}
    ~model_source:"def test.foo(parameter): ..."
    ();
  assert_valid_model
    ~source:
      {|
    from typing import Callable
    def decorate(f: Callable[[int], int]) -> Callable[[], int]:
      def g() -> int:
        return f(42)
      return g
    @decorate
    def foo(parameter: int) -> int:
      return parameter
    |}
    ~model_source:"def test.foo(): ..."
    ();
  assert_valid_model
    ~source:{|
      def foo(__: int, kwonly: str) -> None: ...
    |}
    ~model_source:"def test.foo(__: TaintSource[A], kwonly): ..."
    ();
  assert_invalid_model
    ~source:{|
      class C:
        @property
        def foo(self) -> int: ...
    |}
    ~model_source:"test.C.foo: TaintSource[A] = ..."
    ~expect:
      "The function, method or property `test.C.foo` is not a valid attribute - did you mean to \
       use `def test.C.foo(): ...`?"
    ();
  assert_invalid_model
    ~source:{|
      class C:
        foo = 1
      class D(C):
        pass
    |}
    ~model_source:"test.D.foo: TaintSource[A] = ..."
    ~expect:"Class `test.D` has no attribute `foo`."
    ();
  assert_valid_model
    ~source:{|
      class C:
        foo = 1
      class D(C):
        pass
    |}
    ~model_source:"test.C.foo: TaintSource[A] = ..."
    ();
  assert_valid_model
    ~source:
      {|
      class C:
        foo = 1
      class D(C):
        def __init__(self):
          self.foo = 2
    |}
    ~model_source:"test.D.foo: TaintSource[A] = ..."
    ();
  assert_invalid_model
    ~source:{|
      def foo(x):
        ...
    |}
    ~model_source:"def test.foo(x) -> TaintSource[A[Subkind]]: ..."
    ~expect:"`TaintSource[A[Subkind]]` is an invalid taint annotation: Unsupported taint source `A`"
    ();
  assert_invalid_model
    ~source:{|
      def foo(x):
        ...
    |}
    ~model_source:"def test.foo(x: TaintSink[X[Subkind]]): ..."
    ~expect:"`TaintSink[X[Subkind]]` is an invalid taint annotation: Unsupported taint sink `X`"
    ();
  assert_invalid_model
    ~source:{|
      def foo(x):
        ...
    |}
    ~model_source:{|
      @Sanitize(TaintInTaintOut[LocalReturn])
      def test.foo(x): ...
    |}
    ~expect:
      {|`Sanitize(TaintInTaintOut[LocalReturn])` is an invalid taint annotation: Failed to parse the given taint annotation.|}
    ();

  (* Test source- and sink- specific tito parsing. *)
  assert_valid_model
    ~source:{|
      def foo(x):
        ...
    |}
    ~model_source:
      {|
      @Sanitize(TaintInTaintOut[TaintSource[A]])
      def test.foo(x): ...
    |}
    ();
  assert_valid_model
    ~source:{|
      def foo(x):
        ...
    |}
    ~model_source:
      {|
      @Sanitize(TaintInTaintOut[TaintSink[Test]])
      def test.foo(x): ...
    |}
    ();
  assert_invalid_model
    ~source:{|
      def foo(x):
        ...
    |}
    ~model_source:{|
      @Sanitize(TaintSource[A])
      def test.foo(x): ...
    |}
    ~expect:
      "`Sanitize(TaintSource[A])` is an invalid taint annotation: `TaintSource` is not supported \
       within `Sanitize(...)`. Did you mean to use `SanitizeSingleTrace(...)`?"
    ();
  assert_invalid_model
    ~source:{|
      def foo(x):
        ...
    |}
    ~model_source:
      {|
      @Sanitize(TaintSource[A, Via[featureA]])
      def test.foo(x): ...
    |}
    ~expect:
      {|`Sanitize(TaintSource[(A, Via[featureA])])` is an invalid taint annotation: `ModelParser.Internal.Source {source = A;
   breadcrumbs = [SimpleVia[featureA]]; via_features = []; path = ;
   leaf_names = []; leaf_name_provided = false; trace_length = None}` is not supported within `Sanitize[...]`|}
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        fnid = "functions",
        where = name.matches("foo"),
        model = Returns(TaintSource[Test])
      )
    |}
    ~expect:
      {|The model query arguments at `{ Expression.Call.Argument.name = (Some name); value = "invalid_model" }, { Expression.Call.Argument.name = (Some fnid); value = "functions" }, { Expression.Call.Argument.name = (Some where); value = name.matches("foo") }, { Expression.Call.Argument.name = (Some model);
  value = Returns(TaintSource[Test]) }` are invalid: expected a name, find, where and model clause.|}
    ();
  (* Test Decorator clause in model queries *)
  assert_valid_model
    ~source:{|
      @d("1")
      def foo(x):
        ...
    |}
    ~model_source:
      {|
      ModelQuery(
        name = "valid_model",
        find = "functions",
        where = Decorator(arguments.contains("1"), name.matches("d")),
        model = Returns(TaintSource[A])
      )
    |}
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = Decorator(foo),
        model = Returns(TaintSource[Test])
      )
    |}
    ~expect:"`foo` is not a valid name clause."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = Decorator(name.matches("a"), name.matches("b")),
        model = Returns(TaintSource[Test])
      )
    |}
    ~expect:"`name.matches(\"a\")` is not a valid arguments clause."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = Decorator(arguments.contains("a")),
        model = Returns(TaintSource[Test])
      )
    |}
    ~expect:"`arguments.contains(\"a\")` is not a valid name clause."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = Decorator(arguments.contains("a"), arguments.contains("b")),
        model = Returns(TaintSource[Test])
      )
    |}
    ~expect:"`arguments.contains(\"b\")` is not a valid name clause."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = Decorator(name.matches(a, b)),
        model = Returns(TaintSource[Test])
      )
    |}
    ~expect:
      {|Unsupported arguments for callee `name.matches`: `{ Expression.Call.Argument.name = None; value = a }, { Expression.Call.Argument.name = None; value = b }`.|}
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = Decorator(name.equals(a, b), arguments.equals(1, 2)),
        model = Returns(TaintSource[Test])
      )
    |}
    ~expect:{|`arguments.equals(1, 2)` is not a valid name clause.|}
    ();

  (* We error starting on the first decorator. *)
  assert_invalid_model
    ~path:"a.py"
    ~model_source:{|
      @Sanitize
      def a.not_in_environment(self): ...
    |}
    ~expect:
      "a.py:2: `a.not_in_environment` is not part of the environment, no module `a` in search path."
    ();
  assert_invalid_model
    ~path:"a.py"
    ~model_source:
      {|
      @Sanitize(TaintSink)
      @Sanitize(TaintSource)
      def a.not_in_environment(self): ...
    |}
    ~expect:
      "a.py:2: `a.not_in_environment` is not part of the environment, no module `a` in search path."
    ();
  assert_invalid_model
    ~source:{|
      class C:
          def __init__(self, x) -> None:
              ...
    |}
    ~model_source:{|
      @Sanitize
      def test.C(): ...
    |}
    ~expect:"The class `test.C` is not a valid define - did you mean to model `test.C.__init__()`?"
    ();
  assert_invalid_model
    ~source:{|
      class C:
        pass
    |}
    ~model_source:{|
      test.C: Sanitize
    |}
    ~expect:
      "The class `test.C` is not a valid attribute - did you mean to model `test.C.__init__()`?"
    ();
  (* Class models don't error. *)
  assert_valid_model
    ~source:{|
      class C:
          def __init__(self, x) -> None:
              ...
    |}
    ~model_source:{|
      @Sanitize
      class test.C: ...
    |}
    ();

  (* Error on non-existenting callables. *)
  assert_invalid_model
    ~model_source:"def not_in_the_environment(parameter: InvalidTaintDirection[Test]): ..."
    ~expect:
      "`not_in_the_environment` is not part of the environment, no module `not_in_the_environment` \
       in search path."
    ();
  assert_invalid_model
    ~model_source:"def not_in_the_environment.derp(parameter: InvalidTaintDirection[Test]): ..."
    ~expect:
      "`not_in_the_environment.derp` is not part of the environment, no module \
       `not_in_the_environment` in search path."
    ();
  assert_invalid_model
    ~model_source:"def test(parameter: InvalidTaintDirection[Test]): ..."
    ~expect:"The module `test` is not a valid define."
    ();
  assert_invalid_model
    ~model_source:"test: Sanitize"
    ~expect:"The module `test` is not a valid attribute."
    ();
  assert_invalid_model
    ~source:{|
      class Foo:
        x: int = 1
    |}
    ~model_source:{|
      def test.Foo.x(self) -> TaintSource[Test]: ...
    |}
    ~expect:
      "The attribute `test.Foo.x` is not a valid define - did you mean to use `test.Foo.x: ...`?"
    ();
  assert_invalid_model
    ~source:{|
      def foo():
        return
    |}
    ~model_source:{|
      test.foo: TaintSource[Test]
    |}
    ~expect:
      "The function, method or property `test.foo` is not a valid attribute - did you mean to use \
       `def test.foo(): ...`?"
    ();
  (* Decorators. *)
  assert_valid_model
    ~source:
      {|
        class Foo:
          @unknown_decorator
          def bar(self):
            pass
      |}
    ~model_source:{|
      def test.Foo.bar() -> TaintSource[A]: ...
    |}
    ();
  assert_invalid_model
    ~source:
      {|
        class Foo:
          @unknown_decorator
          def bar(self):
            pass
      |}
    ~model_source:{|
      test.Foo.bar: TaintSource[A]
    |}
    ~expect:
      "The function, method or property `test.Foo.bar` is not a valid attribute - did you mean to \
       use `def test.Foo.bar(): ...`?"
    ();
  (* Accept callable models for Any or Top because of type error. *)
  assert_valid_model
    ~source:
      {|
        class Foo:
          def bar(self):
            pass
          baz = bar
      |}
    ~model_source:{|
      def test.Foo.baz() -> TaintSource[A]: ...
    |}
    ();
  assert_valid_model
    ~source:
      {|
        class Foo:
          def bar(self):
            pass
          baz = bar
      |}
    ~model_source:{|
      test.Foo.baz: TaintSource[A]
    |}
    ();
  (* Overloads *)
  assert_valid_model
    ~sources:
      [
        ( "test.pyi",
          {|
            from typing import overload
            class Foo:
              @overload
              def bar(self, x: int) -> str: ...
              @overload
              def bar(self, x: str) -> int: ...
          |}
        );
      ]
    ~model_source:{|
      def test.Foo.bar(self, x: TaintSink[Test]): ...
    |}
    ();
  assert_invalid_model
    ~sources:
      [
        ( "test.pyi",
          {|
            from typing import overload
            class Foo:
              @overload
              def bar(self, x: int) -> str: ...
              @overload
              def bar(self, x: str) -> int: ...
          |}
        );
      ]
    ~model_source:{|
      test.Foo.bar: TaintSink[Test]
    |}
    ~expect:
      "The function, method or property `test.Foo.bar` is not a valid attribute - did you mean to \
       use `def test.Foo.bar(): ...`?"
    ();
  assert_invalid_model
    ~source:
      {|
      class Parent:
        def foo(self) -> int: ...
      class Child(Parent):
        pass
    |}
    ~model_source:{|
      def test.Child.foo(self) -> TaintSource[Test]: ...
    |}
    ~expect:
      "The modelled function `test.Child.foo` is an imported function, please model \
       `test.Parent.foo` directly."
    ();
  assert_invalid_model
    ~source:
      {|
      class Parent:
        @property
        def foo(self) -> int: ...
      class Child(Parent):
        pass
    |}
    ~model_source:{|
      @property
      def test.Child.foo(self) -> TaintSource[Test]: ...
    |}
    ~expect:"Module `test` does not define `test.Child.foo`."
    ();
  assert_invalid_model
    ~source:{|
      class C:
        x = ...
      |}
    ~model_source:{|
      test.C.x: Sanitize[TaintInTaintOut[TaintSource[Test]]] = ...
    |}
    ~expect:
      "`Sanitize[TaintInTaintOut[TaintSource[Test]]]` is an invalid taint annotation: \
       TaintInTaintOut sanitizers cannot be modelled on attributes"
    ();
  assert_invalid_model
    ~source:{|
      class C:
        x = ...
      |}
    ~model_source:{|
      test.C.x: Sanitize[TaintInTaintOut[TaintSink[Test]]] = ...
    |}
    ~expect:
      "`Sanitize[TaintInTaintOut[TaintSink[Test]]]` is an invalid taint annotation: \
       TaintInTaintOut sanitizers cannot be modelled on attributes"
    ();

  (* ViaTypeOf on attributes *)
  assert_invalid_model
    ~source:{|
      def foo(x: int) -> int: ...
      |}
    ~model_source:{|
      def test.foo(x: ViaTypeOf): ...
    |}
    ~expect:
      {|`ViaTypeOf` is an invalid taint annotation: A standalone `ViaTypeOf` without arguments can only be used in attribute or global models.|}
    ();
  assert_invalid_model
    ~source:{|
      def foo(x: int) -> int: ...
      |}
    ~model_source:{|
      def test.foo(x) -> ViaTypeOf: ...
    |}
    ~expect:
      {|`ViaTypeOf` is an invalid taint annotation: A standalone `ViaTypeOf` without arguments can only be used in attribute or global models.|}
    ();
  assert_invalid_model
    ~source:{|
      class C:
        def foo(x: int) -> int: ...
      |}
    ~model_source:{|
      def test.C.foo(x: ViaTypeOf): ...
    |}
    ~expect:
      {|`ViaTypeOf` is an invalid taint annotation: A standalone `ViaTypeOf` without arguments can only be used in attribute or global models.|}
    ();
  assert_invalid_model
    ~source:{|
      class C:
        def foo(x: int) -> int: ...
      |}
    ~model_source:{|
      def test.C.foo() -> ViaTypeOf: ...
    |}
    ~expect:
      {|`ViaTypeOf` is an invalid taint annotation: A standalone `ViaTypeOf` without arguments can only be used in attribute or global models.|}
    ();
  assert_valid_model
    ~source:{|
      class C:
        x: int = 0
      |}
    ~model_source:{|
      test.C.x: ViaTypeOf = ...
    |}
    ();
  assert_valid_model
    ~source:{|
      class C:
        x: int = 0
      |}
    ~model_source:{|
      test.C.x: TaintInTaintOut[ViaTypeOf] = ...
    |}
    ();
  assert_valid_model
    ~source:{|
      class C:
        x: int = 0
      |}
    ~model_source:{|
      test.C.x: TaintSource[A, ViaTypeOf] = ...
    |}
    ();
  assert_valid_model
    ~source:{|
      class C:
        x: int = 0
      |}
    ~model_source:{|
      test.C.x: TaintSink[X, ViaTypeOf] = ...
    |}
    ();
  ()


let test_demangle_class_attributes _ =
  let assert_demangle ~expected name =
    assert_equal expected (ModelVerifier.demangle_class_attribute name)
  in
  assert_demangle ~expected:"a.B" "a.B";
  assert_demangle ~expected:"a.B" "a.__class__.B";

  (* We require `__class__` to directly precede the attribute of the `.`-separated names. *)
  assert_demangle ~expected:"a.B.__class__" "a.B.__class__";
  assert_demangle ~expected:"a.__class__.B.C" "a.__class__.B.C"


let test_filter_by_rules context =
  let assert_model = assert_model ~context in
  assert_model
    ~rules:
      [
        {
          Taint.TaintConfiguration.Rule.sources = [Sources.NamedSource "TestTest"];
          sinks = [Sinks.NamedSink "TestSink"];
          transforms = [];
          code = 5021;
          message_format = "";
          name = "test rule";
        };
      ]
    ~model_source:"def test.taint() -> TaintSource[TestTest]: ..."
    ~expect:[outcome ~kind:`Function ~returns:[Sources.NamedSource "TestTest"] "test.taint"]
    ();
  assert_model
    ~rules:
      [
        {
          Taint.TaintConfiguration.Rule.sources = [Sources.NamedSource "Test"];
          sinks = [Sinks.NamedSink "TestSink"];
          transforms = [];
          code = 5021;
          message_format = "";
          name = "test rule";
        };
      ]
    ~model_source:"def test.taint() -> TaintSource[TestTest]: ..."
    ~expect:[outcome ~kind:`Function ~returns:[] "test.taint"]
    ();
  assert_model
    ~rules:
      [
        {
          Taint.TaintConfiguration.Rule.sources = [Sources.NamedSource "TestTest"];
          sinks = [Sinks.NamedSink "TestSink"];
          transforms = [];
          code = 5021;
          message_format = "";
          name = "test rule";
        };
      ]
    ~model_source:"def test.taint(x: TaintSink[TestSink]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~sink_parameters:[{ name = "x"; sinks = [Sinks.NamedSink "TestSink"] }]
          "test.taint";
      ]
    ();
  assert_model
    ~rules:
      [
        {
          Taint.TaintConfiguration.Rule.sources = [Sources.NamedSource "TestTest"];
          sinks = [Sinks.NamedSink "Test"];
          transforms = [];
          code = 5021;
          message_format = "";
          name = "test rule";
        };
      ]
    ~model_source:"def test.taint(x: TaintSink[TestSink]): ..."
    ~expect:[outcome ~kind:`Function ~sink_parameters:[] "test.taint"]
    ();
  assert_model
    ~rules:
      [
        {
          Taint.TaintConfiguration.Rule.sources = [Sources.NamedSource "TestTest"];
          sinks = [Sinks.TriggeredPartialSink { kind = "Test"; label = "a" }];
          transforms = [];
          code = 4321;
          message_format = "";
          name = "test multiple sources rule";
        };
        {
          Taint.TaintConfiguration.Rule.sources = [Sources.NamedSource "TestTest"];
          sinks = [Sinks.TriggeredPartialSink { kind = "Test"; label = "b" }];
          transforms = [];
          code = 4321;
          message_format = "";
          name = "test multiple sources rule";
        };
      ]
    ~model_source:"def test.partial_sink(x: PartialSink[Test[a]], y: PartialSink[Test[b]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~sink_parameters:
            [
              { name = "x"; sinks = [Sinks.PartialSink { kind = "Test"; label = "a" }] };
              { name = "y"; sinks = [Sinks.PartialSink { kind = "Test"; label = "b" }] };
            ]
          "test.partial_sink";
      ]
    ()


let test_query_parsing context =
  let open ModelParser.Internal.ModelQuery in
  let module ModelParser = ModelParser.Internal in
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "functions",
     where = name.matches("foo"),
     model = Returns(TaintSource[Test])
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          query = [NameConstraint (Matches (Re2.create_exn "foo"))];
          rule_kind = FunctionModel;
          productions =
            [
              ReturnTaint
                [
                  TaintAnnotation
                    (Source
                       {
                         source = Sources.NamedSource "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                ];
            ];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "functions",
     where = name.matches("foo"),
     model = [Returns(TaintSource[Test])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          query = [NameConstraint (Matches (Re2.create_exn "foo"))];
          rule_kind = FunctionModel;
          productions =
            [
              ReturnTaint
                [
                  TaintAnnotation
                    (Source
                       {
                         source = Sources.NamedSource "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                ];
            ];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "functions",
     where = [name.matches("foo"), name.matches("bar")],
     model = Returns(TaintSource[Test])
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          query =
            [
              NameConstraint (Matches (Re2.create_exn "foo"));
              NameConstraint (Matches (Re2.create_exn "bar"));
            ];
          rule_kind = FunctionModel;
          productions =
            [
              ReturnTaint
                [
                  TaintAnnotation
                    (ModelParser.Source
                       {
                         source = Sources.NamedSource "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                ];
            ];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "functions",
     where = name.matches("foo"),
     model = [Returns([TaintSource[Test], TaintSink[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          query = [NameConstraint (Matches (Re2.create_exn "foo"))];
          rule_kind = FunctionModel;
          productions =
            [
              ReturnTaint
                [
                  TaintAnnotation
                    (ModelParser.Source
                       {
                         source = Sources.NamedSource "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                  TaintAnnotation
                    (ModelParser.Sink
                       {
                         sink = Sinks.NamedSink "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                ];
            ];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "functions",
     where = name.equals("test.foo"),
     model = [Returns([TaintSource[Test], TaintSink[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          query = [NameConstraint (Equals "test.foo")];
          rule_kind = FunctionModel;
          productions =
            [
              ReturnTaint
                [
                  TaintAnnotation
                    (ModelParser.Source
                       {
                         source = Sources.NamedSource "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                  TaintAnnotation
                    (ModelParser.Sink
                       {
                         sink = Sinks.NamedSink "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                ];
            ];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "foo_finders",
     find = "functions",
     where = name.matches("foo"),
     model = [Returns([TaintSource[Test], TaintSink[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "foo_finders";
          query = [NameConstraint (Matches (Re2.create_exn "foo"))];
          rule_kind = FunctionModel;
          productions =
            [
              ReturnTaint
                [
                  TaintAnnotation
                    (ModelParser.Source
                       {
                         source = Sources.NamedSource "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                  TaintAnnotation
                    (ModelParser.Sink
                       {
                         sink = Sinks.NamedSink "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                ];
            ];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
       name = "foo_finders",
       find = "functions",
       where = return_annotation.is_annotated_type(),
       model = [Returns([TaintSource[Test]])]
    )
    |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "foo_finders";
          query = [ReturnConstraint IsAnnotatedTypeConstraint];
          rule_kind = FunctionModel;
          productions =
            [
              ReturnTaint
                [
                  TaintAnnotation
                    (ModelParser.Source
                       {
                         source = Sources.NamedSource "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                ];
            ];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
       name = "foo_finders",
       find = "functions",
       where = any_parameter.annotation.is_annotated_type(),
       model = [Returns([TaintSource[Test]])]
    )
    |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "foo_finders";
          query = [AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint)];
          rule_kind = FunctionModel;
          productions =
            [
              ReturnTaint
                [
                  TaintAnnotation
                    (ModelParser.Source
                       {
                         source = Sources.NamedSource "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                ];
            ];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
       name = "foo_finders",
       find = "functions",
       where = AnyOf(
         any_parameter.annotation.is_annotated_type(),
         return_annotation.is_annotated_type(),
       ),
       model = [Returns([TaintSource[Test]])]
    )
    |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 10; column = 1 } };
          name = "foo_finders";
          query =
            [
              AnyOf
                [
                  AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint);
                  ReturnConstraint IsAnnotatedTypeConstraint;
                ];
            ];
          rule_kind = FunctionModel;
          productions =
            [
              ReturnTaint
                [
                  TaintAnnotation
                    (ModelParser.Source
                       {
                         source = Sources.NamedSource "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                ];
            ];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
       name = "foo_finders",
       find = "functions",
       where = AllOf(
         any_parameter.annotation.is_annotated_type(),
         return_annotation.is_annotated_type(),
       ),
       model = [Returns([TaintSource[Test]])]
    )
    |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 10; column = 1 } };
          name = "foo_finders";
          query =
            [
              AllOf
                [
                  AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint);
                  ReturnConstraint IsAnnotatedTypeConstraint;
                ];
            ];
          rule_kind = FunctionModel;
          productions =
            [
              ReturnTaint
                [
                  TaintAnnotation
                    (ModelParser.Source
                       {
                         source = Sources.NamedSource "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                ];
            ];
        };
      ]
    ();
  (* All parameters. *)
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "foo_finders",
     find = "functions",
     where = name.matches("foo"),
     model = [AllParameters(TaintSource[Test])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "foo_finders";
          query = [NameConstraint (Matches (Re2.create_exn "foo"))];
          rule_kind = FunctionModel;
          productions =
            [
              AllParametersTaint
                {
                  excludes = [];
                  taint =
                    [
                      TaintAnnotation
                        (ModelParser.Source
                           {
                             source = Sources.NamedSource "Test";
                             breadcrumbs = [];
                             via_features = [];
                             path = [];
                             leaf_names = [];
                             leaf_name_provided = false;
                             trace_length = None;
                           });
                    ];
                };
            ];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "foo_finders",
     find = "functions",
     where = name.matches("foo"),
     model = [AllParameters(TaintSource[Test], exclude="self")]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "foo_finders";
          query = [NameConstraint (Matches (Re2.create_exn "foo"))];
          rule_kind = FunctionModel;
          productions =
            [
              AllParametersTaint
                {
                  excludes = ["self"];
                  taint =
                    [
                      TaintAnnotation
                        (ModelParser.Source
                           {
                             source = Sources.NamedSource "Test";
                             breadcrumbs = [];
                             via_features = [];
                             path = [];
                             leaf_names = [];
                             leaf_name_provided = false;
                             trace_length = None;
                           });
                    ];
                };
            ];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "foo_finders",
     find = "functions",
     where = name.matches("foo"),
     model = [AllParameters(TaintSource[Test], exclude=["self", "other"])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "foo_finders";
          query = [NameConstraint (Matches (Re2.create_exn "foo"))];
          rule_kind = FunctionModel;
          productions =
            [
              AllParametersTaint
                {
                  excludes = ["self"; "other"];
                  taint =
                    [
                      TaintAnnotation
                        (ModelParser.Source
                           {
                             source = Sources.NamedSource "Test";
                             breadcrumbs = [];
                             via_features = [];
                             path = [];
                             leaf_names = [];
                             leaf_name_provided = false;
                             trace_length = None;
                           });
                    ];
                };
            ];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
       name = "foo_finders",
       find = "functions",
       where = any_parameter.annotation.is_annotated_type(),
       model = [
         AllParameters(
           ParametricSourceFromAnnotation(pattern=DynamicSource, kind=Dynamic)
         )
       ]
    )
    |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 11; column = 1 } };
          name = "foo_finders";
          query = [AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint)];
          rule_kind = FunctionModel;
          productions =
            [
              AllParametersTaint
                {
                  excludes = [];
                  taint =
                    [
                      ParametricSourceFromAnnotation
                        { source_pattern = "DynamicSource"; kind = "Dynamic" };
                    ];
                };
            ];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
       name = "foo_finders",
       find = "functions",
       where = any_parameter.annotation.is_annotated_type(),
       model = [
         AllParameters(
           ParametricSinkFromAnnotation(pattern=DynamicSink, kind=Dynamic)
         )
       ]
    )
    |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 11; column = 1 } };
          name = "foo_finders";
          query = [AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint)];
          rule_kind = FunctionModel;
          productions =
            [
              AllParametersTaint
                {
                  excludes = [];
                  taint =
                    [
                      ParametricSinkFromAnnotation { sink_pattern = "DynamicSink"; kind = "Dynamic" };
                    ];
                };
            ];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = parent.equals("Foo"),
     model = [Returns([TaintSource[Test], TaintSink[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          query = [ParentConstraint (NameSatisfies (Equals "Foo"))];
          rule_kind = MethodModel;
          productions =
            [
              ReturnTaint
                [
                  TaintAnnotation
                    (ModelParser.Source
                       {
                         source = Sources.NamedSource "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                  TaintAnnotation
                    (ModelParser.Sink
                       {
                         sink = Sinks.NamedSink "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                ];
            ];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = parent.extends("Foo"),
     model = [Returns([TaintSource[Test], TaintSink[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          query = [ParentConstraint (Extends { class_name = "Foo"; is_transitive = false })];
          rule_kind = MethodModel;
          productions =
            [
              ReturnTaint
                [
                  TaintAnnotation
                    (ModelParser.Source
                       {
                         source = Sources.NamedSource "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                  TaintAnnotation
                    (ModelParser.Sink
                       {
                         sink = Sinks.NamedSink "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                ];
            ];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = parent.extends("Foo", is_transitive=False),
     model = [Returns([TaintSource[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          query = [ParentConstraint (Extends { class_name = "Foo"; is_transitive = false })];
          rule_kind = MethodModel;
          productions =
            [
              ReturnTaint
                [
                  TaintAnnotation
                    (ModelParser.Source
                       {
                         source = Sources.NamedSource "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                ];
            ];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = parent.extends("Foo", is_transitive=True),
     model = [Returns([TaintSource[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          query = [ParentConstraint (Extends { class_name = "Foo"; is_transitive = true })];
          rule_kind = MethodModel;
          productions =
            [
              ReturnTaint
                [
                  TaintAnnotation
                    (ModelParser.Source
                       {
                         source = Sources.NamedSource "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                ];
            ];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = parent.matches("Foo.*"),
     model = [Returns([TaintSource[Test], TaintSink[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          query = [ParentConstraint (NameSatisfies (Matches (Re2.create_exn "Foo.*")))];
          rule_kind = MethodModel;
          productions =
            [
              ReturnTaint
                [
                  TaintAnnotation
                    (ModelParser.Source
                       {
                         source = Sources.NamedSource "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                  TaintAnnotation
                    (ModelParser.Sink
                       {
                         sink = Sinks.NamedSink "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                ];
            ];
        };
      ]
    ();
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = parent.decorator(name.matches("foo.*")),
     model = [Returns([TaintSource[Test], TaintSink[Test]])]
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          query =
            [
              ParentConstraint
                (DecoratorSatisfies
                   {
                     name_constraint = Matches (Re2.create_exn "foo.*");
                     arguments_constraint = None;
                   });
            ];
          rule_kind = MethodModel;
          productions =
            [
              ReturnTaint
                [
                  TaintAnnotation
                    (ModelParser.Source
                       {
                         source = Sources.NamedSource "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                  TaintAnnotation
                    (ModelParser.Sink
                       {
                         sink = Sinks.NamedSink "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                ];
            ];
        };
      ]
    ();

  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
     name = "get_foo",
     find = "methods",
     where = Decorator(name.matches("foo")),
     model = [Returns([TaintSource[Test]])],
    )
  |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 7; column = 1 } };
          name = "get_foo";
          query =
            [
              AnyDecoratorConstraint
                { name_constraint = Matches (Re2.create_exn "foo"); arguments_constraint = None };
            ];
          rule_kind = MethodModel;
          productions =
            [
              ReturnTaint
                [
                  TaintAnnotation
                    (ModelParser.Source
                       {
                         source = Sources.NamedSource "Test";
                         breadcrumbs = [];
                         via_features = [];
                         path = [];
                         leaf_names = [];
                         leaf_name_provided = false;
                         trace_length = None;
                       });
                ];
            ];
        };
      ]
    ();

  (* Parameters *)
  assert_queries
    ~context
    ~model_source:
      {|
    ModelQuery(
       name = "get_POST_annotated_sources",
       find = "functions",
       where = [Decorator(name.matches("api_view"))],
       model = [
         Parameters(
           TaintSink[Test],
           where=[
              Not(AnyOf(
                name.matches("self"),
                name.matches("cls"),
                type_annotation.matches("IGWSGIRequest"),
                type_annotation.matches("HttpRequest"),
              ))
           ]
         )
       ]
    )
    |}
    ~expect:
      [
        {
          location = { start = { line = 2; column = 0 }; stop = { line = 19; column = 1 } };
          name = "get_POST_annotated_sources";
          query =
            [
              AnyDecoratorConstraint
                {
                  name_constraint = Matches (Re2.create_exn "api_view");
                  arguments_constraint = None;
                };
            ];
          rule_kind = FunctionModel;
          productions =
            [
              ParameterTaint
                {
                  where =
                    [
                      Not
                        (AnyOf
                           [
                             ParameterConstraint.NameConstraint (Matches (Re2.create_exn "self"));
                             ParameterConstraint.NameConstraint (Matches (Re2.create_exn "cls"));
                             ParameterConstraint.AnnotationConstraint
                               (AnnotationNameConstraint (Matches (Re2.create_exn "IGWSGIRequest")));
                             ParameterConstraint.AnnotationConstraint
                               (AnnotationNameConstraint (Matches (Re2.create_exn "HttpRequest")));
                           ]);
                    ];
                  taint =
                    [
                      TaintAnnotation
                        (ModelParser.Sink
                           {
                             sink = Sinks.NamedSink "Test";
                             breadcrumbs = [];
                             via_features = [];
                             path = [];
                             leaf_names = [];
                             leaf_name_provided = false;
                             trace_length = None;
                           });
                    ];
                };
            ];
        };
      ]
    ();
  ()


let () =
  "taint_model"
  >::: [
         "attach_features" >:: test_attach_features;
         "class_models" >:: test_class_models;
         "cross_repository_models" >:: test_cross_repository_models;
         "demangle_class_attributes" >:: test_demangle_class_attributes;
         "filter_by_rules" >:: test_filter_by_rules;
         "invalid_models" >:: test_invalid_models;
         "partial_sinks" >:: test_partial_sinks;
         "query_parsing" >:: test_query_parsing;
         "global_sanitize" >:: test_global_sanitize;
         "sanitize_single_trace" >:: test_sanitize_single_trace;
         "attribute_sanitize" >:: test_attribute_sanitize;
         "parameter_sanitize" >:: test_parameter_sanitize;
         "return_sanitize" >:: test_return_sanitize;
         "parameters_sanitize" >:: test_parameters_sanitize;
         "skip_analysis" >:: test_skip_analysis;
         "skip_inlining_decorator" >:: test_skip_inlining_decorator;
         "sink_breadcrumbs" >:: test_sink_breadcrumbs;
         "sink_models" >:: test_sink_models;
         "source_breadcrumbs" >:: test_source_breadcrumbs;
         "source_models" >:: test_source_models;
         "taint_in_taint_out_models" >:: test_taint_in_taint_out_models;
         "taint_in_taint_out_models_alternate" >:: test_taint_in_taint_out_models_alternate;
         "taint_in_taint_out_transform" >:: test_taint_in_taint_out_transform;
         "taint_in_taint_out_update_models" >:: test_taint_in_taint_out_update_models;
         "taint_union_models" >:: test_union_models;
         "tito_breadcrumbs" >:: test_tito_breadcrumbs;
       ]
  |> Test.run
