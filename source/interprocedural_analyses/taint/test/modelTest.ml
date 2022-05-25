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
  let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
    ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
  in
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
    let global_resolution =
      Analysis.AnnotatedGlobalEnvironment.read_only global_environment
      |> Analysis.GlobalResolution.create
    in
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

  let environment =
    Analysis.TypeEnvironment.create global_environment |> Analysis.TypeEnvironment.read_only
  in
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
     find = "functions",
     where = name.matches("foo"),
     model = Returns(TaintSource[Test])
    )
  |}
    ~expect:
      [
        {
          name = None;
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
     find = "functions",
     where = name.matches("foo"),
     model = [Returns(TaintSource[Test])]
    )
  |}
    ~expect:
      [
        {
          name = None;
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
     find = "functions",
     where = [name.matches("foo"), name.matches("bar")],
     model = Returns(TaintSource[Test])
    )
  |}
    ~expect:
      [
        {
          name = None;
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
     find = "functions",
     where = name.matches("foo"),
     model = [Returns([TaintSource[Test], TaintSink[Test]])]
    )
  |}
    ~expect:
      [
        {
          name = None;
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
     find = "functions",
     where = name.equals("test.foo"),
     model = [Returns([TaintSource[Test], TaintSink[Test]])]
    )
  |}
    ~expect:
      [
        {
          name = None;
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
          name = Some "foo_finders";
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
          name = Some "foo_finders";
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
          name = Some "foo_finders";
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
          name = Some "foo_finders";
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
          name = Some "foo_finders";
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
          name = Some "foo_finders";
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
          name = Some "foo_finders";
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
          name = Some "foo_finders";
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
          name = Some "foo_finders";
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
          name = Some "foo_finders";
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
     find = "methods",
     where = parent.equals("Foo"),
     model = [Returns([TaintSource[Test], TaintSink[Test]])]
    )
  |}
    ~expect:
      [
        {
          name = None;
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
     find = "methods",
     where = parent.extends("Foo"),
     model = [Returns([TaintSource[Test], TaintSink[Test]])]
    )
  |}
    ~expect:
      [
        {
          name = None;
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
     find = "methods",
     where = parent.extends("Foo", is_transitive=False),
     model = [Returns([TaintSource[Test]])]
    )
  |}
    ~expect:
      [
        {
          name = None;
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
     find = "methods",
     where = parent.extends("Foo", is_transitive=True),
     model = [Returns([TaintSource[Test]])]
    )
  |}
    ~expect:
      [
        {
          name = None;
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
     find = "methods",
     where = parent.matches("Foo.*"),
     model = [Returns([TaintSource[Test], TaintSink[Test]])]
    )
  |}
    ~expect:
      [
        {
          name = None;
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
     find = "methods",
     where = Decorator(name.matches("foo")),
     model = [Returns([TaintSource[Test]])],
    )
  |}
    ~expect:
      [
        {
          name = None;
          query =
            [
              DecoratorConstraint
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
          name = Some "get_POST_annotated_sources";
          query =
            [
              DecoratorConstraint
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
         "sink_models" >:: test_sink_models;
         "attach_features" >:: test_attach_features;
         "class_models" >:: test_class_models;
         "cross_repository_models" >:: test_cross_repository_models;
         "demangle_class_attributes" >:: test_demangle_class_attributes;
         "filter_by_rules" >:: test_filter_by_rules;
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
