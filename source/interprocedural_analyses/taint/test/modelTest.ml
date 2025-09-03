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
open Taint
module PyrePysaApi = Interprocedural.PyrePysaApi
module AccessPath = Interprocedural.AccessPath

let get_stubs_and_definitions ~pyre_api ~configuration ~source_file_name =
  let qualifier = Ast.Reference.create (String.chop_suffix_exn source_file_name ~suffix:".py") in
  let initial_callables =
    Interprocedural.FetchCallables.from_qualifier ~configuration ~pyre_api ~qualifier
  in
  ( Interprocedural.FetchCallables.get_stubs initial_callables,
    Interprocedural.FetchCallables.get_definitions initial_callables )


let set_up_environment
    ?source
    ?rules
    ?filtered_sources
    ?filtered_sinks
    ?filtered_transforms
    ?(registered_partial_sinks = TaintConfiguration.RegisteredPartialSinks.empty)
    ?(skip_for_pyrefly = false)
    ~context
    ~model_source
    ()
  =
  let source =
    match source with
    | Some source -> source
    | None ->
        {|
          from django.http import Request

          def source() -> None: ...
          def sink(parameter0, parameter1, parameter, arg) -> None: ...
          def taint(x, y) -> None: ...
          def tito(parameter) -> str: ...
          def partial_sink(x, y) -> None: ...
          def update(self, arg1, arg2) -> None: ...
          def both(parameter0, parameter) -> None: ...
          def xss(parameter) -> None: ...
          def multiple(parameter) -> None: ...
        |}
  in
  let source_file_name = "test.py" in
  let project =
    Test.ScratchPyrePysaProject.setup
      ~context
      ~requires_type_of_expressions:false
      ~force_pyre1:skip_for_pyrefly
      [source_file_name, source]
  in
  let pyre_api = Test.ScratchPyrePysaProject.read_only_api project in
  let configuration = Test.ScratchPyrePysaProject.configuration_of project in
  let taint_configuration =
    let named name = { AnnotationParser.KindDefinition.name; kind = Named; location = None } in
    let sources =
      [
        named "TestTest";
        named "UserControlled";
        named "Test";
        named "Demo";
        { AnnotationParser.KindDefinition.name = "WithSubkind"; kind = Parametric; location = None };
      ]
    in
    let sinks =
      [
        named "TestSink";
        named "OtherSink";
        named "Test";
        named "Demo";
        named "XSS";
        {
          AnnotationParser.KindDefinition.name = "TestSinkWithSubkind";
          kind = Parametric;
          location = None;
        };
      ]
    in
    let transforms = [TaintTransform.Named "TestTransform"; TaintTransform.Named "DemoTransform"] in
    let rules =
      match rules with
      | Some rules -> rules
      | None ->
          [
            {
              Rule.sources =
                List.map sources ~f:(fun { AnnotationParser.KindDefinition.name; _ } ->
                    Sources.NamedSource name);
              sinks =
                List.map sinks ~f:(fun { AnnotationParser.KindDefinition.name; _ } ->
                    Sinks.NamedSink name);
              transforms = [];
              code = 1;
              name = "rule 1";
              message_format = "";
              filters = None;
              location = None;
            };
          ]
    in
    TaintConfiguration.Heap.
      {
        empty with
        sources;
        sinks;
        transforms;
        features = ["special"];
        registered_partial_sinks;
        rules;
        filtered_rule_codes = None;
        filtered_sources;
        filtered_sinks;
        source_sink_filter =
          SourceSinkFilter.create
            ~rules
            ~filtered_rule_codes:None
            ~filtered_sources
            ~filtered_sinks
            ~filtered_transforms;
      }
  in
  let source = Test.trim_extra_indentation model_source in

  PyrePysaApi.ModelQueries.invalidate_cache pyre_api;
  let stubs, definitions = get_stubs_and_definitions ~pyre_api ~configuration ~source_file_name in
  let ({ ModelParseResult.errors; _ } as parse_result) =
    ModelParser.parse
      ~pyre_api
      ~source
      ~taint_configuration
      ~source_sink_filter:(Some taint_configuration.source_sink_filter)
      ~definitions:(Some (Interprocedural.Target.HashSet.of_list definitions))
      ~stubs:
        (stubs
        |> Interprocedural.Target.HashsetSharedMemory.from_heap
        |> Interprocedural.Target.HashsetSharedMemory.read_only)
      ~python_version:(ModelParser.PythonVersion.create ())
      ()
  in
  assert_bool
    (Format.sprintf
       "Models have parsing errors: %s"
       (List.to_string errors ~f:ModelVerificationError.display))
    (List.is_empty errors);

  parse_result, pyre_api, taint_configuration


let assert_model
    ?source
    ?rules
    ?filtered_sources
    ?filtered_sinks
    ?expected_skipped_overrides
    ?registered_partial_sinks
    ?skip_for_pyrefly
    ~context
    ~model_source
    ~expect
    ()
  =
  let { ModelParseResult.models; _ }, pyre_api, taint_configuration =
    set_up_environment
      ?source
      ?rules
      ?filtered_sources
      ?filtered_sinks
      ?registered_partial_sinks
      ?skip_for_pyrefly
      ~context
      ~model_source
      ()
  in
  begin
    match expected_skipped_overrides with
    | Some expected ->
        let expected_set =
          List.map expected ~f:Ast.Reference.create |> Ast.Reference.SerializableSet.of_list
        in
        assert_equal
          ~cmp:Ast.Reference.SerializableSet.equal
          ~printer:Ast.Reference.SerializableSet.show
          expected_set
          (Registry.skip_overrides models)
    | None -> ()
  end;
  let get_model = Registry.get models in
  let get_errors _ = [] in
  List.iter ~f:(check_expectation ~pyre_api ~taint_configuration ~get_model ~get_errors) expect


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
  let pyre_api =
    Test.ScratchPyrePysaProject.setup ~context ~requires_type_of_expressions:false sources
    |> Test.ScratchPyrePysaProject.read_only_api
  in
  let taint_configuration =
    TaintConfiguration.Heap.
      {
        empty with
        sources =
          List.map
            ~f:(fun name -> { AnnotationParser.KindDefinition.name; kind = Named; location = None })
            ["A"; "B"; "Test"];
        sinks =
          List.map
            ~f:(fun name -> { AnnotationParser.KindDefinition.name; kind = Named; location = None })
            ["X"; "Y"; "Test"];
        features = ["featureA"; "featureB"];
        rules = [];
        registered_partial_sinks =
          TaintConfiguration.RegisteredPartialSinks.of_alist_exn
            ["TestA", ["TestB"]; "TestB", ["TestA"]];
      }
  in
  let error_message =
    let path = path >>| PyrePath.create_absolute in
    PyrePysaApi.ModelQueries.invalidate_cache pyre_api;
    ModelParser.parse
      ~pyre_api
      ~taint_configuration
      ~source_sink_filter:None
      ?path
      ~source:(Test.trim_extra_indentation model_source)
      ~definitions:None
      ~stubs:
        ([]
        |> Interprocedural.Target.HashsetSharedMemory.from_heap
        |> Interprocedural.Target.HashsetSharedMemory.read_only)
      ~python_version:(ModelParser.PythonVersion.create ())
      ()
    |> fun { ModelParseResult.errors; _ } ->
    if List.is_empty errors then
      "no failure"
    else if List.length errors = 1 then
      List.hd_exn errors |> ModelVerificationError.display
    else
      let error_strings = List.map errors ~f:ModelVerificationError.display in
      List.fold error_strings ~init:"Multiple errors:\n[" ~f:(fun accum string ->
          accum ^ "\n" ^ string)
      ^ "\n]"
  in
  assert_equal ~printer:Fn.id expect error_message


let test_models_with_if context =
  let assert_model = assert_model ~context in
  let assert_invalid_model = assert_invalid_model ~context in
  (* valid conditional operators *)
  assert_model
    ~source:{|
      def foo(x, y): ...
    |}
    ~model_source:
      {|
      if sys.version < (1,0,0):
        def test.foo(x: TaintSource[Test]): ...
      else:
        def test.foo(y: TaintSource[Test]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sources:[{ name = "y"; sources = [Sources.NamedSource "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.foo";
      ]
    ();
  assert_model
    ~source:{|
      def foo(x, y): ...
    |}
    ~model_source:
      {|
      if sys.version < (100,0,0):
        def test.foo(x: TaintSource[Test]): ...
      else:
        def test.foo(y: TaintSource[Test]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sources:[{ name = "x"; sources = [Sources.NamedSource "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.foo";
      ]
    ();
  assert_model
    ~source:{|
      def foo(x, y): ...
    |}
    ~model_source:
      {|
      if sys.version > (1,0,0):
        def test.foo(x: TaintSource[Test]): ...
      else:
        def test.foo(y: TaintSource[Test]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sources:[{ name = "x"; sources = [Sources.NamedSource "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.foo";
      ]
    ();
  assert_model
    ~source:{|
      def foo(x, y): ...
    |}
    ~model_source:
      {|
      if sys.version > (100,0,0):
        def test.foo(x: TaintSource[Test]): ...
      else:
        def test.foo(y: TaintSource[Test]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sources:[{ name = "y"; sources = [Sources.NamedSource "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.foo";
      ]
    ();
  assert_model
    ~source:{|
      def foo(x, y): ...
    |}
    ~model_source:
      {|
      if sys.version <= (1,0,0):
        def test.foo(x: TaintSource[Test]): ...
      else:
        def test.foo(y: TaintSource[Test]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sources:[{ name = "y"; sources = [Sources.NamedSource "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.foo";
      ]
    ();
  assert_model
    ~source:{|
      def foo(x, y): ...
    |}
    ~model_source:
      {|
      if sys.version <= (100,0,0):
        def test.foo(x: TaintSource[Test]): ...
      else:
        def test.foo(y: TaintSource[Test]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sources:[{ name = "x"; sources = [Sources.NamedSource "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.foo";
      ]
    ();
  assert_model
    ~source:{|
      def foo(x, y): ...
    |}
    ~model_source:
      {|
      if sys.version >= (1,0,0):
        def test.foo(x: TaintSource[Test]): ...
      else:
        def test.foo(y: TaintSource[Test]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sources:[{ name = "x"; sources = [Sources.NamedSource "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.foo";
      ]
    ();
  assert_model
    ~source:{|
      def foo(x, y): ...
    |}
    ~model_source:
      {|
      if sys.version >= (100,0,0):
        def test.foo(x: TaintSource[Test]): ...
      else:
        def test.foo(y: TaintSource[Test]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sources:[{ name = "y"; sources = [Sources.NamedSource "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.foo";
      ]
    ();
  assert_model
    ~source:{|
      def foo(x, y): ...
    |}
    ~model_source:
      {|
      if sys.version == (1,0,0): # Always False
        def test.foo(x: TaintSource[Test]): ...
      else:
        def test.foo(y: TaintSource[Test]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sources:[{ name = "y"; sources = [Sources.NamedSource "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.foo";
      ]
    ();
  assert_model
    ~source:{|
      def foo(x, y): ...
    |}
    ~model_source:
      {|
      if sys.version != (1,0,0): # Always True
        def test.foo(x: TaintSource[Test]): ...
      else:
        def test.foo(y: TaintSource[Test]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sources:[{ name = "x"; sources = [Sources.NamedSource "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.foo";
      ]
    ();
  (* Nested if conditions *)
  assert_model
    ~source:{|
      def foo(x, y): ...
    |}
    ~model_source:
      {|
      if sys.version != (1,0,0):
        if sys.version >= (1,0,0):
          def test.foo(x: TaintSource[Test]): ...
        else:
          def test.foo(y: TaintSource[Test]): ...
      else:
        def test.foo(y: TaintSource[Test]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sources:[{ name = "x"; sources = [Sources.NamedSource "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.foo";
      ]
    ();
  assert_model
    ~source:{|
      def foo(x, y): ...
    |}
    ~model_source:
      {|
      if sys.version == (1,0,0):
        def test.foo(x: TaintSource[Test]): ...
      else:
        if sys.version <= (1,0,0):
          def test.foo(x: TaintSource[Test]): ...
        else:
          def test.foo(y: TaintSource[Test]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sources:[{ name = "y"; sources = [Sources.NamedSource "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.foo";
      ]
    ();
  (* Invalid operators *)
  assert_invalid_model
    ~model_source:{|
      if sys.version in (1,2,3):
        pass
    |}
    ~expect:"The operator `in` in the if condition is not supported"
    ();
  assert_invalid_model
    ~model_source:{|
      if sys.version not in (1,2,3):
        pass
    |}
    ~expect:"The operator `not in` in the if condition is not supported"
    ();
  assert_invalid_model
    ~model_source:{|
      if sys.version is (1,2,3):
        pass
    |}
    ~expect:"The operator `is` in the if condition is not supported"
    ();
  (* Invalid if conditions *)
  assert_invalid_model
    ~model_source:{|
      if True:
        pass
    |}
    ~expect:
      "Unsupported if condition: `True`. If conditions need to be of the form: `sys.version \
       operator version_tuple`. All models inside the if-block (along with those in else-if and \
       else block, if present) will be ignored."
    ();
  assert_invalid_model
    ~model_source:{|
      if foo.version == (0,0,0):
        pass
    |}
    ~expect:
      "Unsupported if condition: `foo.version == (0, 0, 0)`. If conditions need to be of the form: \
       `sys.version operator version_tuple`. All models inside the if-block (along with those in \
       else-if and else block, if present) will be ignored."
    ();
  assert_invalid_model
    ~model_source:{|
      if sys.version == "foo":
        pass
    |}
    ~expect:
      "Unsupported if condition: `sys.version == \"foo\"`. If conditions need to be of the form: \
       `sys.version operator version_tuple`. All models inside the if-block (along with those in \
       else-if and else block, if present) will be ignored."
    ();
  ()


let test_source_models context =
  let assert_model = assert_model ~context in
  let assert_invalid_model = assert_invalid_model ~context in
  assert_model
    ~model_source:"def test.taint() -> TaintSource[TestTest]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.NamedSource "TestTest"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
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
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:"os.environ: TaintSink[Test] = ..."
    ~expect:
      [
        outcome
          ~kind:`Object
          ~parameter_sinks:[{ name = "$global"; sinks = [Sinks.NamedSink "Test"] }]
          "os.environ";
      ]
    ();
  assert_model
    ~source:"def f(x: int): ..."
    ~model_source:"def test.f(x) -> TaintSource[Test, ViaValueOf[x]]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.NamedSource "Test"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.f";
      ]
    ();
  assert_model
    ~source:"def f(x: int): ..."
    ~model_source:"def test.f() -> TaintSource[Test, ViaValueOf[x]]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.NamedSource "Test"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.f";
      ]
    ();
  assert_model
    ~source:"def f(x: int): ..."
    ~model_source:{|def test.f(x) -> TaintSource[Test, ViaValueOf[x, WithTag["tag"]]]: ...|}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.NamedSource "Test"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.f";
      ]
    ();
  assert_invalid_model
    ~source:"def f(x: int): ..."
    ~model_source:{|def test.f(x) -> TaintSource[Test, ViaValueOf[WithTag["tag"]]]: ...|}
    ~expect:
      "`TaintSource[(Test, ViaValueOf[WithTag[\"tag\"]])]` is an invalid taint annotation: Missing \
       parameter name for `ViaValueOf`"
    ();
  assert_model
    ~source:"def f(x: int): ..."
    ~model_source:"def test.f(x) -> TaintSource[Test, ViaTypeOf[x]]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.NamedSource "Test"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.f";
      ]
    ();
  assert_model
    ~source:"def f(x: int): ..."
    ~model_source:"def test.f() -> TaintSource[Test, ViaTypeOf[x]]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.NamedSource "Test"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.f";
      ]
    ();
  assert_model
    ~source:"def f(x: int): ..."
    ~model_source:{|def test.f(x) -> TaintSource[Test, ViaTypeOf[x, WithTag["tag"]]]: ...|}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.NamedSource "Test"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.f";
      ]
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
    ~expect:
      [
        outcome
          ~kind:`PropertySetter
          ~parameter_generations:[{ name = "self"; sources = [Sources.NamedSource "Test"] }]
          "test.C.foo";
      ]
    ();
  assert_model
    ~source:"def f(x: int): ..."
    ~model_source:"def test.f(x) -> AppliesTo[0, TaintSource[Test]]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.NamedSource "Test"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.f";
      ]
    ();
  assert_model
    ~source:"def f(x: int): ..."
    ~model_source:"def test.f(x) -> TaintSource[Test, ReturnPath[_[0]]]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.NamedSource "Test"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.f";
      ]
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
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
          ~parameter_sources:[{ name = "y"; sources = [Sources.NamedSource "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.foo";
      ]
    ();
  assert_model
    ~source:{|
      def foo(x, y): ...
    |}
    ~model_source:"def test.foo(y: TaintSource[Test, ParameterPath[_[0]]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sources:[{ name = "y"; sources = [Sources.NamedSource "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.foo";
      ]
    ();
  assert_model
    ~source:"def f(x: int): ..."
    ~model_source:"def test.f(x) -> TaintSource[Test, ReturnPath[_.all_static_fields()]]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.NamedSource "Test"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.f";
      ]
    ();
  ()


let test_global_sanitize context =
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
          ~global_sanitizer:Sanitize.all
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
          ~global_sanitizer:(Sanitize.from_tito_only SanitizeTransformSet.all)
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
            (Sanitize.from_tito_only
               (SanitizeTransformSet.from_sources
                  (SanitizeTransform.SourceSet.singleton (SanitizeTransform.Source.Named "Test"))))
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
            (Sanitize.from_tito_only
               (SanitizeTransformSet.from_sinks
                  (SanitizeTransform.SinkSet.singleton (SanitizeTransform.Sink.Named "Test"))))
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
            (Sanitize.from_tito_only
               {
                 SanitizeTransformSet.sources =
                   SanitizeTransform.SourceSet.singleton (SanitizeTransform.Source.Named "Test");
                 sinks = SanitizeTransform.SinkSet.singleton (SanitizeTransform.Sink.Named "Test");
               })
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:{|
      @Sanitize
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~global_sanitizer:Sanitize.all
          ~analysis_modes:(Model.ModeSet.of_list [Obscure])
          "test.taint";
      ]
    ();
  assert_model
    ~model_source:{|
      @IgnoreDecorator
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~analysis_modes:(Model.ModeSet.of_list [IgnoreDecorator; Obscure])
          "test.taint";
      ]
    ();
  ()


let test_sanitize_single_trace context =
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
          ~global_sanitizer:(Sanitize.from_sources_only SanitizeTransform.SourceSet.all)
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
          ~global_sanitizer:(Sanitize.from_sinks_only SanitizeTransform.SinkSet.all)
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
            (Sanitize.from_sources_only
               (SanitizeTransform.SourceSet.singleton (SanitizeTransform.Source.Named "Test")))
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
            (Sanitize.from_sources_only
               (SanitizeTransform.SourceSet.of_list
                  [
                    SanitizeTransform.Source.Named "UserControlled";
                    SanitizeTransform.Source.Named "Test";
                  ]))
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
            (Sanitize.from_sinks_only
               (SanitizeTransform.SinkSet.singleton (SanitizeTransform.Sink.Named "Test")))
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
            (Sanitize.from_sinks_only
               (SanitizeTransform.SinkSet.of_list
                  [
                    SanitizeTransform.Sink.Named "TestSink"; SanitizeTransform.Sink.Named "OtherSink";
                  ]))
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
  let assert_model = assert_model ~context in
  assert_model
    ~model_source:"django.http.Request.GET: Sanitize = ..."
    ~expect:[outcome ~kind:`Object ~global_sanitizer:Sanitize.all "django.http.Request.GET"]
    ();
  assert_model
    ~model_source:"django.http.Request.GET: Sanitize[TaintSource] = ..."
    ~expect:
      [
        outcome
          ~kind:`Object
          ~global_sanitizer:(Sanitize.from_sources_only SanitizeTransform.SourceSet.all)
          "django.http.Request.GET";
      ]
    ();
  assert_model
    ~model_source:"django.http.Request.GET: Sanitize[TaintSink] = ..."
    ~expect:
      [
        outcome
          ~kind:`Object
          ~global_sanitizer:(Sanitize.from_sinks_only SanitizeTransform.SinkSet.all)
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
            (Sanitize.from_sources_only
               (SanitizeTransform.SourceSet.singleton (SanitizeTransform.Source.Named "Test")))
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
            (Sanitize.from_sinks_only
               (SanitizeTransform.SinkSet.singleton (SanitizeTransform.Sink.Named "Test")))
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
            (Sanitize.from_sources_only
               (SanitizeTransform.SourceSet.of_list
                  [SanitizeTransform.Source.Named "TestTest"; SanitizeTransform.Source.Named "Test"]))
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
            (Sanitize.from_sinks_only
               (SanitizeTransform.SinkSet.of_list
                  [SanitizeTransform.Sink.Named "TestSink"; SanitizeTransform.Sink.Named "Test"]))
          "django.http.Request.GET";
      ]
    ()


let test_parameter_sanitize context =
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
          ~parameter_sanitizers:[{ name = "x"; sanitize = Sanitize.all }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
            [{ name = "x"; sanitize = Sanitize.from_tito_only SanitizeTransformSet.all }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
                  Sanitize.from_sources_only
                    (SanitizeTransform.SourceSet.singleton (SanitizeTransform.Source.Named "Test"));
              };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
                  Sanitize.from_sources_only
                    (SanitizeTransform.SourceSet.of_list
                       [
                         SanitizeTransform.Source.Named "UserControlled";
                         SanitizeTransform.Source.Named "Test";
                       ]);
              };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
                  Sanitize.from_tito_only
                    (SanitizeTransformSet.from_sources
                       (SanitizeTransform.SourceSet.singleton
                          (SanitizeTransform.Source.Named "Test")));
              };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
                  Sanitize.from_tito_only
                    (SanitizeTransformSet.from_sinks
                       (SanitizeTransform.SinkSet.singleton (SanitizeTransform.Sink.Named "Test")));
              };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
                      SanitizeTransform.SourceSet.singleton (SanitizeTransform.Source.Named "Test");
                    sinks = SanitizeTransform.SinkSet.empty;
                    tito =
                      SanitizeTransformSet.from_sinks
                        (SanitizeTransform.SinkSet.singleton (SanitizeTransform.Sink.Named "Test"));
                  };
              };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
                  Sanitize.from_tito_only
                    {
                      SanitizeTransformSet.sources =
                        SanitizeTransform.SourceSet.singleton
                          (SanitizeTransform.Source.Named "Test");
                      sinks =
                        SanitizeTransform.SinkSet.singleton (SanitizeTransform.Sink.Named "Test");
                    };
              };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ()


let test_return_sanitize context =
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
          ~return_sanitizer:Sanitize.all
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
          ~return_sanitizer:(Sanitize.from_tito_only SanitizeTransformSet.all)
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
            (Sanitize.from_sources_only
               (SanitizeTransform.SourceSet.singleton (SanitizeTransform.Source.Named "Test")))
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
            (Sanitize.from_sources_only
               (SanitizeTransform.SourceSet.of_list
                  [
                    SanitizeTransform.Source.Named "UserControlled";
                    SanitizeTransform.Source.Named "Test";
                  ]))
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
            (Sanitize.from_tito_only
               {
                 SanitizeTransformSet.sources =
                   SanitizeTransform.SourceSet.singleton (SanitizeTransform.Source.Named "Test");
                 sinks = SanitizeTransform.SinkSet.empty;
               })
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
            (Sanitize.from_tito_only
               {
                 SanitizeTransformSet.sources = SanitizeTransform.SourceSet.empty;
                 sinks = SanitizeTransform.SinkSet.singleton (SanitizeTransform.Sink.Named "Test");
               })
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
                SanitizeTransform.SourceSet.singleton (SanitizeTransform.Source.Named "Test");
              sinks = SanitizeTransform.SinkSet.empty;
              tito =
                {
                  SanitizeTransformSet.sources = SanitizeTransform.SourceSet.empty;
                  sinks = SanitizeTransform.SinkSet.singleton (SanitizeTransform.Sink.Named "Test");
                };
            }
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
            (Sanitize.from_tito_only
               {
                 SanitizeTransformSet.sources =
                   SanitizeTransform.SourceSet.singleton (SanitizeTransform.Source.Named "Test");
                 sinks = SanitizeTransform.SinkSet.singleton (SanitizeTransform.Sink.Named "Test");
               })
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ()


let test_parameters_sanitize context =
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
          ~parameters_sanitizer:Sanitize.all
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
          ~parameters_sanitizer:(Sanitize.from_tito_only SanitizeTransformSet.all)
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
            (Sanitize.from_sources_only
               (SanitizeTransform.SourceSet.singleton (SanitizeTransform.Source.Named "Test")))
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
            (Sanitize.from_sources_only
               (SanitizeTransform.SourceSet.of_list
                  [
                    SanitizeTransform.Source.Named "UserControlled";
                    SanitizeTransform.Source.Named "Test";
                  ]))
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
            (Sanitize.from_tito_only
               {
                 SanitizeTransformSet.sources =
                   SanitizeTransform.SourceSet.singleton (SanitizeTransform.Source.Named "Test");
                 sinks = SanitizeTransform.SinkSet.empty;
               })
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
            (Sanitize.from_tito_only
               {
                 SanitizeTransformSet.sources = SanitizeTransform.SourceSet.empty;
                 sinks = SanitizeTransform.SinkSet.singleton (SanitizeTransform.Sink.Named "Test");
               })
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
                SanitizeTransform.SourceSet.singleton (SanitizeTransform.Source.Named "Test");
              sinks = SanitizeTransform.SinkSet.empty;
              tito =
                {
                  SanitizeTransformSet.sources = SanitizeTransform.SourceSet.empty;
                  sinks = SanitizeTransform.SinkSet.singleton (SanitizeTransform.Sink.Named "Test");
                };
            }
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
            (Sanitize.from_tito_only
               {
                 SanitizeTransformSet.sources =
                   SanitizeTransform.SourceSet.singleton (SanitizeTransform.Source.Named "Test");
                 sinks = SanitizeTransform.SinkSet.singleton (SanitizeTransform.Sink.Named "Test");
               })
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
          ~parameter_sinks:[{ name = "parameter"; sinks = [Sinks.NamedSink "TestSink"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.sink";
      ]
    ();
  assert_model
    ~model_source:"def test.sink(parameter0, parameter1: TaintSink[Test]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:[{ name = "parameter1"; sinks = [Sinks.NamedSink "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.sink";
      ]
    ();
  assert_model
    ~model_source:"def test.sink(parameter0: TaintSink[Test], parameter1: TaintSink[Test]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:
            [
              { name = "parameter0"; sinks = [Sinks.NamedSink "Test"] };
              { name = "parameter1"; sinks = [Sinks.NamedSink "Test"] };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.sink";
      ]
    ();
  assert_model
    ~model_source:"def test.sink(parameter0: TaintSink[Test], parameter1: TaintSink[Test]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:
            [
              { name = "parameter0"; sinks = [Sinks.NamedSink "Test"] };
              { name = "parameter1"; sinks = [Sinks.NamedSink "Test"] };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
          ~parameter_sinks:[{ name = "parameter0"; sinks = [Sinks.NamedSink "Demo"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.both";
      ]
    ();
  assert_model
    ~model_source:
      {|
      def test.sink(
        parameter0: TaintSink[Test],
        parameter1: TaintSink[Test, ViaValueOf[parameter0]]
      ): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:
            [
              { name = "parameter0"; sinks = [Sinks.NamedSink "Test"] };
              { name = "parameter1"; sinks = [Sinks.NamedSink "Test"] };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.sink";
      ]
    ();
  assert_model
    ~model_source:
      {|
      def test.sink(
        parameter0,
        parameter1: TaintSink[Test, ViaValueOf[parameter0, WithTag["tag"]]]
      ): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:[{ name = "parameter1"; sinks = [Sinks.NamedSink "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.sink";
      ]
    ();
  assert_model
    ~model_source:
      {|
      def test.sink(
        parameter0,
        parameter1: TaintSink[Test, ViaTypeOf[parameter0]]
      ): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:[{ name = "parameter1"; sinks = [Sinks.NamedSink "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.sink";
      ]
    ();
  assert_model
    ~model_source:
      {|
      def test.sink(
        parameter0,
        parameter1: TaintSink[Test, ViaTypeOf[parameter0, WithTag["tag"]]]
      ): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:[{ name = "parameter1"; sinks = [Sinks.NamedSink "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.sink";
      ]
    ();
  assert_model
    ~model_source:"def test.xss(parameter: TaintSink[XSS]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:[{ name = "parameter"; sinks = [Sinks.NamedSink "XSS"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.xss";
      ]
    ();
  assert_model
    ~model_source:"def test.multiple(parameter: TaintSink[XSS, Demo]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:
            [{ name = "parameter"; sinks = [Sinks.NamedSink "Demo"; Sinks.NamedSink "XSS"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.multiple";
      ]
    ();
  assert_model
    ~model_source:"def test.multiple(parameter: AppliesTo[1, TaintSink[XSS, Demo]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:
            [{ name = "parameter"; sinks = [Sinks.NamedSink "Demo"; Sinks.NamedSink "XSS"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.multiple";
      ]
    ();
  assert_model
    ~model_source:"def test.multiple(parameter: TaintSink[XSS, Demo, ParameterPath[_[1]]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:
            [{ name = "parameter"; sinks = [Sinks.NamedSink "Demo"; Sinks.NamedSink "XSS"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.multiple";
      ]
    ();
  assert_model
    ~model_source:
      "def test.sink(parameter0: TaintSink[XSS, ParameterPath[_.all_static_fields()]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:[{ name = "parameter0"; sinks = [Sinks.NamedSink "XSS"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.sink";
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
          ~parameter_sinks:
            [
              {
                name = "parameter";
                sinks =
                  [Sinks.ParametricSink { sink_name = "TestSinkWithSubkind"; subkind = "Subkind" }];
              };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.sink";
      ]
    ();
  assert_model
    ~model_source:"def test.multiple() -> TaintSink[XSS]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~return_sinks:[Sinks.NamedSink "XSS"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.multiple";
      ]
    ();
  assert_model
    ~model_source:"def test.multiple() -> TaintSink[XSS, ReturnPath[_[0]]]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~return_sinks:[Sinks.NamedSink "XSS"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.multiple";
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
          ~parameter_sources:
            [{ name = "source_parameter"; sources = [Sources.NamedSource "UserControlled"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
          ~parameter_sources:
            [{ name = "source_parameter"; sources = [Sources.NamedSource "UserControlled"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
          ~parameter_sources:
            [{ name = "source_parameter"; sources = [Sources.NamedSource "UserControlled"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
          ~parameter_sinks:[{ name = "x"; sinks = [Sinks.NamedSink "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
          ~parameter_sinks:[{ name = "parameter"; sinks = [Sinks.NamedSink "TestSink"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.Sink.method";
        outcome
          ~kind:`Method
          ~parameter_sinks:
            [
              { name = "first"; sinks = [Sinks.NamedSink "TestSink"] };
              { name = "second"; sinks = [Sinks.NamedSink "TestSink"] };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
          ~parameter_sinks:
            [
              {
                name = "parameter";
                sinks = [Sinks.NamedSink "OtherSink"; Sinks.NamedSink "TestSink"];
              };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.Sink.method";
        outcome
          ~kind:`Method
          ~parameter_sinks:
            [
              { name = "first"; sinks = [Sinks.NamedSink "OtherSink"; Sinks.NamedSink "TestSink"] };
              { name = "second"; sinks = [Sinks.NamedSink "OtherSink"; Sinks.NamedSink "TestSink"] };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
          ~parameter_sinks:[{ name = "parameter"; sinks = [Sinks.NamedSink "TestSink"] }]
          ~returns:[Sources.NamedSource "TestTest"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
        outcome
          ~kind:`Method
          ~returns:[Sources.NamedSource "UserControlled"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.Source.method";
        outcome
          ~kind:`Method
          ~returns:[Sources.NamedSource "UserControlled"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
          ~parameter_sinks:[{ name = "parameter"; sinks = [Sinks.NamedSink "TestSink"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
          ~parameter_sinks:[{ name = "parameter"; sinks = [Sinks.NamedSink "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
      [
        outcome
          ~kind:`Method
          ~returns:[Sources.NamedSource "UserControlled"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.Source.method";
      ]
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
      [
        outcome
          ~kind:`Method
          ~returns:[Sources.NamedSource "UserControlled"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.Source.prop";
      ]
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
          ~analysis_modes:(Model.ModeSet.of_list [SkipAnalysis; Obscure])
          "test.SkipMe.method";
        outcome
          ~kind:`Method
          ~analysis_modes:(Model.ModeSet.of_list [SkipAnalysis; Obscure])
          "test.SkipMe.method_with_multiple_parameters";
      ]
    ();
  assert_model
    ~source:
      {|
        class SkipMe:
          def method(parameter): ...
          def method_with_multiple_parameters(first, second): ...
      |}
    ~model_source:"class test.SkipMe(SkipModelBroadening): ..."
    ~expect:
      [
        outcome
          ~kind:`Method
          ~analysis_modes:(Model.ModeSet.of_list [SkipModelBroadening; Obscure])
          "test.SkipMe.method";
        outcome
          ~kind:`Method
          ~analysis_modes:(Model.ModeSet.of_list [SkipModelBroadening; Obscure])
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


let test_closure_models context =
  (* TODO(T225700656): Handle models on captured variables for Pyrefly. *)
  assert_model
    ~context
    ~model_source:
      {|
      @CapturedVariables(TaintSource[Test])
      def test.outer.inner(): ...
    |}
    ~source:{|
      def outer():
        def inner():
          pass
    |}
    ~expect:[outcome ~kind:`Function "test.outer.inner"]
    ~skip_for_pyrefly:true
    ();
  assert_invalid_model
    ~context
    ~model_source:{|
      @CapturedVariables
      def test.outer.inner(): ...
    |}
    ~source:{|
      def outer():
        def inner():
          pass
    |}
    ~expect:
      "`CapturedVariables()` is an invalid taint annotation: `@CapturedVariables(...)` needs one \
       Taint Annotation as argument."
    ();
  assert_model
    ~context
    ~model_source:
      {|
      @CapturedVariables(TaintSource[Test], generation=True)
      def test.outer.inner(): ...
    |}
    ~source:{|
      def outer():
        def inner():
          pass
    |}
    ~expect:[outcome ~kind:`Function "test.outer.inner"]
    ~skip_for_pyrefly:true
    ();
  assert_invalid_model
    ~context
    ~model_source:
      {|
      @CapturedVariables(TaintSource[Test], generation=1)
      def test.outer.inner(): ...
    |}
    ~source:{|
      def outer():
        def inner():
          pass
    |}
    ~expect:
      "`CapturedVariables(TaintSource[Test], generation = 1)` is an invalid taint annotation: Use \
       `@CapturedVariables(..., generation=True)` to specify generation source."
    ();
  assert_invalid_model
    ~context
    ~model_source:
      {|
      @CapturedVariables(TaintSink[Test], generation=True)
      def test.outer.inner(): ...
    |}
    ~source:{|
      def outer():
        def inner():
          pass
    |}
    ~expect:
      "`CapturedVariables(TaintSink[Test], generation = True)` is an invalid taint annotation: \
       `@CapturedVariables(..., generation=True)` must be used only on `TaintSource`s."
    ();
  assert_invalid_model
    ~context
    ~model_source:
      {|
      @CapturedVariables(TaintSource[Test], TaintSource[Test])
      def test.outer.inner(): ...
    |}
    ~source:{|
      def outer():
        def inner():
          pass
    |}
    ~expect:
      "`CapturedVariables(TaintSource[Test], TaintSource[Test])` is an invalid taint annotation: \
       `@CapturedVariables(...)` takes only one Taint Annotation as argument."
    ();
  assert_model
    ~context
    ~model_source:
      {|
      @CapturedVariables(TaintInTaintOut[Transform[TestTransform]])
      def test.outer.inner(): ...
    |}
    ~source:{|
      def outer():
        def inner():
          pass
    |}
    ~expect:[outcome ~kind:`Function "test.outer.inner"]
    ~skip_for_pyrefly:true
    ();
  ()


let test_taint_in_taint_out_models context =
  assert_model
    ~context
    ~model_source:"def test.tito(parameter: TaintInTaintOut): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_titos:[{ name = "parameter"; titos = [Sinks.LocalReturn] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
          ~parameter_titos:[{ name = "parameter"; titos = [Sinks.LocalReturn] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.tito";
      ]
    ();
  assert_model
    ~context
    ~model_source:"def test.tito(parameter: TaintInTaintOut[ParameterPath[_[0]]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_titos:[{ name = "parameter"; titos = [Sinks.LocalReturn] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.tito";
      ]
    ();
  assert_model
    ~context
    ~model_source:"def test.tito(parameter: TaintInTaintOut[ReturnPath[_[1]]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_titos:[{ name = "parameter"; titos = [Sinks.LocalReturn] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.tito";
      ]
    ();
  assert_model
    ~context
    ~model_source:
      "def test.tito(parameter: TaintInTaintOut[ParameterPath[_[0]], ReturnPath[_[1]]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_titos:[{ name = "parameter"; titos = [Sinks.LocalReturn] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.tito";
      ]
    ();
  assert_model
    ~context
    ~model_source:
      "def test.tito(parameter: TaintInTaintOut[ParameterPath[_[0]], ReturnPath[_[1]], \
       NoCollapse]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_titos:[{ name = "parameter"; titos = [Sinks.LocalReturn] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.tito";
      ]
    ();
  assert_model
    ~context
    ~model_source:
      "def test.tito(parameter: TaintInTaintOut[ParameterPath[_[0]], ReturnPath[_[1]], Collapse]): \
       ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_titos:[{ name = "parameter"; titos = [Sinks.LocalReturn] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.tito";
      ]
    ();
  assert_model
    ~context
    ~model_source:
      "def test.tito(parameter: TaintInTaintOut[ParameterPath[_[0]], ReturnPath[_[1]], \
       CollapseDepth[2]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_titos:[{ name = "parameter"; titos = [Sinks.LocalReturn] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.tito";
      ]
    ();
  ()


let test_taint_in_taint_out_models_alternate context =
  assert_model
    ~context
    ~model_source:"def test.tito(parameter: TaintInTaintOut[LocalReturn]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_titos:[{ name = "parameter"; titos = [Sinks.LocalReturn] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
          ~parameter_titos:
            [
              {
                name = "parameter";
                titos =
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
          ~parameter_sinks:
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
                        base = Sinks.ExtraTraceSink;
                      };
                  ];
              };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.tito";
      ]
    ()


let test_modes context =
  assert_model
    ~context
    ~model_source:{|
      @SkipAnalysis
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~analysis_modes:(Model.ModeSet.of_list [Obscure; SkipAnalysis])
          "test.taint";
      ]
    ();
  assert_model
    ~context
    ~model_source:{|
      @SkipModelBroadening
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~analysis_modes:(Model.ModeSet.of_list [SkipModelBroadening; Obscure])
          "test.taint";
      ]
    ();
  assert_model
    ~context
    ~model_source:{|
      def test.taint(x): ...|}
    ~expect:[outcome ~kind:`Function ~analysis_modes:(Model.ModeSet.singleton Obscure) "test.taint"]
    ();
  assert_model
    ~context
    ~model_source:{|
      @AnalyzeAllOverrides
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~analysis_modes:(Model.ModeSet.of_list [AnalyzeAllOverrides; Obscure])
          "test.taint";
      ]
    ();
  (* Test conflicting modes. *)
  assert_model
    ~context
    ~model_source:{|
      def test.taint(x): ...

      @SkipObscure
      def test.taint(x): ...|}
    ~expect:
      [outcome ~kind:`Function ~analysis_modes:(Model.ModeSet.singleton SkipObscure) "test.taint"]
    ();
  assert_model
    ~context
    ~model_source:
      {|
      @SkipOverrides
      def test.taint(x): ...

      @AnalyzeAllOverrides
      def test.taint(x): ... |}
    ~expected_skipped_overrides:[]
    ~expect:
      [
        outcome
          ~kind:`Function
          ~analysis_modes:(Model.ModeSet.of_list [AnalyzeAllOverrides; Obscure])
          "test.taint";
      ]
    ();
  assert_model
    ~context
    ~model_source:{|
      @SkipOverrides
      @AnalyzeAllOverrides
      def test.taint(x): ... |}
    ~expected_skipped_overrides:[]
    ~expect:
      [
        outcome
          ~kind:`Function
          ~analysis_modes:(Model.ModeSet.of_list [AnalyzeAllOverrides; Obscure])
          "test.taint";
      ]
    ();
  assert_model
    ~context
    ~model_source:{|
      @CalledWhenParameter
      def test.taint(x): ... |}
    ~expected_skipped_overrides:[]
    ~expect:
      [
        outcome
          ~kind:`Function
          ~analysis_modes:(Model.ModeSet.of_list [CalledWhenParameter; Obscure])
          "test.taint";
      ]
    ();
  ()


let test_taint_in_taint_out_update_models context =
  let assert_model = assert_model ~context in
  let positional_parameter position name =
    AccessPath.Root.PositionalParameter { position; name; positional_only = false }
  in
  assert_model
    ~model_source:"def test.update(self, arg1: TaintInTaintOut[Updates[self]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_titos:
            [{ name = "arg1"; titos = [Sinks.ParameterUpdate (positional_parameter 0 "self")] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.update";
      ]
    ();
  assert_model
    ~model_source:"def test.update(self, arg1, arg2: TaintInTaintOut[Updates[self, arg1]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_titos:
            [
              {
                name = "arg2";
                titos =
                  [
                    Sinks.ParameterUpdate (positional_parameter 0 "self");
                    Sinks.ParameterUpdate (positional_parameter 1 "arg1");
                  ];
              };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.update";
      ]
    ();
  assert_model
    ~model_source:"def test.update(self: TaintInTaintOut[LocalReturn, Updates[arg1]], arg1): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_titos:
            [
              {
                name = "self";
                titos = [Sinks.LocalReturn; Sinks.ParameterUpdate (positional_parameter 1 "arg1")];
              };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.update";
      ]
    ();
  assert_model
    ~model_source:
      "def test.update(self, arg1: TaintInTaintOut[Updates[self], UpdatePath[_[0]], \
       ParameterPath[_[1]]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_titos:
            [{ name = "arg1"; titos = [Sinks.ParameterUpdate (positional_parameter 0 "self")] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.update";
      ]
    ();
  ()


let test_union_models context =
  assert_model
    ~context
    ~model_source:"def test.both(parameter: Union[TaintInTaintOut, TaintSink[XSS]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:[{ name = "parameter"; sinks = [Sinks.NamedSink "XSS"] }]
          ~parameter_titos:[{ name = "parameter"; titos = [Sinks.LocalReturn] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.both";
      ]
    ()


let test_source_breadcrumbs context =
  assert_model
    ~context
    ~model_source:"def test.source() -> TaintSource[Test, Via[special]]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.NamedSource "Test"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.source";
      ]
    ();
  assert_model
    ~context
    ~model_source:
      "def test.source() -> TaintSource[Test, ViaDynamicFeature[NotSpecifiedInConfig]]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.NamedSource "Test"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.source";
      ]
    ();
  assert_model
    ~context
    ~model_source:
      "def test.source() -> TaintSource[Test, ViaDynamicFeature[\"NotSpecifiedInConfig\"]]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.NamedSource "Test"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.source";
      ]
    ();
  assert_model
    ~context
    ~model_source:"def test.source() -> TaintSource[Test, ViaDynamicFeature[\"a-b\"]]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.NamedSource "Test"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.source";
      ]
    ()


let test_sink_breadcrumbs context =
  assert_model
    ~context
    ~model_source:"def test.sink(parameter: TaintSink[Test, Via[special]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:[{ name = "parameter"; sinks = [Sinks.NamedSink "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.sink";
      ]
    ();
  assert_model
    ~context
    ~model_source:"def test.sink(parameter: TaintSink[Test, Via[\"special\"]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:[{ name = "parameter"; sinks = [Sinks.NamedSink "Test"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
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
          ~parameter_titos:[{ name = "parameter"; titos = [Sinks.LocalReturn] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.tito";
      ]
    ();
  assert_model
    ~context
    ~model_source:"def test.tito(parameter: TaintInTaintOut[Via[\"special\"]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_titos:[{ name = "parameter"; titos = [Sinks.LocalReturn] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.tito";
      ]
    ()


let test_attach_features context =
  let assert_model = assert_model ~context in
  assert_model
    ~model_source:"def test.source() -> AttachToSource[Via[special]]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.Attach]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.source";
      ]
    ();
  assert_model
    ~model_source:"def test.sink(arg: AttachToSink[Via[special]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.Attach] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.sink";
      ]
    ();
  assert_model
    ~model_source:"def test.tito(parameter: AttachToTito[Via[special]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_titos:[{ name = "parameter"; titos = [Sinks.Attach] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.tito";
      ]
    ()


let test_partial_sinks context =
  assert_model
    ~registered_partial_sinks:
      (TaintConfiguration.RegisteredPartialSinks.of_alist_exn
         ["TestA", ["TestB"]; "TestB", ["TestA"]])
    ~rules:
      [
        {
          Rule.sources = [Sources.NamedSource "TestTest"];
          sinks = [Sinks.PartialSink "TestA"];
          transforms = [TaintTransform.TriggeredPartialSink { triggering_source = "TestTest" }];
          code = 4321;
          message_format = "";
          name = "test multiple sources rule";
          filters = None;
          location = None;
        };
        {
          Rule.sources = [Sources.NamedSource "TestTest"];
          sinks = [Sinks.PartialSink "TestB"];
          transforms = [TaintTransform.TriggeredPartialSink { triggering_source = "TestTest" }];
          code = 4321;
          message_format = "";
          name = "test multiple sources rule";
          filters = None;
          location = None;
        };
      ]
    ~model_source:"def test.partial_sink(x: PartialSink[TestA], y: PartialSink[TestB]): ..."
    ~expect:
      [
        outcome
          ~parameter_sinks:
            [
              { name = "x"; sinks = [Sinks.PartialSink "TestA"] };
              { name = "y"; sinks = [Sinks.PartialSink "TestB"] };
            ]
          ~kind:`Function
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.partial_sink";
      ]
    ~context
    ();
  assert_model
    ~registered_partial_sinks:
      (TaintConfiguration.RegisteredPartialSinks.of_alist_exn
         ["TestA", ["TestB"]; "TestB", ["TestA"]])
    ~rules:
      [
        {
          Rule.sources = [Sources.NamedSource "TestTest"];
          sinks = [Sinks.PartialSink "TestA"];
          transforms = [TaintTransform.TriggeredPartialSink { triggering_source = "TestTest" }];
          code = 4321;
          message_format = "";
          name = "test multiple sources rule";
          filters = None;
          location = None;
        };
        {
          Rule.sources = [Sources.NamedSource "TestTest"];
          sinks = [Sinks.PartialSink "TestB"];
          transforms = [TaintTransform.TriggeredPartialSink { triggering_source = "TestTest" }];
          code = 4321;
          message_format = "";
          name = "test multiple sources rule";
          filters = None;
          location = None;
        };
      ]
    ~model_source:"def test.partial_sink(x: PartialSink[TestA], y: PartialSink[TestB]): ..."
    ~expect:
      [
        outcome
          ~parameter_sinks:
            [
              { name = "x"; sinks = [Sinks.PartialSink "TestA"] };
              { name = "y"; sinks = [Sinks.PartialSink "TestB"] };
            ]
          ~kind:`Function
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.partial_sink";
      ]
    ~context
    ()


let test_demangle_class_attributes _ =
  let assert_demangle ~expected name =
    assert_equal
      (Ast.Reference.create expected)
      (ModelParser.demangle_class_attribute (Ast.Reference.create name))
  in
  assert_demangle ~expected:"a.B" "a.B";
  assert_demangle ~expected:"a.B" "a.__class__.B";

  (* We require `__class__` to directly precede the attribute of the `.`-separated names. *)
  assert_demangle ~expected:"a.B.__class__" "a.B.__class__";
  assert_demangle ~expected:"a.__class__.B.C" "a.__class__.B.C"


let test_filter_by_rules context =
  let assert_model =
    assert_model
      ~registered_partial_sinks:
        (TaintConfiguration.RegisteredPartialSinks.of_alist_exn
           ["TestA", ["TestB"]; "TestB", ["TestA"]])
      ~context
  in
  assert_model
    ~rules:
      [
        {
          Rule.sources = [Sources.NamedSource "TestTest"];
          sinks = [Sinks.NamedSink "TestSink"];
          transforms = [];
          code = 5021;
          message_format = "";
          name = "test rule";
          filters = None;
          location = None;
        };
      ]
    ~model_source:"def test.taint() -> TaintSource[TestTest]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.NamedSource "TestTest"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ();
  assert_model
    ~rules:
      [
        {
          Rule.sources = [Sources.NamedSource "Test"];
          sinks = [Sinks.NamedSink "TestSink"];
          transforms = [];
          code = 5021;
          message_format = "";
          name = "test rule";
          filters = None;
          location = None;
        };
      ]
    ~model_source:"def test.taint() -> TaintSource[TestTest]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ();
  assert_model
    ~rules:
      [
        {
          Rule.sources = [Sources.NamedSource "TestTest"];
          sinks = [Sinks.NamedSink "TestSink"];
          transforms = [];
          code = 5021;
          message_format = "";
          name = "test rule";
          filters = None;
          location = None;
        };
      ]
    ~model_source:"def test.taint(x: TaintSink[TestSink]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:[{ name = "x"; sinks = [Sinks.NamedSink "TestSink"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ();
  assert_model
    ~rules:
      [
        {
          Rule.sources = [Sources.NamedSource "TestTest"];
          sinks = [Sinks.NamedSink "Test"];
          transforms = [];
          code = 5021;
          message_format = "";
          name = "test rule";
          filters = None;
          location = None;
        };
      ]
    ~model_source:"def test.taint(x: TaintSink[TestSink]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:[]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ();
  assert_model
    ~rules:
      [
        {
          Rule.sources = [Sources.NamedSource "TestTest"];
          sinks = [Sinks.PartialSink "TestA"];
          transforms = [TaintTransform.TriggeredPartialSink { triggering_source = "TestTest" }];
          code = 4321;
          message_format = "";
          name = "test multiple sources rule";
          filters = None;
          location = None;
        };
        {
          Rule.sources = [Sources.NamedSource "TestTest"];
          sinks = [Sinks.PartialSink "TestB"];
          transforms = [TaintTransform.TriggeredPartialSink { triggering_source = "TestTest" }];
          code = 4321;
          message_format = "";
          name = "test multiple sources rule";
          filters = None;
          location = None;
        };
      ]
    ~model_source:"def test.partial_sink(x: PartialSink[TestA], y: PartialSink[TestB]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:
            [
              { name = "x"; sinks = [Sinks.PartialSink "TestA"] };
              { name = "y"; sinks = [Sinks.PartialSink "TestB"] };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.partial_sink";
      ]
    ();
  assert_model
    ~rules:
      [
        {
          Rule.sources = [Sources.NamedSource "WithSubkind"];
          sinks = [Sinks.NamedSink "Test"];
          transforms = [];
          code = 5022;
          message_format = "";
          name = "test rule";
          filters = None;
          location = None;
        };
      ]
    ~model_source:"def test.taint() -> TaintSource[WithSubkind[A]]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.ParametricSource { source_name = "WithSubkind"; subkind = "A" }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ();
  assert_model
    ~rules:
      [
        {
          Rule.sources = [Sources.NamedSource "TestTest"];
          sinks = [Sinks.NamedSink "TestSinkWithSubkind"];
          transforms = [];
          code = 5023;
          message_format = "";
          name = "test rule";
          filters = None;
          location = None;
        };
      ]
    ~model_source:"def test.taint(x: TaintSink[TestSinkWithSubkind[A]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:
            [
              {
                name = "x";
                sinks = [Sinks.ParametricSink { sink_name = "TestSinkWithSubkind"; subkind = "A" }];
              };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ();
  ()


let test_filter_by_sources context =
  let assert_model = assert_model ~context in
  assert_model
    ~filtered_sources:(Sources.Set.of_list [Sources.NamedSource "Test"])
    ~model_source:"def test.taint() -> TaintSource[Test]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.NamedSource "Test"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ();
  assert_model
    ~filtered_sources:(Sources.Set.of_list [Sources.NamedSource "Test"])
    ~model_source:"def test.taint() -> TaintSource[TestTest]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ();
  assert_model
    ~filtered_sources:(Sources.Set.of_list [Sources.NamedSource "WithSubkind"])
    ~model_source:"def test.taint() -> TaintSource[WithSubkind[A]]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.ParametricSource { source_name = "WithSubkind"; subkind = "A" }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ();
  assert_model
    ~filtered_sources:(Sources.Set.of_list [Sources.NamedSource "Test"])
    ~model_source:"def test.taint() -> TaintSource[WithSubkind[A]]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ();
  (* Does not affect AttachTo *)
  assert_model
    ~filtered_sources:(Sources.Set.of_list [Sources.NamedSource "Test"])
    ~model_source:"def test.source() -> AttachToSource[Via[special]]: ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~returns:[Sources.Attach]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.source";
      ]
    ();
  (* Filters sanitizers *)
  assert_model
    ~filtered_sources:(Sources.Set.of_list [Sources.NamedSource "Test"])
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
                  Sanitize.from_sources_only
                    (SanitizeTransform.SourceSet.singleton (SanitizeTransform.Source.Named "Test"));
              };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ();
  assert_model
    ~filtered_sources:(Sources.Set.of_list [Sources.NamedSource "Test"])
    ~model_source:{|
      def test.taint(x: Sanitize[TaintSource[TestTest]]): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sanitizers:[]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ();
  assert_model
    ~filtered_sources:(Sources.Set.of_list [Sources.NamedSource "Test"])
    ~model_source:{|
      @SanitizeSingleTrace(TaintSource)
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~global_sanitizer:(Sanitize.from_sources_only SanitizeTransform.SourceSet.all)
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ();
  assert_model
    ~filtered_sources:(Sources.Set.of_list [Sources.NamedSource "Test"])
    ~model_source:
      {|
      @SanitizeSingleTrace(TaintSource[TestTest])
      def test.taint(x): ...
    |}
    ~expect:[outcome ~kind:`Function ~analysis_modes:(Model.ModeSet.singleton Obscure) "test.taint"]
    ();
  assert_model
    ~filtered_sources:(Sources.Set.of_list [Sources.NamedSource "Test"])
    ~model_source:{|
      def test.taint(x) -> Sanitize[TaintSource[TestTest]]: ...
    |}
    ~expect:[outcome ~kind:`Function ~analysis_modes:(Model.ModeSet.singleton Obscure) "test.taint"]
    ();
  ()


let test_filter_by_sinks context =
  let assert_model = assert_model ~context in
  assert_model
    ~filtered_sinks:(Sinks.Set.of_list [Sinks.NamedSink "TestSink"])
    ~model_source:"def test.taint(x: TaintSink[TestSink]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:[{ name = "x"; sinks = [Sinks.NamedSink "TestSink"] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ();
  assert_model
    ~filtered_sinks:(Sinks.Set.of_list [Sinks.NamedSink "TestSink"])
    ~model_source:"def test.taint(x: TaintSink[OtherSink]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:[]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ();
  assert_model
    ~filtered_sinks:(Sinks.Set.of_list [Sinks.NamedSink "TestSinkWithSubkind"])
    ~model_source:"def test.taint(x: TaintSink[TestSinkWithSubkind[A]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:
            [
              {
                name = "x";
                sinks = [Sinks.ParametricSink { sink_name = "TestSinkWithSubkind"; subkind = "A" }];
              };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ();
  assert_model
    ~filtered_sinks:(Sinks.Set.of_list [Sinks.NamedSink "TestSink"])
    ~model_source:"def test.taint(x: TaintSink[TestSinkWithSubkind[A]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:[]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ();
  (* Does not affect AttachTo, Tito, Transform *)
  assert_model
    ~filtered_sinks:(Sinks.Set.of_list [Sinks.NamedSink "TestSink"])
    ~model_source:"def test.sink(arg: AttachToSink[Via[special]]): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_sinks:[{ name = "arg"; sinks = [Sinks.Attach] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.sink";
      ]
    ();
  assert_model
    ~filtered_sinks:(Sinks.Set.of_list [Sinks.NamedSink "TestSink"])
    ~model_source:"def test.tito(parameter: TaintInTaintOut): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_titos:[{ name = "parameter"; titos = [Sinks.LocalReturn] }]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.tito";
      ]
    ();
  assert_model
    ~filtered_sinks:(Sinks.Set.of_list [Sinks.NamedSink "TestSink"])
    ~model_source:"def test.update(self: TaintInTaintOut[LocalReturn, Updates[arg1]], arg1): ..."
    ~expect:
      [
        outcome
          ~kind:`Function
          ~parameter_titos:
            [
              {
                name = "self";
                titos =
                  [
                    Sinks.LocalReturn;
                    Sinks.ParameterUpdate
                      (AccessPath.Root.PositionalParameter
                         { position = 1; name = "arg1"; positional_only = false });
                  ];
              };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.update";
      ]
    ();
  (* Filters sanitizers *)
  assert_model
    ~filtered_sinks:(Sinks.Set.of_list [Sinks.NamedSink "TestSink"])
    ~model_source:{|
      def test.taint(x: Sanitize[TaintSink[TestSink]]): ...
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
                  Sanitize.from_sinks_only
                    (SanitizeTransform.SinkSet.singleton (SanitizeTransform.Sink.Named "TestSink"));
              };
            ]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ();
  assert_model
    ~filtered_sinks:(Sinks.Set.of_list [Sinks.NamedSink "TestSink"])
    ~model_source:{|
      def test.taint(x: Sanitize[TaintSink[OtherSink]]): ...
    |}
    ~expect:[outcome ~kind:`Function ~analysis_modes:(Model.ModeSet.singleton Obscure) "test.taint"]
    ();
  assert_model
    ~filtered_sinks:(Sinks.Set.of_list [Sinks.NamedSink "TestSink"])
    ~model_source:{|
      def test.taint(x) -> Sanitize[TaintSink[OtherSink]]: ...
    |}
    ~expect:[outcome ~kind:`Function ~analysis_modes:(Model.ModeSet.singleton Obscure) "test.taint"]
    ();
  assert_model
    ~filtered_sinks:(Sinks.Set.of_list [Sinks.NamedSink "TestSink"])
    ~model_source:
      {|
      @SanitizeSingleTrace(TaintSink[OtherSink])
      def test.taint(x): ...
    |}
    ~expect:[outcome ~kind:`Function ~analysis_modes:(Model.ModeSet.singleton Obscure) "test.taint"]
    ();
  ()


let test_access_path _ =
  let module TreeLabel = Abstract.TreeDomain.Label in
  let module TaintPath = ModelParseResult.TaintPath in
  let parse_access_path source =
    PyreMenhirParser.Parser.parse_exn [source]
    |> (function
         | [{ Ast.Node.value = Ast.Statement.Statement.Expression expression; _ }] -> expression
         | _ -> failwith "unexpected statement for acces path")
    |> ModelParser.parse_access_path ~path:None ~location:Ast.Location.any
  in
  let assert_valid_path ~source ~expected =
    match parse_access_path source with
    | Error error ->
        assert_bool
          (Format.asprintf "Unexpected access path error: %a" ModelVerificationError.pp error)
          false
    | Ok path -> assert_equal ~printer:TaintPath.show ~cmp:TaintPath.equal expected path
  in
  let assert_invalid_path ~source ~expected =
    match parse_access_path source with
    | Error error -> assert_equal ~printer:Fn.id expected (ModelVerificationError.display error)
    | Ok _ -> assert_bool (Format.sprintf "Unexpected valid access path for: %s" source) false
  in
  let index_label index = TaintPath.Label.TreeLabel (TreeLabel.Index index) in
  assert_valid_path ~source:"_" ~expected:(TaintPath.Path []);
  assert_valid_path ~source:"_.foo" ~expected:(TaintPath.Path [index_label "foo"]);
  assert_valid_path
    ~source:"_.foo.bar"
    ~expected:(TaintPath.Path [index_label "foo"; index_label "bar"]);
  assert_valid_path ~source:"_['foo']" ~expected:(TaintPath.Path [index_label "foo"]);
  assert_valid_path ~source:"_[\"foo\"]" ~expected:(TaintPath.Path [index_label "foo"]);
  assert_valid_path ~source:"_[0]" ~expected:(TaintPath.Path [index_label "0"]);
  assert_valid_path
    ~source:"_['foo']['bar']"
    ~expected:(TaintPath.Path [index_label "foo"; index_label "bar"]);
  assert_valid_path
    ~source:"_['foo'].bar"
    ~expected:(TaintPath.Path [index_label "foo"; index_label "bar"]);
  assert_valid_path
    ~source:"_.foo['bar']"
    ~expected:(TaintPath.Path [index_label "foo"; index_label "bar"]);
  assert_valid_path
    ~source:"_.foo[0]"
    ~expected:(TaintPath.Path [index_label "foo"; index_label "0"]);
  assert_valid_path
    ~source:"_.keys()"
    ~expected:(TaintPath.Path [TaintPath.Label.TreeLabel AccessPath.dictionary_keys]);
  assert_valid_path
    ~source:"_.all()"
    ~expected:(TaintPath.Path [TaintPath.Label.TreeLabel TreeLabel.AnyIndex]);
  assert_valid_path ~source:"_.all_static_fields()" ~expected:TaintPath.AllStaticFields;
  assert_valid_path
    ~source:"_.parameter_name()"
    ~expected:(TaintPath.Path [TaintPath.Label.ParameterName]);
  assert_valid_path
    ~source:"_[0].keys().foo.all()"
    ~expected:
      (TaintPath.Path
         [
           index_label "0";
           TaintPath.Label.TreeLabel AccessPath.dictionary_keys;
           index_label "foo";
           TaintPath.Label.TreeLabel TreeLabel.AnyIndex;
         ]);
  assert_valid_path
    ~source:"_.all()['a'].bar.parameter_name()"
    ~expected:
      (TaintPath.Path
         [
           TaintPath.Label.TreeLabel TreeLabel.AnyIndex;
           index_label "a";
           index_label "bar";
           TaintPath.Label.ParameterName;
         ]);
  assert_invalid_path
    ~source:"foo"
    ~expected:"`foo` is an invalid access path: access path must start with `_`";
  assert_invalid_path
    ~source:"foo.bar"
    ~expected:"`foo.bar` is an invalid access path: access path must start with `_`";
  assert_invalid_path
    ~source:"_.a-b"
    ~expected:"`_.a - b` is an invalid access path: unexpected expression";
  assert_invalid_path
    ~source:"_[a]"
    ~expected:
      "`_[a]` is an invalid access path: expected int or string literal argument for index access, \
       got `a`";
  assert_invalid_path
    ~source:"_[_.foo]"
    ~expected:
      "`_[_.foo]` is an invalid access path: expected int or string literal argument for index \
       access, got `_.foo`";
  assert_invalid_path
    ~source:"_.keys().something()"
    ~expected:
      "`_.keys().something()` is an invalid access path: unexpected method call `something` \
       (allowed: `keys`, `all`, `all_static_fields`, `parameter_name`)";
  assert_invalid_path
    ~source:"_.foo.all_static_fields()"
    ~expected:
      "`_.foo.all_static_fields()` is an invalid access path: `all_static_fields()` can only be \
       used on `_`";
  assert_invalid_path
    ~source:"_['foo'].all_static_fields()"
    ~expected:
      "`_[\"foo\"].all_static_fields()` is an invalid access path: `all_static_fields()` can only \
       be used on `_`";
  assert_invalid_path
    ~source:"_.all().all_static_fields()"
    ~expected:
      "`_.all().all_static_fields()` is an invalid access path: `all_static_fields()` can only be \
       used on `_`";
  assert_invalid_path
    ~source:"_.keys().all_static_fields()"
    ~expected:
      "`_.keys().all_static_fields()` is an invalid access path: `all_static_fields()` can only be \
       used on `_`";
  assert_invalid_path
    ~source:"_.parameter_name().all_static_fields()"
    ~expected:
      "`_.parameter_name().all_static_fields()` is an invalid access path: `all_static_fields()` \
       can only be used on `_`";
  assert_invalid_path
    ~source:"_.all_static_fields().foo"
    ~expected:
      "`_.all_static_fields().foo` is an invalid access path: cannot access attributes or methods \
       of `all_static_fields()`";
  assert_invalid_path
    ~source:"_.all_static_fields()['foo']"
    ~expected:
      "`_.all_static_fields()[\"foo\"]` is an invalid access path: cannot access attributes or \
       methods of `all_static_fields()`";
  assert_invalid_path
    ~source:"_.all_static_fields().all_static_fields()"
    ~expected:
      "`_.all_static_fields().all_static_fields()` is an invalid access path: cannot access \
       attributes or methods of `all_static_fields()`";
  ()


let test_add_breadcrumb_to_state context =
  let assert_model = assert_model ~context in
  let assert_invalid_model = assert_invalid_model ~context in
  assert_model
    ~model_source:{|
      @AddBreadcrumbToState(Via[special])
      def test.taint(x): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~add_breadcrumbs_to_state:["special"]
          ~analysis_modes:(Model.ModeSet.singleton Obscure)
          "test.taint";
      ]
    ();
  assert_invalid_model
    ~model_source:{|
      @AddBreadcrumbToState(special)
      def test.taint(x): ...
    |}
    ~expect:
      "`AddBreadcrumbToState(special)` is an invalid taint annotation: Invalid expression for \
       breadcrumb, expected `Via[..]`"
    ();
  assert_invalid_model
    ~model_source:
      {|
      @AddBreadcrumbToState(Via[does_not_exist])
      def test.taint(x): ...
    |}
    ~expect:
      "`AddBreadcrumbToState(Via[does_not_exist])` is an invalid taint annotation: Unrecognized \
       Via annotation `does_not_exist`"
    ();
  (* feature is not declared *)
  ()


let test_parse_decorator_modes _ =
  let open Analysis in
  let assert_decorator_modes source expected =
    let source = Test.trim_extra_indentation source in
    let actual =
      ModelParser.parse_model_modes ~path:(PyrePath.create_absolute "/root/test.py") ~source
      |> ModelParser.decorator_actions_from_modes
      |> Ast.Reference.SerializableMap.to_alist
    in
    assert_equal
      ~cmp:[%equal: (Ast.Reference.t * DecoratorPreprocessing.Action.t) list]
      ~printer:[%show: (Ast.Reference.t * DecoratorPreprocessing.Action.t) list]
      expected
      actual
  in
  assert_decorator_modes
    {|
    @SkipObscure
    @SkipOverrides
    def bar.skip_this_decorator2(f): ...

    @SkipObscure
    @SkipOverrides
    def bar.dont_skip(self: TaintInTaintOut[LocalReturn]): ...

    @Sanitize
    def bar.dont_skip2(self: TaintInTaintOut[LocalReturn]): ...

    def baz.dont_skip3(): ...

    @IgnoreDecorator
    def foo.ignore_this_decorator(): ...

    @IgnoreDecorator
    def foo.ignore_decorator_class.__call__(): ...
  |}
    [
      !&"foo.ignore_decorator_class", DecoratorPreprocessing.Action.Discard;
      !&"foo.ignore_this_decorator", DecoratorPreprocessing.Action.Discard;
    ];
  ()


let test_top_level_models context =
  let assert_model = assert_model ~context in
  assert_model
    ~model_source:{|
      @SkipAnalysis
      def test.__top_level__(): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Function
          ~analysis_modes:(Model.ModeSet.of_list [SkipAnalysis])
          "test.$toplevel";
      ]
    ();
  assert_model
    ~source:{|
    class TopLevel:
      x = ...

      def foo(self):
        pass
    |}
    ~model_source:{|
      @SkipAnalysis
      def test.TopLevel.__class_top_level__(): ...
    |}
    ~expect:
      [
        outcome
          ~kind:`Method
          ~analysis_modes:(Model.ModeSet.of_list [SkipAnalysis])
          "test.TopLevel.$class_toplevel";
      ]
    ()


let () =
  "taint_model"
  >::: [
         "access_path" >:: test_access_path;
         "attach_features" >:: test_attach_features;
         "attribute_sanitize" >:: test_attribute_sanitize;
         "class_models" >:: test_class_models;
         "closure_models" >:: test_closure_models;
         "cross_repository_models" >:: test_cross_repository_models;
         "demangle_class_attributes" >:: test_demangle_class_attributes;
         "filter_by_rules" >:: test_filter_by_rules;
         "filter_by_sinks" >:: test_filter_by_sinks;
         "filter_by_sources" >:: test_filter_by_sources;
         "global_sanitize" >:: test_global_sanitize;
         "models_with_if" >:: test_models_with_if;
         "modes" >:: test_modes;
         "parameter_sanitize" >:: test_parameter_sanitize;
         "parameters_sanitize" >:: test_parameters_sanitize;
         "parse_decorator_modes" >:: test_parse_decorator_modes;
         "partial_sinks" >:: test_partial_sinks;
         "return_sanitize" >:: test_return_sanitize;
         "sanitize_single_trace" >:: test_sanitize_single_trace;
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
         "add_breadcrumb_to_state" >:: test_add_breadcrumb_to_state;
         "top_level_models" >:: test_top_level_models;
       ]
  |> Test.run
