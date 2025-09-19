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
open Taint
module PyrePysaApi = Interprocedural.PyrePysaApi

let assert_invalid_model
    ?path
    ?(skip_for_pyrefly = false)
    ?(source_path = "test.py")
    ?source
    ?(sources = [])
    ?pyrefly_expect
    ~model_source
    ~expect
    context
  =
  let source =
    match source with
    | Some source -> source
    | None ->
        {|
              from typing import overload, Union
              from unittest import TestCase

              unannotated_global = source()
              def sink(parameter) -> None: pass
              def sink_with_optional(parameter, firstOptional=1, secondOptional=2) -> None: pass
              def source() -> None: pass
              def taint(x, y) -> None: pass
              def partial_sink(x, y) -> None: pass
              def partial_sink_3(x, y, z) -> None: pass
              def partial_sink_4(w, x, y, z) -> None: pass
              def function_with_args(normal_arg, __anonymous_arg, *args) -> None: pass
              def function_with_kwargs(normal_arg, **kwargs) -> None: pass
              def anonymous_only(__arg1, __arg2, __arg3) -> None: pass
              def anonymous_with_optional(__arg1, __arg2, __arg3=2) -> None: pass
              class C:
                unannotated_class_variable = source()
              @overload
              def function_with_overloads(__key: str, firstNamed: int) -> int: ...
              @overload
              def function_with_overloads(__key: str, secondNamed: str) -> str: ...
              def function_with_overloads(__key: str) -> Union[int, str]:
                return
              @overload
              def function_with_multiple_positions(a: int, c: int) -> str: ...
              def function_with_multiple_positions(a: int, b: int, c: int) -> Union[int, str]: ...
              def function_with_positional_and_named(a: str, __x: str, __y: str, b: str) -> None: ...
            |}
  in
  let sources = (source_path, source) :: sources in
  let pyre_api =
    Test.ScratchPyrePysaProject.setup
      ~context
      ~requires_type_of_expressions:false
      ~force_pyre1:skip_for_pyrefly
      sources
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
            ["TestA", ["TestB"]; "TestB", ["TestA"]; "TestC", ["TestD"]; "TestD", ["TestC"]];
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
  let expect =
    match pyrefly_expect with
    | Some pyrefly_expect when PyrePysaApi.ReadOnly.is_pyrefly pyre_api -> pyrefly_expect
    | _ -> expect
  in
  assert_equal ~printer:Fn.id expect error_message


let test_invalid_models =
  let assert_valid_model ?path ?source ?sources ~model_source =
    assert_invalid_model ?path ?source ?sources ~model_source ~expect:"no failure"
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model ~model_source:"1 + import" ~expect:"Syntax error.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"import foo"
           ~expect:"Unexpected statement: `import foo`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             "def test.sink(parameter: TaintSink[X, Unsupported]) -> TaintSource[A]: ..."
           ~expect:
             "`TaintSink[(X, Unsupported)]` is an invalid taint annotation: Unsupported taint sink \
              `Unsupported`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink(parameter: TaintSink[UserControlled]): ..."
           ~expect:
             "`TaintSink[UserControlled]` is an invalid taint annotation: Unsupported taint sink \
              `UserControlled`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink(parameter: SkipAnalysis): ..."
           ~expect:
             "`SkipAnalysis` is an invalid taint annotation: Failed to parse the given taint \
              annotation.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink(parameter: TaintSink[X, Y, LocalReturn]): ..."
           ~expect:
             "`TaintSink[(X, Y, LocalReturn)]` is an invalid taint annotation: Unsupported taint \
              sink `LocalReturn`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.source() -> TaintSource[Invalid]: ..."
           ~expect:
             "`TaintSource[Invalid]` is an invalid taint annotation: Unsupported taint source \
              `Invalid`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.source() -> TaintInTaintOut: ..."
           ~expect:"Invalid model for `test.source`: Invalid return annotation `TaintInTaintOut`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink(parameter: TaintInTaintOut[Test]): ..."
           ~expect:
             "`TaintInTaintOut[Test]` is an invalid taint annotation: Unsupported taint in taint \
              out specification `Test`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink(parameter: TaintInTaintOut[Transform[Invalid]]): ..."
           ~expect:
             "`TaintInTaintOut[Transform[Invalid]]` is an invalid taint annotation: Unsupported \
              transform `Invalid`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink(parameter: InvalidTaintDirection[Test]): ..."
           ~expect:
             "`InvalidTaintDirection[Test]` is an invalid taint annotation: Failed to parse the \
              given taint annotation.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             "def test.partial_sink(x: PartialSink[TestFirst], y: PartialSink[TestB]): ..."
           ~expect:
             "`PartialSink[TestFirst]` is an invalid taint annotation: Unrecognized partial sink \
              `TestFirst` (choices: `TestA, TestB, TestC, TestD`)";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             "def test.partial_sink(x: PartialSink[TestA], y: PartialSink[TestSecond]): ..."
           ~expect:
             "`PartialSink[TestSecond]` is an invalid taint annotation: Unrecognized partial sink \
              `TestSecond` (choices: `TestA, TestB, TestC, TestD`)";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.partial_sink(x: PartialSink[TestA], y: PartialSink[TestD]): ..."
           ~expect:"Cannot find any matching partial sink for `PartialSink[TestA]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.partial_sink(x: PartialSink[TestA], y): ..."
           ~expect:"Cannot find any matching partial sink for `PartialSink[TestA]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.partial_sink(x: PartialSink[TestA], y: PartialSink[TestA]): ..."
           ~expect:"Cannot find any matching partial sink for `PartialSink[TestA]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      def test.partial_sink_3(
        x: PartialSink[TestA],
        y: PartialSink[TestB],
        z: PartialSink[TestB]
      ): ...
       |}
           ~expect:"Cannot find any matching partial sink for `PartialSink[TestB]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:
             {|
      def test.partial_sink_4(
        w: PartialSink[TestA],
        x: PartialSink[TestB],
        y: PartialSink[TestC],
        z: PartialSink[TestD]
      ): ...
      |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.partial_sink(x: PartialSink[XA], y: PartialSink[XB]): ..."
           ~expect:
             "`PartialSink[XA]` is an invalid taint annotation: Unrecognized partial sink `XA` \
              (choices: `TestA, TestB, TestC, TestD`)";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink(parameter: TaintSource.foo(A)): ..."
           ~expect:
             {|`TaintSource.foo(A)` is an invalid taint annotation: Failed to parse the given taint annotation.|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:"def f(x: int): ..."
           ~model_source:"def test.f(x) -> TaintSource[WithSubkind[X, Y, Z]]: ..."
           ~expect:
             "`TaintSource[WithSubkind[(X, Y, Z)]]` is an invalid taint annotation: Invalid \
              expression for taint subkind: (X, Y, Z)";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:"def f(x: int): ..."
           ~model_source:"def test.f(x) -> TaintSource[Collapse[Subkind]]: ..."
           ~expect:
             "`TaintSource[Collapse[Subkind]]` is an invalid taint annotation: Unsupported taint \
              source `Collapse`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:"def f(x: int): ..."
           ~model_source:"def test.f(x) -> TaintSource[Collapse]: ..."
           ~expect:
             "`TaintSource[Collapse]` is an invalid taint annotation: `Collapse` can only be used \
              within `TaintInTaintOut[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:"def f(x: int): ..."
           ~model_source:"def test.f(x) -> TaintSource[NoCollapse]: ..."
           ~expect:
             "`TaintSource[NoCollapse]` is an invalid taint annotation: `NoCollapse` can only be \
              used within `TaintInTaintOut[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:"def f(x: int): ..."
           ~model_source:"def test.f(x) -> TaintSource[WithSubkind[A][B]]: ..."
           ~expect:
             "`TaintSource[WithSubkind[A][B]]` is an invalid taint annotation: Invalid expression \
              for taint kind: WithSubkind[A][B]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:"def f(x: int): ..."
           ~model_source:"def test.f(x) -> TaintSource[Test.attribute]: ..."
           ~expect:
             "`TaintSource[Test.attribute]` is an invalid taint annotation: Invalid expression for \
              taint kind: Test.attribute";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:"def f(x: int): ..."
           ~model_source:"def test.f(x) -> TaintSource[ViaAttributeName]: ..."
           ~expect:
             "`TaintSource[ViaAttributeName]` is an invalid taint annotation: `ViaAttributeName` \
              can only be used in attribute or global models.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:"def f(x: int): ..."
           ~model_source:{|def test.f(x) -> TaintSource[ViaAttributeName[WithTag["tag"]]]: ...|}
           ~expect:
             "`TaintSource[ViaAttributeName[WithTag[\"tag\"]]]` is an invalid taint annotation: \
              `ViaAttributeName` can only be used in attribute or global models.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:"def f(x: int): ..."
           ~model_source:"def test.f(x) -> TaintSink[ViaAttributeName]: ..."
           ~expect:
             "`TaintSink[ViaAttributeName]` is an invalid taint annotation: `ViaAttributeName` can \
              only be used in attribute or global models.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:"def f(x: int): ..."
           ~model_source:{|def test.f(x) -> TaintSink[ViaAttributeName[WithTag["tag"]]]: ...|}
           ~expect:
             "`TaintSink[ViaAttributeName[WithTag[\"tag\"]]]` is an invalid taint annotation: \
              `ViaAttributeName` can only be used in attribute or global models.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      class C:
        x: int = 0
      |}
           ~model_source:"test.C.x: ViaTypeOf[a.b] = ..."
           ~expect:
             "`ViaTypeOf[a.b]` is an invalid taint annotation: Invalid expression in `ViaTypeOf` \
              declaration: a.b";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:"def test.partial_sink(x: PartialSink[TestA], y: PartialSink[TestB]): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model ~model_source:"def test.sink(): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model ~model_source:"def test.sink_with_optional(): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model ~model_source:"def test.sink_with_optional(parameter): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:"def test.sink_with_optional(parameter, firstOptional): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:
             "def test.sink_with_optional(parameter, firstOptional, secondOptional): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             "def test.sink_with_optional(parameter, firstOptional, secondOptional, \
              thirdOptional): ..."
           ~expect:
             "Model signature parameters for `test.sink_with_optional` do not match implementation \
              `def (parameter: unknown, firstOptional: unknown = ..., secondOptional: unknown = \
              ...) -> None: ...`. Reason: unexpected named parameter: `thirdOptional`."
           ~pyrefly_expect:
             "Model signature parameters for `test.sink_with_optional` do not match implementation \
              `def (parameter: Unknown, firstOptional: int | Unknown = ..., secondOptional: int | \
              Unknown = ...) -> None: ...`. Reason: unexpected named parameter: `thirdOptional`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink_with_optional(parameter, firstBad, secondBad): ..."
           ~expect:
             "Model signature parameters for `test.sink_with_optional` do not match implementation \
              `def (parameter: unknown, firstOptional: unknown = ..., secondOptional: unknown = \
              ...) -> None: ...`. Reason: unexpected named parameter: `firstBad`."
           ~pyrefly_expect:
             "Model signature parameters for `test.sink_with_optional` do not match implementation \
              `def (parameter: Unknown, firstOptional: int | Unknown = ..., secondOptional: int | \
              Unknown = ...) -> None: ...`. Reason: unexpected named parameter: `firstBad`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink_with_optional(parameter, *args): ..."
           ~expect:
             "Model signature parameters for `test.sink_with_optional` do not match implementation \
              `def (parameter: unknown, firstOptional: unknown = ..., secondOptional: unknown = \
              ...) -> None: ...`. Reason: unexpected star parameter."
           ~pyrefly_expect:
             "Model signature parameters for `test.sink_with_optional` do not match implementation \
              `def (parameter: Unknown, firstOptional: int | Unknown = ..., secondOptional: int | \
              Unknown = ...) -> None: ...`. Reason: unexpected star parameter.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink_with_optional(parameter, **kwargs): ..."
           ~expect:
             "Model signature parameters for `test.sink_with_optional` do not match implementation \
              `def (parameter: unknown, firstOptional: unknown = ..., secondOptional: unknown = \
              ...) -> None: ...`. Reason: unexpected star star parameter."
           ~pyrefly_expect:
             "Model signature parameters for `test.sink_with_optional` do not match implementation \
              `def (parameter: Unknown, firstOptional: int | Unknown = ..., secondOptional: int | \
              Unknown = ...) -> None: ...`. Reason: unexpected star star parameter.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink_with_optional(__parameter): ..."
           ~expect:
             "Model signature parameters for `test.sink_with_optional` do not match implementation \
              `def (parameter: unknown, firstOptional: unknown = ..., secondOptional: unknown = \
              ...) -> None: ...`. Reason: unexpected positional only parameter: `__parameter`."
           ~pyrefly_expect:
             "Model signature parameters for `test.sink_with_optional` do not match implementation \
              `def (parameter: Unknown, firstOptional: int | Unknown = ..., secondOptional: int | \
              Unknown = ...) -> None: ...`. Reason: unexpected positional only parameter: \
              `__parameter`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             "def test.function_with_args(normal_arg, __random_name, named_arg, *args): ..."
           ~expect:
             "Model signature parameters for `test.function_with_args` do not match implementation \
              `def (normal_arg: unknown, __arg1: unknown, *args) -> None: ...`. Reason: unexpected \
              named parameter: `named_arg`."
           ~pyrefly_expect:
             "Model signature parameters for `test.function_with_args` do not match implementation \
              `def (normal_arg: Unknown, __anonymous_arg: Unknown, args) -> None: ...`. Reason: \
              unexpected named parameter: `named_arg`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:"def test.function_with_args(normal_arg, __random_name, *args): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             "def test.function_with_args(normal_arg, __random_name, *, named_arg, *args): ..."
           ~expect:
             "Model signature parameters for `test.function_with_args` do not match implementation \
              `def (normal_arg: unknown, __arg1: unknown, *args) -> None: ...`. Reason: unexpected \
              named parameter: `named_arg`."
           ~pyrefly_expect:
             "Model signature parameters for `test.function_with_args` do not match implementation \
              `def (normal_arg: Unknown, __anonymous_arg: Unknown, args) -> None: ...`. Reason: \
              unexpected named parameter: `named_arg`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:
             "def test.function_with_args(normal_arg, __random_name, __random_name_2, *args): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model ~model_source:"def test.function_with_kwargs(normal_arg, **kwargs): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.function_with_kwargs(normal_arg, crazy_arg, **kwargs): ..."
           ~expect:
             "Model signature parameters for `test.function_with_kwargs` do not match \
              implementation `def (normal_arg: unknown, **kwargs: unknown) -> None: ...`. Reason: \
              unexpected named parameter: `crazy_arg`."
           ~pyrefly_expect:
             "Model signature parameters for `test.function_with_kwargs` do not match \
              implementation `def (normal_arg: Unknown, kwargs: Unknown) -> None: ...`. Reason: \
              unexpected named parameter: `crazy_arg`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model ~model_source:"def test.function_with_overloads(__key): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model ~model_source:"def test.function_with_overloads(firstNamed): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model ~model_source:"def test.function_with_overloads(secondNamed): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.function_with_overloads(unknownNamed): ..."
           ~expect:
             "Model signature parameters for `test.function_with_overloads` do not match \
              implementation `def (__arg0: str) -> int | str: ... | def (__arg0: str, firstNamed: \
              int) -> int: ... | def (__arg0: str, secondNamed: str) -> str: ...`. Reason: \
              unexpected named parameter: `unknownNamed`."
           ~pyrefly_expect:
             "Model signature parameters for `test.function_with_overloads` do not match \
              implementation `def (__key: str, firstNamed: int) -> int: ... | def (__key: str, \
              secondNamed: str) -> str: ...`. Reason: unexpected named parameter: `unknownNamed`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.function_with_overloads(firstNamed, secondNamed): ..."
           ~expect:
             "Model signature parameters for `test.function_with_overloads` do not match \
              implementation `def (__arg0: str) -> int | str: ... | def (__arg0: str, firstNamed: \
              int) -> int: ... | def (__arg0: str, secondNamed: str) -> str: ...`. Reasons:\n\
              unexpected named parameter: `secondNamed` in overload `def (__arg0: str) -> int | \
              str: ...`\n\
              unexpected named parameter: `firstNamed` in overload `def (__arg0: str) -> int | \
              str: ...`\n\
              unexpected named parameter: `secondNamed` in overload `def (__arg0: str, firstNamed: \
              int) -> int: ...`\n\
              unexpected named parameter: `firstNamed` in overload `def (__arg0: str, secondNamed: \
              str) -> str: ...`"
           ~pyrefly_expect:
             "Model signature parameters for `test.function_with_overloads` do not match \
              implementation `def (__key: str, firstNamed: int) -> int: ... | def (__key: str, \
              secondNamed: str) -> str: ...`. Reasons:\n\
              unexpected named parameter: `secondNamed` in overload `def (__key: str, firstNamed: \
              int) -> int: ...`\n\
              unexpected named parameter: `firstNamed` in overload `def (__key: str, secondNamed: \
              str) -> str: ...`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.function_with_multiple_positions(c): ..."
           ~expect:
             "Model signature parameters for `test.function_with_multiple_positions` do not match \
              implementation `def (a: int, b: int, c: int) -> int | str: ... | def (a: int, c: \
              int) -> str: ...`. Reason: invalid position 0 for named parameter `c` (valid options \
              are {formal(c, position=1), formal(c, position=2)})."
           ~skip_for_pyrefly:true;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.function_with_positional_and_named(__x): ..."
           ~expect:
             "Model signature parameters for `test.function_with_positional_and_named` do not \
              match implementation `def (a: str, __arg1: str, __arg2: str, b: str) -> None: ...`. \
              Reason: unexpected positional only parameter: `__x` at position: 0 (0 not in {1, \
              2})."
           ~pyrefly_expect:
             "Model signature parameters for `test.function_with_positional_and_named` do not \
              match implementation `def (a: str, __x: str, __y: str, b: str) -> None: ...`. \
              Reason: unexpected positional only parameter: `__x` at position: 0 (0 not in {1, \
              2}).";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:"def test.function_with_positional_and_named(a, __random_name): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:"def test.function_with_positional_and_named(a, __random_name, b): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.function_with_positional_and_named(a, __x, b, __y): ..."
           ~expect:
             "Model signature parameters for `test.function_with_positional_and_named` do not \
              match implementation `def (a: str, __arg1: str, __arg2: str, b: str) -> None: ...`. \
              Reason: unexpected positional only parameter: `__y` at position: 3 (3 not in {1, \
              2})."
           ~pyrefly_expect:
             "Model signature parameters for `test.function_with_positional_and_named` do not \
              match implementation `def (a: str, __x: str, __y: str, b: str) -> None: ...`. \
              Reason: unexpected positional only parameter: `__y` at position: 3 (3 not in {1, \
              2}).";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:"def test.function_with_kwargs(normal_arg, *, crazy_arg, **kwargs): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model ~model_source:"def test.anonymous_only(__a1, __a2, __a3): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.anonymous_only(parameter: Any): ..."
           ~expect:
             "Model signature parameters for `test.anonymous_only` do not match implementation \
              `def (__arg0: unknown, __arg1: unknown, __arg2: unknown) -> None: ...`. Reason: \
              unexpected named parameter: `parameter`."
           ~pyrefly_expect:
             "Model signature parameters for `test.anonymous_only` do not match implementation \
              `def (__arg1: Unknown, __arg2: Unknown, __arg3: Unknown) -> None: ...`. Reason: \
              unexpected named parameter: `parameter`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model ~model_source:"def test.anonymous_with_optional(__a1, __a2): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:"def test.anonymous_with_optional(__a1, __a2, __a3=...): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink(parameter: Any): ..."
           ~expect:
             "`Any` is an invalid taint annotation: Failed to parse the given taint annotation.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~path:"broken_model.pysa"
           ~model_source:"def test.sink(parameter: Any): ..."
           ~expect:
             "broken_model.pysa:1: `Any` is an invalid taint annotation: Failed to parse the given \
              taint annotation.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink(parameter: TaintSink[Test, Via[bad_feature]]): ..."
           ~expect:
             "`TaintSink[(Test, Via[bad_feature])]` is an invalid taint annotation: Unrecognized \
              Via annotation `bad_feature`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink(parameter: TaintSink[Updates[self]]): ..."
           ~expect:
             "`TaintSink[Updates[self]]` is an invalid taint annotation: `Updates` can only be \
              used within `TaintInTaintOut[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model ~model_source:"test.unannotated_global: TaintSink[Test]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"test.missing_global: TaintSink[Test]"
           ~expect:"Module `test` does not define `test.missing_global`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      class C:
        a: int = 1
     |}
           ~model_source:"test.C.b: TaintSink[Test] = ..."
           ~expect:"Class `test.C` has no attribute `b`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model ~model_source:"test.C.unannotated_class_variable: TaintSink[Test]";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"test.C.missing: TaintSink[Test]"
           ~expect:"Class `test.C` has no attribute `missing`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"test.C().unannotated_class_variable: TaintSink[Test]"
           ~expect:
             "Invalid identifier: `test.C().unannotated_class_variable`. Expected a \
              fully-qualified name.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"test.C.unannotated_class_variable: Foo"
           ~expect:"`Foo` is an invalid taint annotation: Unsupported annotation for attributes";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      class test.ClassSinkWithMethod(TaintSink[TestSink]):
          def method(self): ...
      |}
           ~expect:"Class model for `test.ClassSinkWithMethod` must have a body of `...`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:{|
      def foo(): TaintSource[A]
    |}
           ~expect:"Callable model for `foo` must have a body of `...`.";
      (* Attach syntax. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink(parameter: AttachToSink): ..."
           ~expect:
             "`AttachToSink` is an invalid taint annotation: Failed to parse the given taint \
              annotation.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink(parameter: AttachToSink[feature]): ..."
           ~expect:
             "`AttachToSink[feature]` is an invalid taint annotation: All parameters to \
              `AttachToSink` must be of the form `Via[feature]`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink(parameter: AttachToTito[feature]): ..."
           ~expect:
             "`AttachToTito[feature]` is an invalid taint annotation: All parameters to \
              `AttachToTito` must be of the form `Via[feature]`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.source() -> AttachToSource[feature]: ..."
           ~expect:
             "`AttachToSource[feature]` is an invalid taint annotation: All parameters to \
              `AttachToSource` must be of the form `Via[feature]`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.source() -> AttachToSource[Via[featureA], ReturnPath[_]]: ..."
           ~expect:
             "`AttachToSource[(Via[featureA], ReturnPath[_])]` is an invalid taint annotation: All \
              parameters to `AttachToSource` must be of the form `Via[feature]`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             "def test.sink(parameter: AttachToSink[Via[featureA], ParameterPath[_]]): ..."
           ~expect:
             "`AttachToSink[(Via[featureA], ParameterPath[_])]` is an invalid taint annotation: \
              All parameters to `AttachToSink` must be of the form `Via[feature]`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             "def test.sink(parameter: AttachToTito[Via[featureA], ParameterPath[_]]): ..."
           ~expect:
             "`AttachToTito[(Via[featureA], ParameterPath[_])]` is an invalid taint annotation: \
              All parameters to `AttachToTito` must be of the form `Via[feature]`.";
      (* Multiple features. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:"def test.sink(parameter: AttachToSink[Via[featureA, featureB]]): ...";
      (* Default values must be `...`. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink(parameter = TaintSink[Test]): ..."
           ~expect:
             "Default values of `test.sink`'s parameters must be `...`. Did you mean to write \
              `parameter: TaintSink[Test]`?";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink(parameter = 1): ..."
           ~expect:
             "Default values of `test.sink`'s parameters must be `...`. Did you mean to write \
              `parameter: 1`?";
      (* Input and output path specification. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink(parameter: TaintSink[Test, ParameterPath[test]]): ..."
           ~expect:"`test` is an invalid access path: access path must start with `_`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             "def test.sink(parameter: TaintSink[Test, ParameterPath[_.unknown()]]): ..."
           ~expect:
             "`_.unknown()` is an invalid access path: unexpected method call `unknown` (allowed: \
              `keys`, `all`, `all_static_fields`, `parameter_name`)";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink(parameter: TaintSink[Test, ReturnPath[_[0]]]): ..."
           ~expect:
             "`TaintSink[(Test, ReturnPath[_[0]])]` is an invalid taint annotation: `ReturnPath[]` \
              can only be used as a return annotation or within `TaintInTaintOut[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink(parameter: TaintSink[Test, UpdatePath[_[0]]]): ..."
           ~expect:
             "`TaintSink[(Test, UpdatePath[_[0]])]` is an invalid taint annotation: `UpdatePath` \
              can only be used within `TaintInTaintOut[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             "def test.sink(parameter: AppliesTo[0, TaintSink[Test, ParameterPath[_[1]]]]): ..."
           ~expect:
             "`AppliesTo[(0, TaintSink[(Test, ParameterPath[_[1]])])]` is an invalid taint \
              annotation: `AppliesTo[]` cannot be used with `ParameterPath[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             "def test.sink(parameter: AppliesTo[1, TaintSink[Test, UpdatePath[_[0]]]]): ..."
           ~expect:
             "`AppliesTo[(1, TaintSink[(Test, UpdatePath[_[0]])])]` is an invalid taint \
              annotation: `UpdatePath` can only be used within `TaintInTaintOut[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.taint(y: TaintSource[Test, ReturnPath[_[0]]]): ..."
           ~expect:
             "`TaintSource[(Test, ReturnPath[_[0]])]` is an invalid taint annotation: \
              `ReturnPath[]` can only be used as a return annotation or within `TaintInTaintOut[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.taint(y: TaintSource[Test, UpdatePath[_[0]]]): ..."
           ~expect:
             "`TaintSource[(Test, UpdatePath[_[0]])]` is an invalid taint annotation: `UpdatePath` \
              can only be used within `TaintInTaintOut[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             "def test.taint(y: AppliesTo[0, TaintSource[Test, ParameterPath[_[1]]]]): ..."
           ~expect:
             "`AppliesTo[(0, TaintSource[(Test, ParameterPath[_[1]])])]` is an invalid taint \
              annotation: `AppliesTo[]` cannot be used with `ParameterPath[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.taint(y: AppliesTo[1, TaintSource[Test, UpdatePath[_[0]]]]): ..."
           ~expect:
             "`AppliesTo[(1, TaintSource[(Test, UpdatePath[_[0]])])]` is an invalid taint \
              annotation: `UpdatePath` can only be used within `TaintInTaintOut[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink() -> TaintSink[Test, ParameterPath[_[0]]]: ..."
           ~expect:
             "`TaintSink[(Test, ParameterPath[_[0]])]` is an invalid taint annotation: \
              `ParameterPath` can only be used on parameters";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink() -> TaintSink[Test, UpdatePath[_[0]]]: ..."
           ~expect:
             "`TaintSink[(Test, UpdatePath[_[0]])]` is an invalid taint annotation: `UpdatePath` \
              can only be used within `TaintInTaintOut[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.sink() -> AppliesTo[1, TaintSink[Test, ReturnPath[_[0]]]]: ..."
           ~expect:
             "`AppliesTo[(1, TaintSink[(Test, ReturnPath[_[0]])])]` is an invalid taint \
              annotation: `AppliesTo[]` cannot be used with `ReturnPath[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.taint() -> TaintSource[Test, ParameterPath[_[0]]]: ..."
           ~expect:
             "`TaintSource[(Test, ParameterPath[_[0]])]` is an invalid taint annotation: \
              `ParameterPath` can only be used on parameters";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.taint() -> TaintSource[Test, UpdatePath[_[0]]]: ..."
           ~expect:
             "`TaintSource[(Test, UpdatePath[_[0]])]` is an invalid taint annotation: `UpdatePath` \
              can only be used within `TaintInTaintOut[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             "def test.taint() -> AppliesTo[1, TaintSource[Test, ReturnPath[_[0]]]]: ..."
           ~expect:
             "`AppliesTo[(1, TaintSource[(Test, ReturnPath[_[0]])])]` is an invalid taint \
              annotation: `AppliesTo[]` cannot be used with `ReturnPath[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.taint(x: TaintInTaintOut[UpdatePath[_[0]]]): ..."
           ~expect:
             "Invalid model for `test.taint`: Invalid UpdatePath annotation for TaintInTaintOut \
              annotation";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             "def test.taint(x: AppliesTo[0, TaintInTaintOut[ParameterPath[_[0]]]]): ..."
           ~expect:
             "`AppliesTo[(0, TaintInTaintOut[ParameterPath[_[0]]])]` is an invalid taint \
              annotation: `AppliesTo[]` cannot be used with `ParameterPath[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.taint(x, y: TaintInTaintOut[Updates[x], ReturnPath[_[0]]]): ..."
           ~expect:
             "Invalid model for `test.taint`: Invalid ReturnPath annotation for Updates annotation";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             "def test.taint(x: TaintInTaintOut[UpdatePath[_.all_static_fields()]]): ..."
           ~expect:
             "`TaintInTaintOut[UpdatePath[_.all_static_fields()]]` is an invalid taint annotation: \
              `all_static_fields()` is not allowed within `TaintInTaintOut[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             "def test.taint(x: TaintInTaintOut[ParameterPath[_.all_static_fields()]]): ..."
           ~expect:
             "`TaintInTaintOut[ParameterPath[_.all_static_fields()]]` is an invalid taint \
              annotation: `all_static_fields()` is not allowed within `TaintInTaintOut[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             "def test.taint(x: TaintInTaintOut[ReturnPath[_.all_static_fields()]]): ..."
           ~expect:
             "`TaintInTaintOut[ReturnPath[_.all_static_fields()]]` is an invalid taint annotation: \
              `all_static_fields()` is not allowed within `TaintInTaintOut[]`";
      (* Collapse depth specification. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.taint(x: TaintInTaintOut[CollapseDepth[a]]): ..."
           ~expect:
             "`TaintInTaintOut[CollapseDepth[a]]` is an invalid taint annotation: expected \
              non-negative int literal argument for CollapseDepth, got `a`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test.taint(x: TaintInTaintOut[CollapseDepth[-1]]): ..."
           ~expect:
             "`TaintInTaintOut[CollapseDepth[-1]]` is an invalid taint annotation: expected \
              non-negative int literal argument for CollapseDepth, got `-1`";
      (* ViaValueOf models must specify existing parameters. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             "def test.sink(parameter) -> TaintSource[Test, ViaValueOf[nonexistent_parameter]]: ..."
           ~expect:
             "`TaintSource[(Test, ViaValueOf[nonexistent_parameter])]` is an invalid taint \
              annotation: No such parameter `nonexistent_parameter`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
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
             "Model signature parameters for `test.C.foo` do not match implementation `def (self: \
              C) -> int: ...`. Reason: unexpected named parameter: `value`."
           ~pyrefly_expect:
             "Model signature parameters for `test.C.foo` do not match implementation `def (self: \
              test.C) -> int: ...`. Reason: unexpected named parameter: `value`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
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
           ~model_source:
             {|
      @foo.setter
      def test.C.foo(self) -> TaintSource[A]: ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      def accidental_decorator_passed_in(): ...
      |}
           ~model_source:
             {|
      @decorated
      def test.accidental_decorator_passed_in() -> TaintSource[Test]: ...
    |}
           ~expect:
             "Unexpected decorators found when parsing model for \
              `test.accidental_decorator_passed_in`: `decorated`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
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
           ~expect:
             "Unexpected decorators found when parsing model for `test.C.foo`: `wrong_name.setter`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      def accidental_decorator_passed_in(): ...
      |}
           ~model_source:
             {|
      @custom_property
      def test.accidental_decorator_passed_in() -> TaintSource[Test]: ...
    |}
           ~expect:
             "Unexpected decorators found when parsing model for \
              `test.accidental_decorator_passed_in`: `custom_property`. If you're looking to model \
              a custom property decorator, use the @property decorator.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
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
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      def unittest.TestCase.assertIsNotNone(self, x: TaintSink[Test]): ...
    |}
           ~expect:
             "The modelled function `unittest.TestCase.assertIsNotNone` is an imported function, \
              please model `unittest.case.TestCase.assertIsNotNone` directly."
           ~pyrefly_expect:"Module `unittest` does not define `unittest.TestCase.assertIsNotNone`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
        def test.sink(parameter: TaintSink[Test, Via[a-feature]]):
          ...
    |}
           ~expect:
             "`TaintSink[(Test, Via[a - feature])]` is an invalid taint annotation: Invalid \
              expression for breadcrumb: a - feature";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:"def partial_sink(x, y) -> None: ..."
           ~model_source:
             "def test.partial_sink(x: PartialSink[NonexistentA], y: PartialSink[NonexistentB]): \
              ..."
           ~expect:
             "`PartialSink[NonexistentA]` is an invalid taint annotation: Unrecognized partial \
              sink `NonexistentA` (choices: `TestA, TestB, TestC, TestD`)";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:"def f(parameter): ..."
           ~model_source:
             "def test.f(parameter: CrossRepositoryTaint[TaintSource[UserControlled]]): ..."
           ~expect:
             "`CrossRepositoryTaint[TaintSource[UserControlled]]` is an invalid taint annotation: \
              Cross repository taint must be of the form CrossRepositoryTaint[taint, \
              canonical_name, canonical_port, producer_id, trace_length].";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:"def f(parameter): ..."
           ~model_source:
             "def test.f(parameter: CrossRepositoryTaint[TaintSource[UserControlled], \
              some_canonical_name, 'formal(0)', 0]): ..."
           ~expect:
             "`CrossRepositoryTaint[(TaintSource[UserControlled], some_canonical_name, \
              \"formal(0)\", 0)]` is an invalid taint annotation: Cross repository taint must be \
              of the form CrossRepositoryTaint[taint, canonical_name, canonical_port, producer_id, \
              trace_length].";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:"def f(parameter): ..."
           ~model_source:
             "def test.f(parameter: CrossRepositoryTaint[TaintSource[UserControlled], \
              'some_canonical_name', 0, 0]): ..."
           ~expect:
             "`CrossRepositoryTaint[(TaintSource[UserControlled], \"some_canonical_name\", 0, 0)]` \
              is an invalid taint annotation: Cross repository taint must be of the form \
              CrossRepositoryTaint[taint, canonical_name, canonical_port, producer_id, \
              trace_length].";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:"def f(parameter): ..."
           ~model_source:
             "def test.f(parameter: CrossRepositoryTaint[TaintSource[UserControlled], \
              'canonical_name', 'formal(x)', 0, 'oh']): ..."
           ~expect:
             "`CrossRepositoryTaint[(TaintSource[UserControlled], \"canonical_name\", \
              \"formal(x)\", 0, \"oh\")]` is an invalid taint annotation: Cross repository taint \
              must be of the form CrossRepositoryTaint[taint, canonical_name, canonical_port, \
              producer_id, trace_length].";
      (* Ensure that we're verifying models against the undecorated signature. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
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
           ~model_source:"def test.foo(parameter): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
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
           ~model_source:"def test.foo(): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source:{|
      def foo(__: int, kwonly: str) -> None: ...
    |}
           ~model_source:"def test.foo(__: TaintSource[A], kwonly): ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      class C:
        @property
        def foo(self) -> int: ...
    |}
           ~model_source:"test.C.foo: TaintSource[A] = ..."
           ~expect:
             "The function, method or property `test.C.foo` is not a valid attribute - did you \
              mean to use `def test.C.foo(): ...`?";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      class C:
        foo = 1
      class D(C):
        pass
    |}
           ~model_source:"test.D.foo: TaintSource[A] = ..."
           ~expect:"Class `test.D` has no attribute `foo`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source:{|
      class C:
        foo = 1
      class D(C):
        pass
    |}
           ~model_source:"test.C.foo: TaintSource[A] = ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source:
             {|
      class C:
        foo = 1
      class D(C):
        def __init__(self):
          self.foo = 2
    |}
           ~model_source:"test.D.foo: TaintSource[A] = ...";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      def foo(x):
        ...
    |}
           ~model_source:"def test.foo(x) -> TaintSource[A[Subkind]]: ..."
           ~expect:
             "`TaintSource[A[Subkind]]` is an invalid taint annotation: Unsupported taint source \
              `A`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      def foo(x):
        ...
    |}
           ~model_source:"def test.foo(x: TaintSink[X[Subkind]]): ..."
           ~expect:
             "`TaintSink[X[Subkind]]` is an invalid taint annotation: Unsupported taint sink `X`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      def foo(x):
        ...
    |}
           ~model_source:
             {|
      @Sanitize(TaintInTaintOut[LocalReturn])
      def test.foo(x): ...
    |}
           ~expect:
             {|`Sanitize(TaintInTaintOut[LocalReturn])` is an invalid taint annotation: Failed to parse the given taint annotation.|};
      (* Test source- and sink- specific tito parsing. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source:{|
      def foo(x):
        ...
    |}
           ~model_source:
             {|
      @Sanitize(TaintInTaintOut[TaintSource[A]])
      def test.foo(x): ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source:{|
      def foo(x):
        ...
    |}
           ~model_source:
             {|
      @Sanitize(TaintInTaintOut[TaintSink[Test]])
      def test.foo(x): ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      def foo(x):
        ...
    |}
           ~model_source:{|
      @Sanitize(TaintSource[A])
      def test.foo(x): ...
    |}
           ~expect:
             "`Sanitize(TaintSource[A])` is an invalid taint annotation: `TaintSource` is not \
              supported within `Sanitize(...)`. Did you mean to use `SanitizeSingleTrace(...)`?";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
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
             {|`Sanitize(TaintSource[(A, Via[featureA])])` is an invalid taint annotation: `TaintSource[A, Via[featureA]]` is not supported within `Sanitize[...]`|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source:{|
      def outer():
        def inner():
          return 1
    |}
           ~model_source:"def test.outer.inner() -> TaintSource[Test]: ...";
    ]


let test_invalid_model_queries =
  let assert_valid_model ?path ?source ?sources ~model_source =
    assert_invalid_model ?path ?source ?sources ~model_source ~expect:"no failure"
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        find = "functions",
        where = name.matches("foo"),
        model = Returns(TaintSource[Test])
      )
    |}
           ~expect:{|Missing required parameter `name` in model query.|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        where = name.matches("foo"),
        model = Returns(TaintSource[Test])
      )
    |}
           ~expect:{|Missing required parameter `find` in model query.|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        model = Returns(TaintSource[Test])
      )
    |}
           ~expect:{|Missing required parameter `where` in model query.|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = name.matches("foo")
      )
    |}
           ~expect:{|Missing required parameter `model` in model query.|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
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
             \   query names should be unique within each file.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
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
             \   query names should be unique within each file.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
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
             "`Returns` is not a valid model for model queries with find clause of kind \
              `attributes`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
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
              `attributes`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
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
              `attributes`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = return_annotation.fully_qualified.equals("int"),
        model = AttributeModel(TaintSource[Test])
      )
    |}
           ~expect:
             "`AttributeModel` is not a valid model for model queries with find clause of kind \
              `functions`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = return_annotation.fully_qualified.equals("int"),
        model = AttributeModel(TaintSource[Test])
      )
    |}
           ~expect:
             "`AttributeModel` is not a valid model for model queries with find clause of kind \
              `methods`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
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
             "`any_parameter.annotation.is_annotated_type` is not a valid constraint for model \
              queries with find clause of kind `attributes`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "attributes",
        where = AnyOf(
          cls.name.matches("foo"),
          any_parameter.annotation.is_annotated_type()
        ),
        model = AttributeModel(TaintSource[Test])
      )
    |}
           ~expect:
             "`any_parameter.annotation.is_annotated_type` is not a valid constraint for model \
              queries with find clause of kind `attributes`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "attributes",
        where = AnyOf(
          cls.name.matches("foo"),
          any_parameter.annotation.fully_qualified.equals("int")
        ),
        model = AttributeModel(TaintSource[Test])
      )
    |}
           ~expect:
             "`any_parameter.annotation.fully_qualified.equals` is not a valid constraint for \
              model queries with find clause of kind `attributes`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "attributes",
        where = AnyOf(
          cls.name.matches("foo"),
          any_parameter.annotation.fully_qualified.matches("int")
        ),
        model = AttributeModel(TaintSource[Test])
      )
    |}
           ~expect:
             "`any_parameter.annotation.fully_qualified.matches` is not a valid constraint for \
              model queries with find clause of kind `attributes`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "attributes",
        where = AnyOf(
          cls.name.matches("foo"),
          return_annotation.fully_qualified.equals("int")
        ),
        model = AttributeModel(TaintSource[Test])
      )
    |}
           ~expect:
             "`return_annotation.fully_qualified.equals` is not a valid constraint for model \
              queries with find clause of kind `attributes`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "attributes",
        where = AnyOf(
          cls.name.matches("foo"),
          return_annotation.fully_qualified.matches("str")
        ),
        model = AttributeModel(TaintSource[Test])
      )
    |}
           ~expect:
             "`return_annotation.fully_qualified.matches` is not a valid constraint for model \
              queries with find clause of kind `attributes`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "attributes",
        where = Decorator(fully_qualified_name.matches("app.route")),
        model = AttributeModel(TaintSource[Test])
      )
    |}
           ~expect:
             "`Decorator` is not a valid constraint for model queries with find clause of kind \
              `attributes`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = cls.name.matches("foo"),
        model = Returns(TaintSource[Test])
      )
    |}
           ~expect:
             "`cls.name.matches` is not a valid constraint for model queries with find clause of \
              kind `functions`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
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
             "`type_annotation.equals` is not a valid constraint for model queries with find \
              clause of kind `functions`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
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
             "`type_annotation.equals` is not a valid constraint for model queries with find \
              clause of kind `methods`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
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
             "`type_annotation.matches` is not a valid constraint for model queries with find \
              clause of kind `functions`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
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
             "`type_annotation.matches` is not a valid constraint for model queries with find \
              clause of kind `methods`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
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
             "`type_annotation.is_annotated_type` is not a valid constraint for model queries with \
              find clause of kind `functions`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
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
             "`type_annotation.is_annotated_type` is not a valid constraint for model queries with \
              find clause of kind `methods`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:
             {|
      ModelQuery(
        name = "valid_model",
        find = "methods",
        where = name.equals("hello"),
        model = Modes([Entrypoint])
      )
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:
             {|
      ModelQuery(
        name = "valid_model",
        find = "methods",
        where = name.equals("hello"),
        model = Modes([Entrypoint, Obscure])
      )
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:
             {|
      ModelQuery(
        name = "valid_model",
        find = "functions",
        where = name.equals("hello"),
        model = Modes([Entrypoint])
      )
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = name.equals("hello"),
        model = Modes()
      )
    |}
           ~expect:"Unexpected model expression: `Modes()`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "globals",
        where = name.equals("hello"),
        model = Modes([Entrypoint])
      )
    |}
           ~expect:
             "`Modes` is not a valid model for model queries with find clause of kind `globals`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = annotation.equals(""),
        model = Modes([Entrypoint])
      )
    |}
           ~expect:"Unsupported callee for constraint: `annotation.equals`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = name.equals("hello"),
        model = Modes([ThisModeDoesntExist])
      )
    |}
           ~expect:"`ThisModeDoesntExist`: unknown mode";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = read_from_cache(),
        model = Returns(TaintSource[Test])
      )
    |}
           ~expect:
             "Invalid arguments for `read_from_cache` clause: expected named parameters `kind` and \
              `name` with string literal arguments, got `read_from_cache()`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = read_from_cache(kind=1),
        model = Returns(TaintSource[Test])
      )
    |}
           ~expect:
             "Invalid arguments for `read_from_cache` clause: expected named parameters `kind` and \
              `name` with string literal arguments, got `read_from_cache(kind = 1)`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = read_from_cache("foo", "bar"),
        model = Returns(TaintSource[Test])
      )
    |}
           ~expect:
             {|Invalid arguments for `read_from_cache` clause: expected named parameters `kind` and `name` with string literal arguments, got `read_from_cache("foo", "bar")`|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = read_from_cache(kind="thrift", name=f"{class_name}"),
        model = Returns(TaintSource[Test])
      )
    |}
           ~expect:
             {|Invalid arguments for `read_from_cache` clause: expected named parameters `kind` and `name` with string literal arguments, got `read_from_cache(kind = "thrift", name = f"{class_name}")`|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = Not(read_from_cache(kind="thrift", name="foo:bar")),
        model = Returns(TaintSource[Test])
      )
    |}
           ~expect:
             {|Invalid constraint: `read_from_cache` clause cannot be nested under `AnyOf` or `Not` clauses in `Not(read_from_cache(kind = "thrift", name = "foo:bar"))`|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = AnyOf(
          read_from_cache(kind="thrift", name="foo:bar"),
          name.matches("foo")
        ),
        model = Returns(TaintSource[Test])
      )
    |}
           ~expect:
             {|Invalid constraint: `read_from_cache` clause cannot be nested under `AnyOf` or `Not` clauses in `AnyOf(read_from_cache(kind = "thrift", name = "foo:bar"), name.matches("foo"))`|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = AnyOf(
          AllOf(read_from_cache(kind="thrift", name="foo:bar")),
          name.matches("foo")
        ),
        model = Returns(TaintSource[Test])
      )
    |}
           ~expect:
             {|Invalid constraint: `read_from_cache` clause cannot be nested under `AnyOf` or `Not` clauses in `AnyOf(AllOf(read_from_cache(kind = "thrift", name = "foo:bar")), name.matches("foo"))`|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = name.matches("foo"),
        model = WriteToCache()
      )
    |}
           ~expect:
             {|Invalid arguments for `WriteToCache` clause: expected a named parameter `kind` with a literal string argument, and a named parameter `name` with a format string argument, got `WriteToCache()`|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = name.matches("foo"),
        model = WriteToCache(1, 2)
      )
    |}
           ~expect:
             {|Invalid arguments for `WriteToCache` clause: expected a named parameter `kind` with a literal string argument, and a named parameter `name` with a format string argument, got `WriteToCache(1, 2)`|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = name.matches("foo"),
        model = WriteToCache(kind="thrift", name="foo")
      )
    |}
           ~expect:
             {|Invalid arguments for `WriteToCache` clause: expected a named parameter `kind` with a literal string argument, and a named parameter `name` with a format string argument, got `WriteToCache(kind = "thrift", name = "foo")`|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = name.matches("foo"),
        model = WriteToCache(kind="thrift", name=f"{func()}")
      )
    |}
           ~expect:
             {|Invalid argument for the parameter `name` of `WriteToCache`: unsupported expression `func()`|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = name.matches("foo"),
        model = WriteToCache(kind="thrift", name=f"{unknown}")
      )
    |}
           ~expect:
             {|Invalid argument for the parameter `name` of `WriteToCache`: unknown identifier `unknown`|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = name.matches("foo"),
        model = WriteToCache(kind="thrift", name=f"{function_name}")
      )
    |}
           ~expect:
             {|Invalid argument for the parameter `name` of `WriteToCache`: invalid identifier `function_name` for find="methods"|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = name.matches("foo"),
        model = WriteToCache(kind="thrift", name=f"{method_name}")
      )
    |}
           ~expect:
             {|Invalid argument for the parameter `name` of `WriteToCache`: invalid identifier `method_name` for find="functions"|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = name.matches("foo"),
        model = WriteToCache(kind="thrift", name=f"{class_name}")
      )
    |}
           ~expect:
             {|Invalid argument for the parameter `name` of `WriteToCache`: invalid identifier `class_name` for find="functions"|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = name.matches("foo"),
        model = WriteToCache(kind="thrift", name=f"{parameter_name}")
      )
    |}
           ~expect:
             {|Invalid argument for the parameter `name` of `WriteToCache`: identifier `parameter_name` is invalid in this context|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = name.matches("foo"),
        model = WriteToCache(kind="thrift", name=f"{parameter_position}")
      )
    |}
           ~expect:
             {|Invalid argument for the parameter `name` of `WriteToCache`: identifier `parameter_position` is invalid in this context|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = name.matches("foo"),
        model = WriteToCache(kind="thrift", name=f"{parameter_position * 2}")
      )
    |}
           ~expect:
             {|Invalid argument for the parameter `name` of `WriteToCache`: identifier `parameter_position` is invalid in this context|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = read_from_cache(kind="first", name="foo"),
        model = WriteToCache(kind="second", name=f"{function_name}")
      )
    |}
           ~expect:{|WriteToCache and read_from_cache cannot be used in the same model query|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = [
          name.matches("foo"),
        ],
        model = [
          WriteToCache(kind="second", name=f"{function_name}"),
          Returns(TaintSource[Test]),
        ]
      )
    |}
           ~expect:
             {|WriteToCache cannot be used with other taint annotations in the same model query|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = cls.extends("foo", is_transitive=foobar),
        model = ReturnModel(TaintSource[Test])
      )
    |}
           ~expect:
             "The Extends and AnyChild `is_transitive` attribute must be either True or False, \
              got: `foobar`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = cls.extends("foo", includes_self=foobar),
        model = ReturnModel(TaintSource[Test])
      )
    |}
           ~expect:
             "The Extends and AnyChild `includes_self` attribute must be either True or False, \
              got: `foobar`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = cls.extends("foo", foobar),
        model = ReturnModel(TaintSource[Test])
      )
    |}
           ~expect:"Unsupported arguments for `cls.extends`: `cls.extends(\"foo\", foobar)`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = cls.name.matches("foo", is_transitive=foobar),
        model = ReturnModel(TaintSource[Test])
      )
    |}
           ~expect:{|`cls.name.matches("foo", is_transitive = foobar)` is not a valid name clause.|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = cls.matches("foo"),
        model = ReturnModel(TaintSource[Test])
      )
    |}
           ~expect:
             {|Constraint `cls.matches` is deprecated, use `cls.fully_qualified_name.matches` instead.|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = name.foo("foo"),
        model = ReturnModel(TaintSource[Test])
      )
    |}
           ~expect:{|`name.foo("foo")` is not a valid name clause.|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = name.matches(foobar, 1, 2),
        model = ReturnModel(TaintSource[Test])
      )
    |}
           ~expect:"`name.matches(foobar, 1, 2)` is not a valid name clause.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = name.equals(foobar, 1, 2),
        model = ReturnModel(TaintSource[Test])
      )
    |}
           ~expect:"`name.equals(foobar, 1, 2)` is not a valid name clause.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
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
             {|Unsupported named parameter `fnid` in model query (expected: name, find, where, model, expected_models, unexpected_models).|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        find = "functions",
        where = name.matches("foo"),
        model = Returns(TaintSource[Test])
      )
    |}
           ~expect:{|Duplicate parameter `find` in model query.|};
      (* Test expected_models and unexpected_models clauses *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = name.matches("foo"),
        model = Returns(TaintSource[Test]),
        expected_models = [
          "def test.food() -> TaintSource[Test]: ..."
        ],
        expected_models = [
          "def test.food() -> TaintSource[Test]: ..."
        ]
      )
    |}
           ~expect:{|Duplicate parameter `expected_models` in model query.|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      def foo(x):
        ...
    |}
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = name.matches("foo"),
        model = Parameters(TaintSource[A]),
        expected_models = [
          """ModelQuery(
                name = "nested_model_query",
                find = "functions",
                where = [
                    name.matches("foo")
                ],
                model = [
                    Parameters(TaintSource[A])
                ]
            )"""
        ]
      )
    |}
           ~expect:
             {|In ModelQuery `invalid_model`: Model string `ModelQuery(
          name = "nested_model_query",
          find = "functions",
          where = [
              name.matches("foo")
          ],
          model = [
              Parameters(TaintSource[A])
          ]
      )` is a ModelQuery, not a model.
    Please make sure that the model string is a syntactically correct model.|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      def foo(x):
        ...
      def food(y):
        ...
    |}
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = name.matches("foo"),
        model = Parameters(TaintSource[A]),
        expected_models = [
          "foo(x)",
          "food(y)"
        ]
      )
    |}
           ~expect:
             {|Multiple errors:
[
Unexpected statement: `foo(x)`
Unexpected statement: `food(y)`
]|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      def foo(x):
        ...
      def bar(z):
        ...
    |}
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = name.matches("foo"),
        model = Parameters(TaintSource[A]),
        unexpected_models = [
          "def bar(z)"
        ]
      )
    |}
           ~expect:"Syntax error.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      def foo(x):
        ...
    |}
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = name.matches("foo"),
        model = Parameters(TaintSource[A]),
        expected_models = foo(x)
      )
    |}
           ~expect:
             "In ModelQuery `invalid_model`: Clause `foo(x)` is not a valid expected_models or \
              unexpected_models clause.\n\
             \   The clause should be a list of syntactically correct model strings.";
      (* Test cls.any_child clause in model queries *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source:{|
      @d("1")
      class A:
        def foo(x):
          ...
    |}
           ~model_source:
             {|
      ModelQuery(
        name = "valid_model",
        find = "methods",
        where = [
            cls.any_child(cls.decorator(arguments.contains("1"), name.matches("d")))
        ],
        model = Returns(TaintSource[A])
      )
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      def foo(x):
        ...
      def bar(z):
        ...
    |}
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = [
            cls.any_child(Decorator(arguments.contains("1"), name.matches("d")))
        ],
        model = Parameters(TaintSource[A])
      )
    |}
           ~expect:
             {|Unsupported callee for class constraint: `Decorator(arguments.contains("1"), name.matches("d"))`|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source:{|
      @d("1")
      class A:
        def foo(x):
          ...
    |}
           ~model_source:
             {|
      ModelQuery(
        name = "valid_model",
        find = "methods",
        where = [
            cls.any_child(
              AnyOf(
                cls.decorator(arguments.contains("1"), name.matches("d")),
                cls.name.matches("A")
              )
            )
        ],
        model = Returns(TaintSource[A])
      )
    |};
      (* Test cls.any_parent clause in model queries *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source:{|
      @d("1")
      class A:
        def foo(x):
          ...
    |}
           ~model_source:
             {|
      ModelQuery(
        name = "valid_model",
        find = "methods",
        where = [
            cls.any_parent(cls.decorator(arguments.contains("1"), name.matches("d")))
        ],
        model = Returns(TaintSource[A])
      )
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      def foo(x):
        ...
      def bar(z):
        ...
    |}
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = [
            cls.any_parent(Decorator(arguments.contains("1"), name.matches("d")))
        ],
        model = Parameters(TaintSource[A])
      )
    |}
           ~expect:
             {|Unsupported callee for class constraint: `Decorator(arguments.contains("1"), name.matches("d"))`|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source:{|
      @d("1")
      class A:
        def foo(x):
          ...
    |}
           ~model_source:
             {|
      ModelQuery(
        name = "valid_model",
        find = "methods",
        where = [
            cls.any_parent(
              AnyOf(
                cls.decorator(arguments.contains("1"), name.matches("d")),
                cls.name.matches("A")
              )
            )
        ],
        model = Returns(TaintSource[A])
      )
    |};
      (* Test Decorator clause in model queries *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
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
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = Decorator(foo),
        model = Returns(TaintSource[Test])
      )
    |}
           ~expect:"Unsupported decorator constraint expression: `foo`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = Decorator(name.matches(a, b)),
        model = Returns(TaintSource[Test])
      )
    |}
           ~expect:{|`name.matches(a, b)` is not a valid name clause.|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = Decorator(name.equals(a, b), arguments.equals(1, 2)),
        model = Returns(TaintSource[Test])
      )
    |}
           ~expect:{|`name.equals(a, b)` is not a valid name clause.|};
      (* Model queries *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "globals",
        where = name.matches("foo"),
        model = Returns(TaintSource[Test])
      )
    |}
           ~expect:
             "`Returns` is not a valid model for model queries with find clause of kind `globals`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "globals",
        where = name.matches("foo"),
        model = AttributeModel(TaintSource[X])
      )
    |}
           ~expect:
             "`AttributeModel` is not a valid model for model queries with find clause of kind \
              `globals`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "globals",
        where = type_annotation.fully_qualified.equals("int"),
        model = GlobalModel(TaintSource[A])
      )
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "globals",
        where = cls.any_child(Decorator(name.matches("d"))),
        model = GlobalModel(TaintSource[X])
      )
    |}
           ~expect:
             "`cls.any_child` is not a valid constraint for model queries with find clause of kind \
              `globals`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "globals",
        where = cls.any_parent(Decorator(name.matches("d"))),
        model = GlobalModel(TaintSource[X])
      )
    |}
           ~expect:
             "`cls.any_parent` is not a valid constraint for model queries with find clause of \
              kind `globals`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "globals",
        where = [AnyOf(
          name.matches("foo"),
          any_parameter.annotation.is_annotated_type(),
        )],
        model = GlobalModel(TaintSource[X])
      )
    |}
           ~expect:
             "`any_parameter.annotation.is_annotated_type` is not a valid constraint for model \
              queries with find clause of kind `globals`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:
             {|
      ModelQuery(
        name = "attributes",
        find = "attributes",
        where = type_annotation.is_annotated_type(),
        model = AttributeModel(ViaTypeOf)
      )
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "globals",
        where = type_annotation.is_annotated_type(),
        model = GlobalModel(TaintSource[A])
      )
    |}
           ~expect:
             "In `type_annotation.is_annotated_type`: `is_annotated_type` is deprecated for find \
              clause of kind `globals`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "globals",
        where = type_annotation.equals("int"),
        model = GlobalModel(TaintSource[A])
      )
    |}
           ~expect:
             "In `type_annotation.equals`: `.equals` is deprecated. Use `fully_qualified.equals` \
              or `original.matches` instead.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "attributes",
        where = type_annotation.equals("int"),
        model = GlobalModel(TaintSource[A])
      )
    |}
           ~expect:
             "In `type_annotation.equals`: `.equals` is deprecated. Use `fully_qualified.equals` \
              or `original.matches` instead.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "globals",
        where = type_annotation.original.matches("int"),
        model = GlobalModel(TaintSource[A])
      )
    |}
           ~expect:
             "In `type_annotation.original.matches`: `original.equals` or `original.matches` is \
              not supported for find clause of kind `globals`. Use `fully_qualified.equals` or \
              `fully_qualified.matches` instead.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = return_annotation.is_annotated_type(),
        model = Returns(TaintSource[Test])
      )
    |}
           ~expect:
             "In `return_annotation.is_annotated_type`: `is_annotated_type` is deprecated for find \
              clause of kind `functions`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = return_annotation.equals("int"),
        model = Returns(TaintSource[Test])
      )
    |}
           ~expect:
             "In `return_annotation.equals`: `.equals` is deprecated. Use `fully_qualified.equals` \
              or `original.matches` instead.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = return_annotation.original.equals("int"),
        model = Returns(TaintSource[Test])
      )
    |}
           ~expect:
             "In `return_annotation.original.equals`: `original.equals` or `original.matches` is \
              not supported for find clause of kind `functions`. Use `fully_qualified.equals` or \
              `fully_qualified.matches` instead.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = return_annotation.original.equals("int"),
        model = Returns(TaintSource[Test])
      )
    |}
           ~expect:
             "In `return_annotation.original.equals`: `original.equals` or `original.matches` is \
              not supported for find clause of kind `methods`. Use `fully_qualified.equals` or \
              `fully_qualified.matches` instead.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:
             {|
      ModelQuery(
        name = "attributes",
        find = "attributes",
        where = type_annotation.is_annotated_type(),
        model = AttributeModel(ViaAttributeName)
      )
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:
             {|
      ModelQuery(
        name = "attributes",
        find = "attributes",
        where = type_annotation.is_annotated_type(),
        model = AttributeModel(ViaAttributeName[WithTag["foo"]])
      )
    |};
      (* ParameterPath/ReturnPath/UpdatePath on model queries *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:
             {|
      ModelQuery(
        name = "query",
        find = "functions",
        where = name.matches("foo"),
        model = Returns(TaintSource[Test, ReturnPath[_.bar]])
      )
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:
             {|
      ModelQuery(
        name = "query",
        find = "functions",
        where = name.matches("foo"),
        model = Parameters(TaintSource[Test, ParameterPath[_.bar]])
      )
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:
             {|
      ModelQuery(
        name = "query",
        find = "functions",
        where = name.matches("foo"),
        model = Parameters(TaintSink[Test, ParameterPath[_.bar]])
      )
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~model_source:
             {|
      ModelQuery(
        name = "query",
        find = "functions",
        where = name.matches("foo"),
        model = Parameters(TaintInTaintOut[LocalReturn, ParameterPath[_.bar], ReturnPath[_.baz]])
      )
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "query",
        find = "functions",
        where = name.matches("foo"),
        model = Returns(TaintSource[Test, ParameterPath[_.bar]])
      )
    |}
           ~expect:
             "`TaintSource[(Test, ParameterPath[_.bar])]` is an invalid taint annotation: \
              `ParameterPath` can only be used on parameters";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "query",
        find = "functions",
        where = name.matches("foo"),
        model = Parameters(TaintSource[Test, ReturnPath[_.bar]])
      )
    |}
           ~expect:
             "`TaintSource[(Test, ReturnPath[_.bar])]` is an invalid taint annotation: \
              `ReturnPath[]` can only be used as a return annotation or within `TaintInTaintOut[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "query",
        find = "functions",
        where = name.matches("foo"),
        model = Returns(TaintSource[Test, UpdatePath[_.bar]])
      )
    |}
           ~expect:
             "`TaintSource[(Test, UpdatePath[_.bar])]` is an invalid taint annotation: \
              `UpdatePath` can only be used within `TaintInTaintOut[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "query",
        find = "functions",
        where = name.matches("foo"),
        model = Returns(TaintInTaintOut[LocalReturn])
      )
    |}
           ~expect:
             "`TaintInTaintOut[LocalReturn]` is an invalid taint annotation: `TaintInTaintOut[]` \
              can only be used on parameters, attributes or globals";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "query",
        find = "functions",
        where = name.matches("foo"),
        model = Parameters(TaintInTaintOut[LocalReturn, ParameterPath[_.all_static_fields()]])
      )
    |}
           ~expect:
             "`TaintInTaintOut[(LocalReturn, ParameterPath[_.all_static_fields()])]` is an invalid \
              taint annotation: `all_static_fields()` is not allowed within `TaintInTaintOut[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "query",
        find = "functions",
        where = name.matches("foo"),
        model = Parameters(TaintInTaintOut[LocalReturn, ReturnPath[_.all_static_fields()]])
      )
    |}
           ~expect:
             "`TaintInTaintOut[(LocalReturn, ReturnPath[_.all_static_fields()])]` is an invalid \
              taint annotation: `all_static_fields()` is not allowed within `TaintInTaintOut[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "query",
        find = "functions",
        where = name.matches("foo"),
        model = Parameters(TaintSource[Test, ParameterPath[_.parameter_name()]])
      )
    |}
           ~expect:
             "`TaintSource[(Test, ParameterPath[_.parameter_name()])]` is an invalid taint \
              annotation: `parameter_name()` can only be used within `TaintInTaintOut[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "query",
        find = "functions",
        where = name.matches("foo"),
        model = Parameters(TaintSink[Test, ParameterPath[_.parameter_name()]])
      )
    |}
           ~expect:
             "`TaintSink[(Test, ParameterPath[_.parameter_name()])]` is an invalid taint \
              annotation: `parameter_name()` can only be used within `TaintInTaintOut[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "query",
        find = "functions",
        where = name.matches("foo"),
        model = Parameters(AppliesTo["bar", TaintSink[Test, ParameterPath[_.foo]]])
      )
    |}
           ~expect:
             "`AppliesTo[(\"bar\", TaintSink[(Test, ParameterPath[_.foo])])]` is an invalid taint \
              annotation: `AppliesTo[]` cannot be used with `ParameterPath[]`";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
        name = "query",
        find = "attributes",
        where = name.matches("foo"),
        model = AttributeModel(TaintInTaintOut[LocalReturn, ParameterPath[_.foo]])
      )
    |}
           ~expect:
             "`TaintInTaintOut[(LocalReturn, ParameterPath[_.foo])]` is an invalid taint \
              annotation: `ParameterPath` can only be used on parameters";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
         name = "foo_finders",
         find = "functions",
         where = any_parameter.annotation.fully_qualified.equals("foo"),
         model = [
           AllParameters(
             ParametricSourceFromAnnotation(pattern=DynamicSource, kind=Dynamic)
           )
         ]
      )
    |}
           ~expect:"Parametric taint annotation `ParametricSourceFromAnnotation` is deprecated.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             {|
      ModelQuery(
         name = "foo_finders",
         find = "functions",
         where = any_parameter.annotation.fully_qualified.equals("foo"),
         model = [
           AllParameters(
             ParametricSinkFromAnnotation(pattern=DynamicSink, kind=Dynamic)
           )
         ]
      )
    |}
           ~expect:"Parametric taint annotation `ParametricSinkFromAnnotation` is deprecated.";
    ]


let test_invalid_decorators =
  let assert_valid_model ?path ?source ?sources ~model_source =
    assert_invalid_model ?path ?source ?sources ~model_source ~expect:"no failure"
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~path:"a.py"
           ~model_source:{|
      @Sanitize
      def a.not_in_environment(self): ...
    |}
           ~expect:
             "a.py:2: `a.not_in_environment` is not part of the environment, no module `a` in \
              search path.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~path:"a.py"
           ~model_source:
             {|
      @Sanitize(TaintSink)
      @Sanitize(TaintSource)
      def a.not_in_environment(self): ...
    |}
           ~expect:
             "a.py:2: `a.not_in_environment` is not part of the environment, no module `a` in \
              search path.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~path:"a.py"
           ~model_source:{|
      class a.Foo(SkipOverrides): ...
    |}
           ~expect:"a.py:2: `a.Foo` is not part of the environment, no module `a` in search path.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:
             {|
      class C:
          def __init__(self, x) -> None:
              ...
    |}
           ~model_source:{|
      @Sanitize
      def test.C(): ...
    |}
           ~expect:
             "The class `test.C` is not a valid define - did you mean to model `test.C.__init__()`?";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      class C:
        pass
    |}
           ~model_source:{|
      test.C: Sanitize
    |}
           ~expect:
             "The class `test.C` is not a valid attribute - did you mean to model \
              `test.C.__init__()`?";
      (* Class models don't error. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source:
             {|
      class C:
          def __init__(self, x) -> None:
              ...
    |}
           ~model_source:{|
      @Sanitize
      class test.C: ...
    |};
      (* Decorators. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
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
             (* TODO(T225700656): Support models for functions with unknown decorators. *)
           ~skip_for_pyrefly:true;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
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
             "The function, method or property `test.Foo.bar` is not a valid attribute - did you \
              mean to use `def test.Foo.bar(): ...`?"
           ~skip_for_pyrefly:true;
    ]


let test_invalid_callables =
  let assert_valid_model ?path ?skip_for_pyrefly ?source ?sources ~model_source =
    assert_invalid_model ?path ?skip_for_pyrefly ?source ?sources ~model_source ~expect:"no failure"
  in
  test_list
    [
      (* Error on non-existing callables. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def not_in_the_environment(parameter: InvalidTaintDirection[Test]): ..."
           ~expect:
             "`not_in_the_environment` is not part of the environment, no module \
              `not_in_the_environment` in search path.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:
             "def not_in_the_environment.derp(parameter: InvalidTaintDirection[Test]): ..."
           ~expect:
             "`not_in_the_environment.derp` is not part of the environment, no module \
              `not_in_the_environment` in search path.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"def test(parameter: InvalidTaintDirection[Test]): ..."
           ~expect:"The module `test` is not a valid define.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~model_source:"test: Sanitize"
           ~expect:"The module `test` is not a valid attribute.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      class Foo:
        x: int = 1
    |}
           ~model_source:{|
      def test.Foo.x(self) -> TaintSource[Test]: ...
    |}
           ~expect:
             "The attribute `test.Foo.x` is not a valid define - did you mean to use `test.Foo.x: \
              ...`?";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      def foo():
        return
    |}
           ~model_source:{|
      test.foo: TaintSource[Test]
    |}
           ~expect:
             "The function, method or property `test.foo` is not a valid attribute - did you mean \
              to use `def test.foo(): ...`?";
      (* Accept callable models for Any or Top because of type error. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
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
           ~skip_for_pyrefly:true;
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
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
           ~skip_for_pyrefly:true;
    ]


let test_invalid_overloads =
  let assert_valid_model ?path ?source ?sources ~model_source =
    assert_invalid_model ?path ?source ?sources ~model_source ~expect:"no failure"
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source_path:"test.pyi"
           ~source:
             {|
            from typing import overload
            class Foo:
              @overload
              def bar(self, x: int) -> str: ...
              @overload
              def bar(self, x: str) -> int: ...
          |}
           ~model_source:{|
      def test.Foo.bar(self, x: TaintSink[Test]): ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source_path:"test.pyi"
           ~source:
             {|
            from typing import overload
            class Foo:
              @overload
              def bar(self, x: int) -> str: ...
              @overload
              def bar(self, x: str) -> int: ...
          |}
           ~model_source:{|
      test.Foo.bar: TaintSink[Test]
    |}
           ~expect:
             "The function, method or property `test.Foo.bar` is not a valid attribute - did you \
              mean to use `def test.Foo.bar(): ...`?";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
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
           ~pyrefly_expect:"Module `test` does not define `test.Child.foo`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:
             {|
      class Parent:
        @property
        def foo(self) -> int: ...
      class Child(Parent):
        pass
    |}
           ~model_source:
             {|
      @property
      def test.Child.foo(self) -> TaintSource[Test]: ...
    |}
           ~expect:"Module `test` does not define `test.Child.foo`.";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      class C:
        x = ...
      |}
           ~model_source:
             {|
      test.C.x: Sanitize[TaintInTaintOut[TaintSource[Test]]] = ...
    |}
           ~expect:
             "`Sanitize[TaintInTaintOut[TaintSource[Test]]]` is an invalid taint annotation: \
              TaintInTaintOut sanitizers cannot be modelled on attributes";
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      class C:
        x = ...
      |}
           ~model_source:{|
      test.C.x: Sanitize[TaintInTaintOut[TaintSink[Test]]] = ...
    |}
           ~expect:
             "`Sanitize[TaintInTaintOut[TaintSink[Test]]]` is an invalid taint annotation: \
              TaintInTaintOut sanitizers cannot be modelled on attributes";
    ]


let test_invalid_via =
  let assert_valid_model ?path ?source ?sources ~model_source =
    assert_invalid_model ?path ?source ?sources ~model_source ~expect:"no failure"
  in
  test_list
    [
      (* ViaTypeOf on attributes *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      def foo(x: int) -> int: ...
      |}
           ~model_source:{|
      def test.foo(x: ViaTypeOf): ...
    |}
           ~expect:
             {|`ViaTypeOf` is an invalid taint annotation: A standalone `ViaTypeOf` without arguments can only be used in attribute or global models|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      def foo(x: int) -> int: ...
      |}
           ~model_source:{|
      def test.foo(x) -> ViaTypeOf: ...
    |}
           ~expect:
             {|`ViaTypeOf` is an invalid taint annotation: A standalone `ViaTypeOf` without arguments can only be used in attribute or global models|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      class C:
        def foo(x: int) -> int: ...
      |}
           ~model_source:{|
      def test.C.foo(x: ViaTypeOf): ...
    |}
           ~expect:
             {|`ViaTypeOf` is an invalid taint annotation: A standalone `ViaTypeOf` without arguments can only be used in attribute or global models|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      class C:
        def foo(x: int) -> int: ...
      |}
           ~model_source:{|
      def test.C.foo() -> ViaTypeOf: ...
    |}
           ~expect:
             {|`ViaTypeOf` is an invalid taint annotation: A standalone `ViaTypeOf` without arguments can only be used in attribute or global models|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source:{|
      class C:
        x: int = 0
      |}
           ~model_source:{|
      test.C.x: ViaTypeOf = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source:{|
      class C:
        x: int = 0
      |}
           ~model_source:{|
      test.C.x: TaintInTaintOut[ViaTypeOf] = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source:{|
      class C:
        x: int = 0
      |}
           ~model_source:{|
      test.C.x: ViaTypeOf[WithTag["tag"]] = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source:{|
      class C:
        x: int = 0
      |}
           ~model_source:{|
      test.C.x: TaintSource[A, ViaTypeOf] = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source:{|
      class C:
        x: int = 0
      |}
           ~model_source:{|
      test.C.x: TaintSink[X, ViaTypeOf] = ...
    |};
      (* ViaAttributeName *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      def foo(x: int) -> int: ...
      |}
           ~model_source:{|
      def test.foo(x: ViaAttributeName): ...
    |}
           ~expect:
             {|`ViaAttributeName` is an invalid taint annotation: `ViaAttributeName` can only be used in attribute or global models.|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_invalid_model
           ~source:{|
      class C:
        def foo(x: int) -> int: ...
      |}
           ~model_source:{|
      def test.C.foo() -> ViaAttributeName: ...
    |}
           ~expect:
             {|`ViaAttributeName` is an invalid taint annotation: `ViaAttributeName` can only be used in attribute or global models.|};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source:{|
      class C:
        x: int = 0
      |}
           ~model_source:{|
      test.C.x: ViaAttributeName = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source:{|
      class C:
        x: int = 0
      |}
           ~model_source:{|
      test.C.x: TaintInTaintOut[ViaAttributeName] = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source:{|
      class C:
        x: int = 0
      |}
           ~model_source:{|
      test.C.x: ViaAttributeName[WithTag["tag"]] = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source:{|
      class C:
        x: int = 0
      |}
           ~model_source:{|
      test.C.x: TaintSource[A, ViaAttributeName] = ...
    |};
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_valid_model
           ~source:{|
      class C:
        x: int = 0
      |}
           ~model_source:{|
      test.C.x: TaintSink[X, ViaAttributeName] = ...
    |};
    ]


let () =
  "taint_model"
  >::: [
         test_invalid_models;
         test_invalid_model_queries;
         test_invalid_decorators;
         test_invalid_callables;
         test_invalid_overloads;
         test_invalid_via;
       ]
  |> Test.run
