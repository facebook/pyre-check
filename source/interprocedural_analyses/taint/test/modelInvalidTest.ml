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
  let global_resolution =
    ScratchProject.setup ~context sources |> ScratchProject.build_global_resolution
  in
  let taint_configuration =
    TaintConfiguration.Heap.
      {
        empty with
        sources =
          List.map
            ~f:(fun name -> { AnnotationParser.name; kind = Named; location = None })
            ["A"; "B"; "Test"];
        sinks =
          List.map
            ~f:(fun name -> { AnnotationParser.name; kind = Named; location = None })
            ["X"; "Y"; "Test"];
        features = ["featureA"; "featureB"];
        rules = [];
        partial_sink_labels =
          TaintConfiguration.PartialSinkLabelsMap.of_alist_exn
            ["Test", { TaintConfiguration.PartialSinkLabelsMap.main = "a"; secondary = "b" }];
      }
  in
  let error_message =
    let path = path >>| PyrePath.create_absolute in
    ModelVerifier.ClassDefinitionsCache.invalidate ();
    ModelParser.parse
      ~resolution:global_resolution
      ~taint_configuration
      ~source_sink_filter:None
      ?path
      ~source:(Test.trim_extra_indentation model_source)
      ~definitions:None
      ~stubs:
        ([]
        |> Interprocedural.Target.HashsetSharedMemory.from_heap
        |> Interprocedural.Target.HashsetSharedMemory.read_only)
      ~python_version:ModelParser.PythonVersion.default
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


let test_invalid_models context =
  let assert_invalid_model ?path ?source ?sources ~model_source ~expect () =
    assert_invalid_model ?path ?source ?sources ~context ~model_source ~expect ()
  in
  let assert_valid_model ?path ?source ?sources ~model_source () =
    assert_invalid_model ?path ?source ?sources ~model_source ~expect:"no failure" ()
  in
  assert_invalid_model ~model_source:"1 + import" ~expect:"Syntax error." ();
  assert_invalid_model ~model_source:"import foo" ~expect:"Unexpected statement: `import foo`" ();
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
  assert_invalid_model
    ~source:"def f(x: int): ..."
    ~model_source:"def test.f(x) -> TaintSource[WithSubkind[X, Y, Z]]: ..."
    ~expect:
      "`TaintSource[WithSubkind[(X, Y, Z)]]` is an invalid taint annotation: Invalid expression \
       for taint subkind: (X, Y, Z)"
    ();
  assert_invalid_model
    ~source:"def f(x: int): ..."
    ~model_source:"def test.f(x) -> TaintSource[Collapse[Subkind]]: ..."
    ~expect:
      "`TaintSource[Collapse[Subkind]]` is an invalid taint annotation: Unsupported taint source \
       `Collapse`"
    ();
  assert_invalid_model
    ~source:"def f(x: int): ..."
    ~model_source:"def test.f(x) -> TaintSource[Collapse]: ..."
    ~expect:
      "`TaintSource[Collapse]` is an invalid taint annotation: `Collapse` can only be used within \
       `TaintInTaintOut[]`"
    ();
  assert_invalid_model
    ~source:"def f(x: int): ..."
    ~model_source:"def test.f(x) -> TaintSource[NoCollapse]: ..."
    ~expect:
      "`TaintSource[NoCollapse]` is an invalid taint annotation: `NoCollapse` can only be used \
       within `TaintInTaintOut[]`"
    ();
  assert_invalid_model
    ~source:"def f(x: int): ..."
    ~model_source:"def test.f(x) -> TaintSource[WithSubkind[A][B]]: ..."
    ~expect:
      "`TaintSource[WithSubkind[A][B]]` is an invalid taint annotation: Invalid expression for \
       taint kind: WithSubkind[A][B]"
    ();
  assert_invalid_model
    ~source:"def f(x: int): ..."
    ~model_source:"def test.f(x) -> TaintSource[Test.attribute]: ..."
    ~expect:
      "`TaintSource[Test.attribute]` is an invalid taint annotation: Invalid expression for taint \
       kind: Test.attribute"
    ();
  assert_invalid_model
    ~source:"def f(x: int): ..."
    ~model_source:"def test.f(x) -> TaintSource[ViaAttributeName]: ..."
    ~expect:
      "`TaintSource[ViaAttributeName]` is an invalid taint annotation: `ViaAttributeName` can only \
       be used in attribute or global models."
    ();
  assert_invalid_model
    ~source:"def f(x: int): ..."
    ~model_source:{|def test.f(x) -> TaintSource[ViaAttributeName[WithTag["tag"]]]: ...|}
    ~expect:
      "`TaintSource[ViaAttributeName[WithTag[\"tag\"]]]` is an invalid taint annotation: \
       `ViaAttributeName` can only be used in attribute or global models."
    ();
  assert_invalid_model
    ~source:"def f(x: int): ..."
    ~model_source:"def test.f(x) -> TaintSink[ViaAttributeName]: ..."
    ~expect:
      "`TaintSink[ViaAttributeName]` is an invalid taint annotation: `ViaAttributeName` can only \
       be used in attribute or global models."
    ();
  assert_invalid_model
    ~source:"def f(x: int): ..."
    ~model_source:{|def test.f(x) -> TaintSink[ViaAttributeName[WithTag["tag"]]]: ...|}
    ~expect:
      "`TaintSink[ViaAttributeName[WithTag[\"tag\"]]]` is an invalid taint annotation: \
       `ViaAttributeName` can only be used in attribute or global models."
    ();
  assert_invalid_model
    ~source:{|
      class C:
        x: int = 0
      |}
    ~model_source:"test.C.x: ViaTypeOf[a.b] = ..."
    ~expect:
      "`ViaTypeOf[a.b]` is an invalid taint annotation: Invalid expression in `ViaTypeOf` \
       declaration: a.b"
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
       str]: ...`. Reason: invalid position 0 for named parameter `c` (valid options are \
       {formal(c, position=1), formal(c, position=2)})."
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
    ~expect:
      "`TaintSink[Updates[self]]` is an invalid taint annotation: `Updates` can only be used \
       within `TaintInTaintOut[]`"
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
  assert_invalid_model
    ~model_source:"def test.source() -> AttachToSource[Via[featureA], ReturnPath[_]]: ..."
    ~expect:
      "`AttachToSource[(Via[featureA], ReturnPath[_])]` is an invalid taint annotation: All \
       parameters to `AttachToSource` must be of the form `Via[feature]`."
    ();
  assert_invalid_model
    ~model_source:"def test.sink(parameter: AttachToSink[Via[featureA], ParameterPath[_]]): ..."
    ~expect:
      "`AttachToSink[(Via[featureA], ParameterPath[_])]` is an invalid taint annotation: All \
       parameters to `AttachToSink` must be of the form `Via[feature]`."
    ();
  assert_invalid_model
    ~model_source:"def test.sink(parameter: AttachToTito[Via[featureA], ParameterPath[_]]): ..."
    ~expect:
      "`AttachToTito[(Via[featureA], ParameterPath[_])]` is an invalid taint annotation: All \
       parameters to `AttachToTito` must be of the form `Via[feature]`."
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

  (* Input and output path specification. *)
  assert_invalid_model
    ~model_source:"def test.sink(parameter: TaintSink[Test, ParameterPath[test]]): ..."
    ~expect:"`test` is an invalid access path: access path must start with `_`"
    ();
  assert_invalid_model
    ~model_source:"def test.sink(parameter: TaintSink[Test, ParameterPath[_.unknown()]]): ..."
    ~expect:
      "`_.unknown()` is an invalid access path: unexpected method call `unknown` (allowed: `keys`, \
       `all`, `all_static_fields`, `parameter_name`)"
    ();
  assert_invalid_model
    ~model_source:"def test.sink(parameter: TaintSink[Test, ReturnPath[_[0]]]): ..."
    ~expect:
      "`TaintSink[(Test, ReturnPath[_[0]])]` is an invalid taint annotation: `ReturnPath[]` can \
       only be used as a return annotation or within `TaintInTaintOut[]`"
    ();
  assert_invalid_model
    ~model_source:"def test.sink(parameter: TaintSink[Test, UpdatePath[_[0]]]): ..."
    ~expect:
      "`TaintSink[(Test, UpdatePath[_[0]])]` is an invalid taint annotation: `UpdatePath` can only \
       be used within `TaintInTaintOut[]`"
    ();
  assert_invalid_model
    ~model_source:
      "def test.sink(parameter: AppliesTo[0, TaintSink[Test, ParameterPath[_[1]]]]): ..."
    ~expect:
      "`AppliesTo[(0, TaintSink[(Test, ParameterPath[_[1]])])]` is an invalid taint annotation: \
       `AppliesTo[]` cannot be used with `ParameterPath[]`"
    ();
  assert_invalid_model
    ~model_source:"def test.sink(parameter: AppliesTo[1, TaintSink[Test, UpdatePath[_[0]]]]): ..."
    ~expect:
      "`AppliesTo[(1, TaintSink[(Test, UpdatePath[_[0]])])]` is an invalid taint annotation: \
       `UpdatePath` can only be used within `TaintInTaintOut[]`"
    ();
  assert_invalid_model
    ~model_source:"def test.taint(y: TaintSource[Test, ReturnPath[_[0]]]): ..."
    ~expect:
      "`TaintSource[(Test, ReturnPath[_[0]])]` is an invalid taint annotation: `ReturnPath[]` can \
       only be used as a return annotation or within `TaintInTaintOut[]`"
    ();
  assert_invalid_model
    ~model_source:"def test.taint(y: TaintSource[Test, UpdatePath[_[0]]]): ..."
    ~expect:
      "`TaintSource[(Test, UpdatePath[_[0]])]` is an invalid taint annotation: `UpdatePath` can \
       only be used within `TaintInTaintOut[]`"
    ();
  assert_invalid_model
    ~model_source:"def test.taint(y: AppliesTo[0, TaintSource[Test, ParameterPath[_[1]]]]): ..."
    ~expect:
      "`AppliesTo[(0, TaintSource[(Test, ParameterPath[_[1]])])]` is an invalid taint annotation: \
       `AppliesTo[]` cannot be used with `ParameterPath[]`"
    ();
  assert_invalid_model
    ~model_source:"def test.taint(y: AppliesTo[1, TaintSource[Test, UpdatePath[_[0]]]]): ..."
    ~expect:
      "`AppliesTo[(1, TaintSource[(Test, UpdatePath[_[0]])])]` is an invalid taint annotation: \
       `UpdatePath` can only be used within `TaintInTaintOut[]`"
    ();
  assert_invalid_model
    ~model_source:"def test.sink() -> TaintSink[Test, ParameterPath[_[0]]]: ..."
    ~expect:
      "`TaintSink[(Test, ParameterPath[_[0]])]` is an invalid taint annotation: `ParameterPath` \
       can only be used on parameters"
    ();
  assert_invalid_model
    ~model_source:"def test.sink() -> TaintSink[Test, UpdatePath[_[0]]]: ..."
    ~expect:
      "`TaintSink[(Test, UpdatePath[_[0]])]` is an invalid taint annotation: `UpdatePath` can only \
       be used within `TaintInTaintOut[]`"
    ();
  assert_invalid_model
    ~model_source:"def test.sink() -> AppliesTo[1, TaintSink[Test, ReturnPath[_[0]]]]: ..."
    ~expect:
      "`AppliesTo[(1, TaintSink[(Test, ReturnPath[_[0]])])]` is an invalid taint annotation: \
       `AppliesTo[]` cannot be used with `ReturnPath[]`"
    ();
  assert_invalid_model
    ~model_source:"def test.taint() -> TaintSource[Test, ParameterPath[_[0]]]: ..."
    ~expect:
      "`TaintSource[(Test, ParameterPath[_[0]])]` is an invalid taint annotation: `ParameterPath` \
       can only be used on parameters"
    ();
  assert_invalid_model
    ~model_source:"def test.taint() -> TaintSource[Test, UpdatePath[_[0]]]: ..."
    ~expect:
      "`TaintSource[(Test, UpdatePath[_[0]])]` is an invalid taint annotation: `UpdatePath` can \
       only be used within `TaintInTaintOut[]`"
    ();
  assert_invalid_model
    ~model_source:"def test.taint() -> AppliesTo[1, TaintSource[Test, ReturnPath[_[0]]]]: ..."
    ~expect:
      "`AppliesTo[(1, TaintSource[(Test, ReturnPath[_[0]])])]` is an invalid taint annotation: \
       `AppliesTo[]` cannot be used with `ReturnPath[]`"
    ();
  assert_invalid_model
    ~model_source:"def test.taint(x: TaintInTaintOut[UpdatePath[_[0]]]): ..."
    ~expect:
      "Invalid model for `test.taint`: Invalid UpdatePath annotation for TaintInTaintOut annotation"
    ();
  assert_invalid_model
    ~model_source:"def test.taint(x: AppliesTo[0, TaintInTaintOut[ParameterPath[_[0]]]]): ..."
    ~expect:
      "`AppliesTo[(0, TaintInTaintOut[ParameterPath[_[0]]])]` is an invalid taint annotation: \
       `AppliesTo[]` cannot be used with `ParameterPath[]`"
    ();
  assert_invalid_model
    ~model_source:"def test.taint(x, y: TaintInTaintOut[Updates[x], ReturnPath[_[0]]]): ..."
    ~expect:"Invalid model for `test.taint`: Invalid ReturnPath annotation for Updates annotation"
    ();
  assert_invalid_model
    ~model_source:"def test.taint(x: TaintInTaintOut[UpdatePath[_.all_static_fields()]]): ..."
    ~expect:
      "`TaintInTaintOut[UpdatePath[_.all_static_fields()]]` is an invalid taint annotation: \
       `all_static_fields()` is not allowed within `TaintInTaintOut[]`"
    ();
  assert_invalid_model
    ~model_source:"def test.taint(x: TaintInTaintOut[ParameterPath[_.all_static_fields()]]): ..."
    ~expect:
      "`TaintInTaintOut[ParameterPath[_.all_static_fields()]]` is an invalid taint annotation: \
       `all_static_fields()` is not allowed within `TaintInTaintOut[]`"
    ();
  assert_invalid_model
    ~model_source:"def test.taint(x: TaintInTaintOut[ReturnPath[_.all_static_fields()]]): ..."
    ~expect:
      "`TaintInTaintOut[ReturnPath[_.all_static_fields()]]` is an invalid taint annotation: \
       `all_static_fields()` is not allowed within `TaintInTaintOut[]`"
    ();

  (* Collapse depth specification. *)
  assert_invalid_model
    ~model_source:"def test.taint(x: TaintInTaintOut[CollapseDepth[a]]): ..."
    ~expect:
      "`TaintInTaintOut[CollapseDepth[a]]` is an invalid taint annotation: expected non-negative \
       int literal argument for CollapseDepth, got `a`"
    ();
  assert_invalid_model
    ~model_source:"def test.taint(x: TaintInTaintOut[CollapseDepth[-1]]): ..."
    ~expect:
      "`TaintInTaintOut[CollapseDepth[-1]]` is an invalid taint annotation: expected non-negative \
       int literal argument for CollapseDepth, got `-1`"
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
       expression for breadcrumb: a.__sub__(feature)"
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
      {|`Sanitize(TaintSource[(A, Via[featureA])])` is an invalid taint annotation: `TaintSource[A, Via[featureA]]` is not supported within `Sanitize[...]`|}
    ();
  (* TODO(T179023946): Allow inner function models *)
  assert_invalid_model
    ~source:{|
      def outer():
        def inner():
          return 1
    |}
    ~model_source:"def test.outer.inner() -> TaintSource[Test]: ..."
    ~expect:"Module `test` does not define `test.outer.inner`."
    ();
  ()


let test_invalid_model_queries context =
  let assert_invalid_model ?path ?source ?sources ~model_source ~expect () =
    assert_invalid_model ?path ?source ?sources ~context ~model_source ~expect ()
  in

  let assert_valid_model ?path ?source ?sources ~model_source () =
    assert_invalid_model ?path ?source ?sources ~model_source ~expect:"no failure" ()
  in
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        find = "functions",
        where = name.matches("foo"),
        model = Returns(TaintSource[Test])
      )
    |}
    ~expect:{|Missing required parameter `name` in model query.|}
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
    ~expect:{|Missing required parameter `find` in model query.|}
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
    ~expect:{|Missing required parameter `where` in model query.|}
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
    ~expect:{|Missing required parameter `model` in model query.|}
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
          cls.name.matches("foo"),
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
          cls.name.matches("foo"),
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
          cls.name.matches("foo"),
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
          cls.name.matches("foo"),
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
          cls.name.matches("foo"),
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
        where = Decorator(fully_qualified_name.matches("app.route")),
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
        where = cls.name.matches("foo"),
        model = Returns(TaintSource[Test])
      )
    |}
    ~expect:
      "`cls.name.matches` is not a valid constraint for model queries with find clause of kind \
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
        name = "valid_model",
        find = "methods",
        where = name.equals("hello"),
        model = Modes([Entrypoint])
      )
    |}
    ~expect:"no failure"
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "valid_model",
        find = "methods",
        where = name.equals("hello"),
        model = Modes([Entrypoint, Obscure])
      )
    |}
    ~expect:"no failure"
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "valid_model",
        find = "functions",
        where = name.equals("hello"),
        model = Modes([Entrypoint])
      )
    |}
    ~expect:"no failure"
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = name.equals("hello"),
        model = Modes()
      )
    |}
    ~expect:"Unexpected model expression: `Modes()`"
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "globals",
        where = name.equals("hello"),
        model = Modes([Entrypoint])
      )
    |}
    ~expect:"`Modes` is not a valid model for model queries with find clause of kind `globals`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = annotation.equals(""),
        model = Modes([Entrypoint])
      )
    |}
    ~expect:"Unsupported callee for constraint: `annotation.equals`"
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = name.equals("hello"),
        model = Modes([ThisModeDoesntExist])
      )
    |}
    ~expect:"`ThisModeDoesntExist`: unknown mode"
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = name.equals("hello"),
        model = Modes([SkipDecoratorWhenInlining])
      )
    |}
    ~expect:"`SkipDecoratorWhenInlining`: mode cannot be used in a model query"
    ();
  assert_invalid_model
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
      "Invalid arguments for `read_from_cache` clause: expected named parameters `kind` and `name` \
       with string literal arguments, got `read_from_cache()`"
    ();
  assert_invalid_model
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
      "Invalid arguments for `read_from_cache` clause: expected named parameters `kind` and `name` \
       with string literal arguments, got `read_from_cache(kind = 1)`"
    ();
  assert_invalid_model
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
      {|Invalid arguments for `read_from_cache` clause: expected named parameters `kind` and `name` with string literal arguments, got `read_from_cache("foo", "bar")`|}
    ();
  assert_invalid_model
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
      {|Invalid arguments for `read_from_cache` clause: expected named parameters `kind` and `name` with string literal arguments, got `read_from_cache(kind = "thrift", name = f"{class_name}")`|}
    ();
  assert_invalid_model
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
      {|Invalid constraint: `read_from_cache` clause cannot be nested under `AnyOf` or `Not` clauses in `Not(read_from_cache(kind = "thrift", name = "foo:bar"))`|}
    ();
  assert_invalid_model
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
      {|Invalid constraint: `read_from_cache` clause cannot be nested under `AnyOf` or `Not` clauses in `AnyOf(read_from_cache(kind = "thrift", name = "foo:bar"), name.matches("foo"))`|}
    ();
  assert_invalid_model
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
      {|Invalid constraint: `read_from_cache` clause cannot be nested under `AnyOf` or `Not` clauses in `AnyOf(AllOf(read_from_cache(kind = "thrift", name = "foo:bar")), name.matches("foo"))`|}
    ();
  assert_invalid_model
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
      {|Invalid arguments for `WriteToCache` clause: expected a named parameter `kind` with a literal string argument, and a named parameter `name` with a format string argument, got `WriteToCache()`|}
    ();
  assert_invalid_model
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
      {|Invalid arguments for `WriteToCache` clause: expected a named parameter `kind` with a literal string argument, and a named parameter `name` with a format string argument, got `WriteToCache(1, 2)`|}
    ();
  assert_invalid_model
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
      {|Invalid arguments for `WriteToCache` clause: expected a named parameter `kind` with a literal string argument, and a named parameter `name` with a format string argument, got `WriteToCache(kind = "thrift", name = "foo")`|}
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = name.matches("foo"),
        model = WriteToCache(kind="thrift", name=f"{1+2}")
      )
    |}
    ~expect:
      {|Invalid argument for the parameter `name` of `WriteToCache`: expected identifier, got `1.__add__(2)`|}
    ();
  assert_invalid_model
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
      {|Invalid argument for the parameter `name` of `WriteToCache`: unknown identifier `unknown`|}
    ();
  assert_invalid_model
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
      {|Invalid identifier `function_name` for parameter `name` of `WriteToCache` for find="methods"|}
    ();
  assert_invalid_model
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
      {|Invalid identifier `method_name` for parameter `name` of `WriteToCache` for find="functions"|}
    ();
  assert_invalid_model
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
      {|Invalid identifier `class_name` for parameter `name` of `WriteToCache` for find="functions"|}
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "functions",
        where = read_from_cache(kind="first", name="foo"),
        model = WriteToCache(kind="second", name=f"{function_name}")
      )
    |}
    ~expect:{|WriteToCache and read_from_cache cannot be used in the same model query|}
    ();
  assert_invalid_model
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
    ~expect:{|WriteToCache cannot be used with other taint annotations in the same model query|}
    ();
  assert_invalid_model
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
      "The Extends and AnyChild `is_transitive` attribute must be either True or False, got: \
       `foobar`."
    ();
  assert_invalid_model
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
      "The Extends and AnyChild `includes_self` attribute must be either True or False, got: \
       `foobar`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = cls.extends("foo", foobar),
        model = ReturnModel(TaintSource[Test])
      )
    |}
    ~expect:"Unsupported arguments for `cls.extends`: `cls.extends(\"foo\", foobar)`."
    ();
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "methods",
        where = cls.name.matches("foo", is_transitive=foobar),
        model = ReturnModel(TaintSource[Test])
      )
    |}
    ~expect:{|`cls.name.matches("foo", is_transitive = foobar)` is not a valid name clause.|}
    ();
  assert_invalid_model
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
      {|Constraint `cls.matches` is deprecated, use `cls.fully_qualified_name.matches` instead.|}
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
    ~expect:{|`name.foo("foo")` is not a valid name clause.|}
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
    ~expect:"`name.matches(foobar, 1, 2)` is not a valid name clause."
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
    ~expect:"`name.equals(foobar, 1, 2)` is not a valid name clause."
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
      {|Unsupported named parameter `fnid` in model query (expected: name, find, where, model, expected_models, unexpected_models).|}
    ();
  assert_invalid_model
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
    ~expect:{|Duplicate parameter `find` in model query.|}
    ();

  (* Test expected_models and unexpected_models clauses *)
  assert_invalid_model
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
    ~expect:{|Duplicate parameter `expected_models` in model query.|}
    ();
  assert_invalid_model
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
    Please make sure that the model string is a syntactically correct model.|}
    ();
  assert_invalid_model
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
    ~expect:{|Multiple errors:
[
Unexpected statement: `foo(x)`
Unexpected statement: `food(y)`
]|}
    ();
  assert_invalid_model
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
    ~expect:"Syntax error."
    ();
  assert_invalid_model
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
      \   The clause should be a list of syntactically correct model strings."
    ();

  (* Test cls.any_child clause in model queries *)
  assert_valid_model
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
    |}
    ();
  assert_invalid_model
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
      {|Unsupported callee for class constraint: `Decorator(arguments.contains("1"), name.matches("d"))`|}
    ();
  assert_valid_model
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
    |}
    ();

  (* Test cls.any_parent clause in model queries *)
  assert_valid_model
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
    |}
    ();
  assert_invalid_model
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
      {|Unsupported callee for class constraint: `Decorator(arguments.contains("1"), name.matches("d"))`|}
    ();
  assert_valid_model
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
    |}
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
    ~expect:"Unsupported decorator constraint expression: `foo`"
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
    ~expect:{|`name.matches(a, b)` is not a valid name clause.|}
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
    ~expect:{|`name.equals(a, b)` is not a valid name clause.|}
    ();
  (* Model queries *)
  assert_invalid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "globals",
        where = name.matches("foo"),
        model = Returns(TaintSource[Test])
      )
    |}
    ~expect:"`Returns` is not a valid model for model queries with find clause of kind `globals`."
    ();
  assert_invalid_model
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
      "`AttributeModel` is not a valid model for model queries with find clause of kind `globals`."
    ();
  assert_valid_model
    ~model_source:
      {|
      ModelQuery(
        name = "invalid_model",
        find = "globals",
        where = type_annotation.is_annotated_type(),
        model = GlobalModel(TaintSource[A])
      )
    |}
    ();
  assert_invalid_model
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
       `globals`."
    ();
  assert_invalid_model
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
      "`cls.any_parent` is not a valid constraint for model queries with find clause of kind \
       `globals`."
    ();
  assert_invalid_model
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
      "`any_parameter.annotation.is_annotated_type` is not a valid constraint for model queries \
       with find clause of kind `globals`."
    ();
  assert_valid_model
    ~model_source:
      {|
      ModelQuery(
        name = "attributes",
        find = "attributes",
        where = type_annotation.is_annotated_type(),
        model = AttributeModel(ViaTypeOf)
      )
    |}
    ();
  assert_valid_model
    ~model_source:
      {|
      ModelQuery(
        name = "attributes",
        find = "attributes",
        where = type_annotation.is_annotated_type(),
        model = AttributeModel(ViaAttributeName)
      )
    |}
    ();
  assert_valid_model
    ~model_source:
      {|
      ModelQuery(
        name = "attributes",
        find = "attributes",
        where = type_annotation.is_annotated_type(),
        model = AttributeModel(ViaAttributeName[WithTag["foo"]])
      )
    |}
    ();

  (* ParameterPath/ReturnPath/UpdatePath on model queries *)
  assert_valid_model
    ~model_source:
      {|
      ModelQuery(
        name = "query",
        find = "functions",
        where = name.matches("foo"),
        model = Returns(TaintSource[Test, ReturnPath[_.bar]])
      )
    |}
    ();
  assert_valid_model
    ~model_source:
      {|
      ModelQuery(
        name = "query",
        find = "functions",
        where = name.matches("foo"),
        model = Parameters(TaintSource[Test, ParameterPath[_.bar]])
      )
    |}
    ();
  assert_valid_model
    ~model_source:
      {|
      ModelQuery(
        name = "query",
        find = "functions",
        where = name.matches("foo"),
        model = Parameters(TaintSink[Test, ParameterPath[_.bar]])
      )
    |}
    ();
  assert_valid_model
    ~model_source:
      {|
      ModelQuery(
        name = "query",
        find = "functions",
        where = name.matches("foo"),
        model = Parameters(TaintInTaintOut[LocalReturn, ParameterPath[_.bar], ReturnPath[_.baz]])
      )
    |}
    ();
  assert_invalid_model
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
      "`TaintSource[(Test, ParameterPath[_.bar])]` is an invalid taint annotation: `ParameterPath` \
       can only be used on parameters"
    ();
  assert_invalid_model
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
      "`TaintSource[(Test, ReturnPath[_.bar])]` is an invalid taint annotation: `ReturnPath[]` can \
       only be used as a return annotation or within `TaintInTaintOut[]`"
    ();
  assert_invalid_model
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
      "`TaintSource[(Test, UpdatePath[_.bar])]` is an invalid taint annotation: `UpdatePath` can \
       only be used within `TaintInTaintOut[]`"
    ();
  assert_invalid_model
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
      "`TaintInTaintOut[LocalReturn]` is an invalid taint annotation: `TaintInTaintOut[]` can only \
       be used on parameters, attributes or globals"
    ();
  assert_invalid_model
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
      "`TaintInTaintOut[(LocalReturn, ParameterPath[_.all_static_fields()])]` is an invalid taint \
       annotation: `all_static_fields()` is not allowed within `TaintInTaintOut[]`"
    ();
  assert_invalid_model
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
      "`TaintInTaintOut[(LocalReturn, ReturnPath[_.all_static_fields()])]` is an invalid taint \
       annotation: `all_static_fields()` is not allowed within `TaintInTaintOut[]`"
    ();
  assert_invalid_model
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
      "`TaintSource[(Test, ParameterPath[_.parameter_name()])]` is an invalid taint annotation: \
       `parameter_name()` can only be used within `TaintInTaintOut[]`"
    ();
  assert_invalid_model
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
      "`TaintSink[(Test, ParameterPath[_.parameter_name()])]` is an invalid taint annotation: \
       `parameter_name()` can only be used within `TaintInTaintOut[]`"
    ();
  assert_invalid_model
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
       annotation: `AppliesTo[]` cannot be used with `ParameterPath[]`"
    ();
  assert_invalid_model
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
      "`TaintInTaintOut[(LocalReturn, ParameterPath[_.foo])]` is an invalid taint annotation: \
       `ParameterPath` can only be used on parameters"
    ();
  ()


let test_invalid_decorators context =
  let assert_invalid_model ?path ?source ?sources ~model_source ~expect () =
    assert_invalid_model ?path ?source ?sources ~context ~model_source ~expect ()
  in

  let assert_valid_model ?path ?source ?sources ~model_source () =
    assert_invalid_model ?path ?source ?sources ~model_source ~expect:"no failure" ()
  in
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
  ()


let test_invalid_callables context =
  let assert_invalid_model ?path ?source ?sources ~model_source ~expect () =
    assert_invalid_model ?path ?source ?sources ~context ~model_source ~expect ()
  in

  let assert_valid_model ?path ?source ?sources ~model_source () =
    assert_invalid_model ?path ?source ?sources ~model_source ~expect:"no failure" ()
  in
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
  ()


let test_invalid_overloads context =
  let assert_invalid_model ?path ?source ?sources ~model_source ~expect () =
    assert_invalid_model ?path ?source ?sources ~context ~model_source ~expect ()
  in

  let assert_valid_model ?path ?source ?sources ~model_source () =
    assert_invalid_model ?path ?source ?sources ~model_source ~expect:"no failure" ()
  in
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
  ()


let test_invalid_via context =
  let assert_invalid_model ?path ?source ?sources ~model_source ~expect () =
    assert_invalid_model ?path ?source ?sources ~context ~model_source ~expect ()
  in

  let assert_valid_model ?path ?source ?sources ~model_source () =
    assert_invalid_model ?path ?source ?sources ~model_source ~expect:"no failure" ()
  in
  (* ViaTypeOf on attributes *)
  assert_invalid_model
    ~source:{|
      def foo(x: int) -> int: ...
      |}
    ~model_source:{|
      def test.foo(x: ViaTypeOf): ...
    |}
    ~expect:
      {|`ViaTypeOf` is an invalid taint annotation: A standalone `ViaTypeOf` without arguments can only be used in attribute or global models|}
    ();
  assert_invalid_model
    ~source:{|
      def foo(x: int) -> int: ...
      |}
    ~model_source:{|
      def test.foo(x) -> ViaTypeOf: ...
    |}
    ~expect:
      {|`ViaTypeOf` is an invalid taint annotation: A standalone `ViaTypeOf` without arguments can only be used in attribute or global models|}
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
      {|`ViaTypeOf` is an invalid taint annotation: A standalone `ViaTypeOf` without arguments can only be used in attribute or global models|}
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
      {|`ViaTypeOf` is an invalid taint annotation: A standalone `ViaTypeOf` without arguments can only be used in attribute or global models|}
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
      test.C.x: ViaTypeOf[WithTag["tag"]] = ...
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
  (* ViaAttributeName *)
  assert_invalid_model
    ~source:{|
      def foo(x: int) -> int: ...
      |}
    ~model_source:{|
      def test.foo(x: ViaAttributeName): ...
    |}
    ~expect:
      {|`ViaAttributeName` is an invalid taint annotation: `ViaAttributeName` can only be used in attribute or global models.|}
    ();
  assert_invalid_model
    ~source:{|
      class C:
        def foo(x: int) -> int: ...
      |}
    ~model_source:{|
      def test.C.foo() -> ViaAttributeName: ...
    |}
    ~expect:
      {|`ViaAttributeName` is an invalid taint annotation: `ViaAttributeName` can only be used in attribute or global models.|}
    ();
  assert_valid_model
    ~source:{|
      class C:
        x: int = 0
      |}
    ~model_source:{|
      test.C.x: ViaAttributeName = ...
    |}
    ();
  assert_valid_model
    ~source:{|
      class C:
        x: int = 0
      |}
    ~model_source:{|
      test.C.x: TaintInTaintOut[ViaAttributeName] = ...
    |}
    ();
  assert_valid_model
    ~source:{|
      class C:
        x: int = 0
      |}
    ~model_source:{|
      test.C.x: ViaAttributeName[WithTag["tag"]] = ...
    |}
    ();
  assert_valid_model
    ~source:{|
      class C:
        x: int = 0
      |}
    ~model_source:{|
      test.C.x: TaintSource[A, ViaAttributeName] = ...
    |}
    ();
  assert_valid_model
    ~source:{|
      class C:
        x: int = 0
      |}
    ~model_source:{|
      test.C.x: TaintSink[X, ViaAttributeName] = ...
    |}
    ();
  ()


let () =
  "taint_model"
  >::: [
         "invalid_models" >:: test_invalid_models;
         "invalid_model_queries" >:: test_invalid_model_queries;
         "invalid_decorators" >:: test_invalid_decorators;
         "invalid_callables" >:: test_invalid_callables;
         "invalid_overloads" >:: test_invalid_overloads;
         "invalid_via" >:: test_invalid_via;
       ]
  |> Test.run
