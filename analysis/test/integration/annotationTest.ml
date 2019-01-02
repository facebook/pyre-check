(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest


let test_check_missing_type_parameters _ =
  assert_type_errors
    {|
      T = typing.TypeVar("_T")
      class C(typing.Generic[T]): ...
      def f(c: C) -> None:
        return None
    |}
    ["Missing type parameters [24]: Generic type `C` expects 1 type parameter."];
  assert_type_errors
    {|
      T = typing.TypeVar("_T")
      class C(typing.Generic[T]): ...
      def f(c: typing.List[C]) -> None:
        return None
    |}
    ["Missing type parameters [24]: Generic type `C` expects 1 type parameter."];
  assert_type_errors
    {|
      T = typing.TypeVar("_T")
      class C(typing.Generic[T]): ...
      def f() -> typing.List[C]:
        return []
    |}
    ["Missing type parameters [24]: Generic type `C` expects 1 type parameter."];
  assert_type_errors
    {|
      T = typing.TypeVar("_T")
      S = typing.TypeVar("_S")
      class C(typing.Generic[T, S]): ...
      def f() -> typing.List[C]:
        return []
    |}
    ["Missing type parameters [24]: Generic type `C` expects 2 type parameters."]


let test_check_invalid_type _ =
  assert_type_errors
    {|
      MyType: typing.Type[int] = int
      x: MyType = 1
    |}
    [];
  assert_type_errors
    {|
      x: MyType = 1
    |}
    ["Undefined type [11]: Type `MyType` is not defined."];
  assert_type_errors
    {|
      MyType: int
      x: MyType = 1
    |}
    ["Invalid type [31]: Expression `MyType` is not a valid type."];
  assert_type_errors
    {|
      MyType = 1
      x: MyType = 1
    |}
    [
      "Missing global annotation [5]: Globally accessible variable `MyType` has type `int`" ^
      " but no type is specified.";
      "Invalid type [31]: Expression `MyType` is not a valid type."
    ];
  assert_type_errors
    {|
      MyType: typing.Any
      x: MyType = 1
    |}
    []


let test_check_undefined_type _ =
  assert_type_errors
    ~debug:false
    {|
      def foo(x: Derp) -> Herp:
        pass
    |}
    [
      "Undefined type [11]: Type `Derp` is not defined.";
      "Undefined type [11]: Type `Herp` is not defined.";
    ];
  assert_type_errors
    ~debug:false
    {|
      def foo(x: Derp, y: Herp) -> None:
        pass
    |}
    [
      "Undefined type [11]: Type `Derp` is not defined.";
      "Undefined type [11]: Type `Herp` is not defined.";
    ];
  assert_type_errors
    ~debug:false
    {|
      def foo(x: int) -> Herp:
        return x
    |}
    ["Undefined type [11]: Type `Herp` is not defined."];
  assert_type_errors
    ~debug:false
    {|
      def foo(x: typing.Union[Derp, Herp]) -> typing.List[Herp]:
        pass
    |}
    [
      "Undefined type [11]: Type `Derp` is not defined.";
      "Undefined type [11]: Type `Herp` is not defined.";
    ];
  assert_type_errors
    ~debug:false
    {|
      def foo(x: Derp[int]) -> None:
        pass
    |}
    ["Undefined type [11]: Type `Derp` is not defined."];
  assert_type_errors
    ~debug:false
    {|
      def foo(x: Derp[int, str]) -> None:
        pass
    |}
    ["Undefined type [11]: Type `Derp` is not defined."];
  assert_type_errors
    ~debug:false
    {|
      def foo(x: typing.Optional[Derp[int]]) -> typing.List[Herp]:
        pass
    |}
    [
      "Undefined type [11]: Type `Derp` is not defined.";
      "Undefined type [11]: Type `Herp` is not defined.";
    ];
  assert_type_errors
    ~debug:false
    {|
      def foo(x: Optional) -> None:
        pass
    |}
    ["Undefined type [11]: Type `Optional` is not defined."];
  assert_type_errors
    ~debug:false
    {|
      def foo(x: Dict) -> None:
        pass
    |}
    ["Undefined type [11]: Type `Dict` is not defined."];

  assert_type_errors
    ~debug:false
    {|
      def foo() -> None:
        x: undefined = 1
        return
    |}
    ["Undefined type [11]: Type `undefined` is not defined."];
  assert_type_errors
    ~debug:false
    {|
      def foo(x: Derp) -> None:
        y: undefined = 1
        return
    |}
    [
      "Undefined type [11]: Type `Derp` is not defined.";
      "Undefined type [11]: Type `undefined` is not defined.";
    ];

  assert_type_errors
    {|
      T = typing.TypeVar('T')
      def foo(x: T) -> typing.Union[str, T]:
        return x
    |}
    [];

  (* Ensure other errors are not missed when undefined type is thrown. *)
  assert_type_errors
    {|
      class Bar:
          async def undefined(self, x: Derp) -> Derp:
              return x
      class Foo(Bar):
          def error(self) -> int:
              return None
          async def undefined(self, x: Herp) -> Herp:
              return x
    |}
    [
      "Undefined type [11]: Type `Derp` is not defined.";
      "Undefined type [11]: Type `Derp` is not defined.";
      "Incompatible return type [7]: Expected `int` but got `None`.";
      "Undefined type [11]: Type `Herp` is not defined.";
      "Undefined type [11]: Type `Herp` is not defined.";
    ]


let test_check_analysis_failure _ =
  assert_type_errors
    {|
      def foo() -> Derp:
        pass

      def bar(x: int = foo()) -> int:
        return x
    |}
    ["Analysis failure [30]: Terminating analysis because type `Derp` is not defined."]


let () =
  "annotation">:::[
    "check_undefined_type">::test_check_undefined_type;
    "check_analysis_failure">::test_check_analysis_failure;
    "check_invalid_type">::test_check_invalid_type;
    "check_missing_type_parameters">::test_check_missing_type_parameters;
  ]
  |> Test.run
