(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open OUnit2
open IntegrationTest


let test_check_function_parameters _ =
  assert_type_errors
    {|
      def foo() -> None:
        int_to_int(1)
    |}
    [];

  assert_type_errors
    {|
      def foo() -> None:
        int_to_int(1.0)
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `int_to_int` but got `float`.";
    ];
  assert_type_errors
    {|
      def preprocessed($renamed_i: str) -> None:
        pass
      def foo() -> None:
        preprocessed(1.0)
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `str` for 1st anonymous parameter to call `preprocessed` but got `float`.";
    ];

  assert_type_errors
    {|
      def foo() -> int:
        return int_to_int(1.0)
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `int_to_int` but got `float`.";
    ];

  assert_type_errors
    {|
      def foo(i) -> None:
        int_to_int(i)
    |}
    ["Missing parameter annotation [2]: Parameter `i` has no type specified."];

  assert_type_errors
    {|
      def foo(i: int, *, j: int) -> None:
        pass
    |}
    [];

  assert_type_errors
    {|
      def foo( *args, **kwargs) -> None:
        pass
    |}
    [
      "Missing parameter annotation [2]: Parameter `*args` has no type specified.";
      "Missing parameter annotation [2]: Parameter `**kwargs` has no type specified.";
    ];

  assert_type_errors
    {|
      class A:
        def foo(self) -> None:
          int_to_int(self.attribute)
    |}
    ["Undefined attribute [16]: `A` has no attribute `attribute`."];
  assert_type_errors
    {|
      class C:
       attribute: int = 1
      try:
        x = C()
      except:
        pass
      x.attribute
    |}
    ["Undefined name [18]: Global name `x` is undefined."];

  assert_type_errors
    {|
      def foo(a: typing.Union[str, None]) -> None: pass
      foo(None)
    |}
    [];

  assert_type_errors
    {|
      def foo(a: typing.Union[str, None, typing.Tuple[int, str]]) -> None:
        pass
      foo(None)
    |}
    [];

  assert_type_errors
    {|
      def foo(a: typing.Optional[int]) -> int:
        return to_int(a and int_to_str(a))
    |}
    [];

  assert_type_errors
    {|
      def foo(a: typing.Optional[int]) -> int:
        return to_int(a or int_to_str(a))
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `int` for 1st anonymous parameter to call `int_to_str` but got " ^
      "`typing.Optional[int]`.";
    ];

  assert_type_errors
    {|
      def foo(a: int) -> int:
        return a
      x: typing.Optional[int]
      foo(x if x else 1)
    |}
    [];

  assert_type_errors
    {|
      def bar(x: typing.Optional[Attributes]) -> None:
          baz(x.int_attribute if x is not None else None)

      def baz(x: typing.Optional[int]) -> None:
          pass
    |}
    [];

  assert_type_errors
    {|
      def bar(x: typing.Optional[Attributes]) -> None:
          baz(x.int_attribute if x else None)

      def baz(x: typing.Optional[int]) -> None:
          pass
    |}
    [];


  assert_type_errors
    {|
      def foo(x) -> None:
        takes_iterable(x)
    |}
    ["Missing parameter annotation [2]: Parameter `x` has no type specified."];
  assert_type_errors
    {|
      def foo(a):  # type: (typing.Optional[int]) -> None
        pass
      foo(None)
    |}
    [];
  assert_type_errors
    {|
      def foo(a):  # type: (typing.Optional[int]) -> None
        pass
      foo("hello")
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `typing.Optional[int]` for 1st anonymous parameter to call `foo` but got `str`."];
  assert_type_errors
    {|
      def foo(a):
        # type: (typing.Optional[int]) -> None
        pass
      foo("hello")
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `typing.Optional[int]` for 1st anonymous parameter to call `foo` but got `str`."];
  assert_type_errors
    {|
      def foo(a, b):
        # type: (typing.Optional[int], str) -> None
        pass
      foo(1, "hello")
    |}
    [];
  assert_type_errors
    {|
      def foo(a, b):
        # type: (typing.Optional[int], str) -> None
        pass
      foo(1, 1)
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `str` for 2nd anonymous parameter to call `foo` but got `int`."]


let test_check_function_parameter_errors _ =
  assert_type_errors
    {|
      class Foo:
        attribute: str = ""
      def foo(input: Foo) -> None:
        str_float_to_int(input.attribute, input.undefined)
    |}
    ["Undefined attribute [16]: `Foo` has no attribute `undefined`."];
  assert_type_errors
    {|
      class Foo:
        attribute: str = ""
      def foo(input: Foo) -> None:
        str_float_to_int(input.undefined, input.undefined)
    |}
    [
      "Undefined attribute [16]: `Foo` has no attribute `undefined`.";
      "Undefined attribute [16]: `Foo` has no attribute `undefined`.";
    ];

  assert_type_errors
    {|
      class Foo:
        attribute: int = 1
      def foo(input: typing.Optional[Foo]) -> None:
        optional_str_to_int(input and input.attribute)
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `typing.Optional[str]` for 1st anonymous parameter to call `optional_str_to_int` " ^
      "but got `typing.Optional[int]`.";
    ];
  assert_type_errors
    {|
      class Foo:
        attribute: int = 1
      def foo(input: typing.Optional[Foo]) -> None:
        optional_str_to_int(input and input.undefined)
    |}
    [
      "Incompatible parameter type [6]: " ^
      "Expected `typing.Optional[str]` for 1st anonymous parameter to call `optional_str_to_int` " ^
      "but got `unknown`.";
      "Undefined attribute [16]: `Foo` has no attribute `undefined`.";
    ];
  assert_type_errors
    {|
      class attribute:
        ...
      class other:
        attribute: int = 1
      def foo(o: other) -> str:
        return o.attribute
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."]


let () =
  "functionParameters">:::[
    "check_function_parameters">::test_check_function_parameters;
    "check_function_parameter_errors">::test_check_function_parameter_errors;
  ]
  |> Test.run
