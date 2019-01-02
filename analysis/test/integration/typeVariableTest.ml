(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest


let test_check_unbounded_variables _ =
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      def expects_any(input) -> None: ...
      def expects_string(inut: str) -> None: ...
      def foo(input: T) -> None:
        expects_any(input)
        expects_string(input)
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `str` for 1st anonymous parameter to call `expects_string` but got `Variable[T]`."];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      def foo(input: T) -> typing.Any:
        return input
    |}
    ["Missing return annotation [3]: Returning `Variable[T]` but type `Any` is specified."];
  assert_type_errors
    {|
      T = typing.TypeVar('T')
      def foo(input: T) -> int:
        return input
    |}
    ["Incompatible return type [7]: Expected `int` but got `Variable[T]`."]


let test_check_variable_bindings _ =
  assert_type_errors
    {|
      T = typing.TypeVar('T', bound=int)
      def foo(t: T) -> None:
        str_to_int(t)
    |}
    ["Incompatible parameter type [6]: " ^
     "Expected `str` for 1st anonymous parameter to call `str_to_int` but got " ^
     "`Variable[T (bound to int)]`."];
  assert_type_errors
    {|
      T = typing.TypeVar('T', bound=int)
      def foo() -> T:
        return 1.0
    |}
    ["Incompatible return type [7]: Expected `Variable[T (bound to int)]` but got `float`."];
  assert_type_errors
    {|
      T = typing.TypeVar('T', bound=int)
      def foo(t: T) -> None:
        int_to_str(t)
      def bar(x: str) -> None:
        foo(x)
    |}
    ["Incompatible parameter type [6]: Expected `Variable[T (bound to int)]` for 1st anonymous " ^
     "parameter to call `foo` but got `str`."];
  assert_type_errors
    {|
      class C():
        def baz(self) -> int:
          return 7
      T = typing.TypeVar('T', bound=C)
      def foo(t: T) -> int:
        return t.baz()
    |}
    []


let () =
  "typeVariable">:::[
    "check_unbounded_variables">::test_check_unbounded_variables;
    "check_variable_bindings">::test_check_variable_bindings;
  ]
  |> Test.run
