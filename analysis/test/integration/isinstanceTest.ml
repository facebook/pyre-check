(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest


let test_check_isinstance _ =
  assert_default_type_errors
    {|
      def f(x) -> int:
        class Stub:
          ...
        class Actual:
          def f(self) -> int:
            return 0
        if isinstance(x, Stub):
          return -1
        elif isinstance(x, Actual):
          return 0
        else:
          return 1
    |}
    [];

  assert_type_errors
    {|
      isinstance(1, NonexistentClass)
    |}
    ["Undefined name [18]: Global name `NonexistentClass` is undefined."];

  assert_type_errors
    {|
      def foo(x: int) -> None:
        if isinstance(x, str):
          reveal_type(x)
        reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `str`.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[int, str]`.";
    ];

  assert_type_errors
    {|
      def foo(x: int) -> None:
        if isinstance(x, NonexistentClass):
          reveal_type(x)
        reveal_type(x)
    |}
    [
      "Undefined name [18]: Global name `NonexistentClass` is undefined.";
      "Revealed type [-1]: Revealed type for `x` is `unknown`.";
      "Revealed type [-1]: Revealed type for `x` is `unknown`.";
    ];
  assert_type_errors
    {|
      def foo(x: typing.Union[int, typing.List[int]]) -> None:
        if isinstance(x, list):
          reveal_type(x)
        else:
          reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing.List[typing.Any]`.";
      "Revealed type [-1]: Revealed type for `x` is `int`.";
    ];
  assert_type_errors
    {|
      def foo(x: typing.Union[int, typing.List[str], str, typing.List[int]]) -> None:
        if isinstance(x, list):
          reveal_type(x)
        else:
          reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing.List[typing.Any]`.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[int, str]`.";
    ];

  assert_type_errors "isinstance(1, (int, str))" [];
  assert_type_errors "isinstance(1, (int, (int, str)))" [];
  assert_type_errors
    "isinstance(str, '')"
    ["Incompatible parameter type [6]: " ^
     "Expected `typing.Type[typing.Any]` for 2nd anonymous parameter to call `isinstance` " ^
     "but got `str`."];
  assert_type_errors
    "isinstance(1, (int, ('', str)))"
    ["Incompatible parameter type [6]: " ^
     "Expected `typing.Type[typing.Any]` for 2nd anonymous parameter to call `isinstance` " ^
     "but got `str`."]


let () =
  "isinstance">:::[
    "check_isinstance">::test_check_isinstance
  ]
  |> Test.run
