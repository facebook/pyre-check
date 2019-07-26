(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_check_isinstance context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
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
    [ "Undefined name [18]: Global name `NonexistentClass` is not defined, or there is at least \
       one control flow path that doesn't define `NonexistentClass`." ];
  assert_type_errors
    {|
      def foo(x: int) -> None:
        if isinstance(x, str):
          reveal_type(x)
        reveal_type(x)
    |}
    [ "Revealed type [-1]: Revealed type for `x` is `str`.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[int, str]`." ];
  assert_type_errors
    {|
      def foo(x: int) -> None:
        if isinstance(x, NonexistentClass):
          reveal_type(x)
        reveal_type(x)
    |}
    [ "Undefined name [18]: Global name `NonexistentClass` is not defined, or there is at least \
       one control flow path that doesn't define `NonexistentClass`.";
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `x` is `int`." ];
  assert_type_errors
    {|
      def foo(x: typing.Union[int, typing.List[int]]) -> None:
        if isinstance(x, list):
          reveal_type(x)
        else:
          reveal_type(x)
    |}
    [ "Revealed type [-1]: Revealed type for `x` is `typing.List[int]`.";
      "Revealed type [-1]: Revealed type for `x` is `int`." ];
  assert_type_errors
    {|
      def foo(x: typing.Union[int, typing.List[str], str, typing.List[int]]) -> None:
        if isinstance(x, list):
          reveal_type(x)
        else:
          reveal_type(x)
    |}
    [ "Revealed type [-1]: Revealed type for `x` is "
      ^ "`typing.Union[typing.List[int], typing.List[str]]`.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[int, str]`." ];
  assert_type_errors
    {|
      def foo(x: typing.Union[int, typing.Set[str], str, typing.Set[int]]) -> None:
        if isinstance(x, set):
          reveal_type(x)
        else:
          reveal_type(x)
    |}
    [ "Revealed type [-1]: Revealed type for `x` is "
      ^ "`typing.Union[typing.Set[int], typing.Set[str]]`.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[int, str]`." ];
  assert_type_errors
    {|
      class CommonBase(): pass
      class ChildA(CommonBase): pass
      class ChildB(CommonBase): pass
      class Unrelated(): pass
      def foo(x: typing.Union[int, ChildA, ChildB, Unrelated]) -> None:
        if isinstance(x, CommonBase):
          reveal_type(x)
        else:
          reveal_type(x)
    |}
    [ "Revealed type [-1]: Revealed type for `x` is `typing.Union[ChildA, ChildB]`.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[Unrelated, int]`." ];
  assert_type_errors
    {|
      def foo(x: typing.Union[int, float, bool]) -> None:
        if isinstance(x, str):
          reveal_type(x)
        else:
          reveal_type(x)
    |}
    [ "Revealed type [-1]: Revealed type for `x` is `str`.";
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[bool, float, int]`." ];
  assert_type_errors "isinstance(1, (int, str))" [];
  assert_type_errors "isinstance(1, (int, (int, str)))" [];
  assert_type_errors
    "isinstance(str, '')"
    [ "Incompatible parameter type [6]: "
      ^ "Expected `typing.Type[typing.Any]` for 2nd anonymous parameter to call `isinstance` "
      ^ "but got `str`." ];
  assert_type_errors
    "isinstance(1, (int, ('', str)))"
    [ "Incompatible parameter type [6]: "
      ^ "Expected `typing.Type[typing.Any]` for 2nd anonymous parameter to call `isinstance` "
      ^ "but got `str`." ]


let () = "isinstance" >::: ["check_isinstance" >:: test_check_isinstance] |> Test.run
