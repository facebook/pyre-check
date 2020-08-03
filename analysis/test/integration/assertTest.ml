(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_check_assert context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import typing
      def foo(optional: typing.Optional[str]) -> None:
        if optional or len(optional) > 0:
          pass
    |}
    [
      "Incompatible parameter type [6]: "
      ^ "Expected `typing.Sized` for 1st positional only parameter to call `len` but got "
      ^ "`typing.Optional[str]`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo(optional: typing.Optional[str]) -> None:
        if optional is None or len(optional) > 0:
          pass
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(optional: typing.Optional[str]) -> None:
        if optional and len(optional) > 0:
          pass
    |}
    [];
  assert_type_errors
    {|
      from builtins import int_to_int
      def foo() -> int:
        if 1 > 2:
          x = 2
        else:
          assert False
        return int_to_int(x)
    |}
    [];
  assert_type_errors
    {|
      from builtins import int_to_int
      def foo() -> int:
        if 1 > 2:
          x = 2
        else:
          assert False, "unreachable, surely"
        return int_to_int(x)
    |}
    [];
  assert_type_errors
    {|
      from builtins import int_to_int
      def foo() -> int:
        if 1 > 2:
          x = 2
        else:
          assert not True
        return int_to_int(x)
    |}
    [];
  assert_type_errors
    {|
      from builtins import int_to_int
      def foo() -> int:
        if True:
          return 0
        else:
          return int_to_int("monkey news")
    |}
    [];
  assert_type_errors
    {|
      from builtins import expect_int
      import typing
      def foo(x: typing.Optional[int]) -> None:
        if x is not None:
          expect_int(x)
          y = x if x is not None else 32
          expect_int(y)
          expect_int(x)
    |}
    [];
  assert_type_errors
    {|
      from builtins import expect_int
      import typing
      def foo(x: typing.Optional[int]) -> None:
        if x is not None:
          expect_int(x)
          y = 32 if x is None else x
          expect_int(y)
          expect_int(x)
    |}
    [];
  assert_type_errors
    {|
      from builtins import expect_int
      import typing
      def foo(x: typing.Optional[int]) -> None:
        if x is not None:
          expect_int(x)
          if x is not None:
            y = 12
          else:
            y = 34
          expect_int(y)
          expect_int(x)
    |}
    [];
  assert_type_errors
    {|
      from builtins import expect_int
      import typing
      def foo(x: typing.Optional[int]) -> None:
        if x is not None:
          expect_int(x)
          if x is None:
            y = 56
          else:
            y = 78
          expect_int(y)
          expect_int(x)
    |}
    [];
  assert_type_errors
    {|
      from builtins import expect_int
      import typing
      def foo(x: typing.Optional[int]) -> None:
        if x is None:
          pass
        else:
          expect_int(x)
          y = x if x is not None else 32
          expect_int(y)
          expect_int(x)
    |}
    [];
  assert_type_errors
    {|
      from builtins import expect_int
      import typing
      def foo(x: typing.Optional[int]) -> None:
        if x is None:
          pass
        else:
          expect_int(x)
          y = 32 if x is None else x
          expect_int(y)
          expect_int(x)
    |}
    []


let test_check_assert_functions context =
  assert_default_type_errors
    ~context
    {|
      import typing
      class One:
          a: int = 1

      # The actual content of this function does not really matter.
      def pyretestassert(x: typing.Any) -> None:
          pass

      def f(o: typing.Optional[One]) -> int:
          assert o
          return o.a

      def f2(o: typing.Optional[One]) -> int:
          pyretestassert(o)
          return o.a
    |}
    ["Undefined attribute [16]: Optional type has no attribute `a`."];
  assert_default_type_errors
    ~context
    ~handle:"foo.py"
    {|
      import typing
      class One:
          a: int = 1

      # The actual content of this function does not really matter.
      def pyretestassert(x: typing.Any) -> None:
          pass

      def f(o: typing.Optional[One]) -> int:
          assert o
          return o.a

      def f2(o: typing.Optional[One]) -> int:
          pyretestassert(o)
          return o.a
    |}
    [];
  assert_type_errors
    ~context
    {|
      import typing
      class One:
          a: int = 1

      def f(o: typing.Optional[One]) -> int:
          assert o
          return o.a

      def f2(o: typing.Optional[One]) -> int:
          pyretestassert(o)
          return o.a
    |}
    [
      "Unbound name [10]: Name `pyretestassert` is used but not defined in the current scope.";
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: Optional type has no attribute `a`.";
    ]


let test_check_all context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import typing
      def foo(x: typing.List[typing.Optional[str]]) -> typing.Optional[str]:
        if all(x):
          return ','.join(x)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Iterable[typing.Optional[str]]) -> typing.Optional[str]:
        if all( x):
          return ','.join(x)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Iterable[typing.Optional[str]]) -> typing.Optional[str]:
        if not all(x):
          return ','.join(x)
    |}
    [
      "Incompatible parameter type [6]: Expected `typing.Iterable[str]` for 1st positional only \
       parameter to call `str.join` but got `typing.Iterable[typing.Optional[str]]`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Iterable[typing.Union[str, None]]) -> typing.Optional[str]:
        if all(x):
          return ','.join(x)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Iterable[typing.Union[str, int, None]]) -> \
          typing.Iterable[typing.Union[str, int]]:
        if all(x):
          return x
        return []
    |}
    [];
  assert_type_errors
    {|
      import typing
      _T = typing.TypeVar('_T')
      def foo(x: typing.Dict[typing.Optional[int], _T]) -> typing.Dict[int, _T]:
        if all(x):
          return x
        return {}
    |}
    [
      "Incompatible return type [7]: Expected `typing.Dict[int, Variable[_T]]` but got \
       `typing.Dict[typing.Optional[int], Variable[_T]]`.";
    ]


let test_check_impossible_assert context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_type_errors {|
      def foo() -> None:
        x = None
        assert x
    |} [];
  assert_type_errors {|
      def foo(x: None) -> None:
        assert x
    |} [];
  assert_default_type_errors
    {|
      from typing import Optional, Any
      def foo(x: Optional[Any] = None) -> None:
        assert x
    |}
    (* We should not treat `x` as having type `None` here *)
    [];
  assert_type_errors
    {|
      def foo(x: Derp) -> None:
        assert x
    |}
    ["Unbound name [10]: Name `Derp` is used but not defined in the current scope."];
  assert_default_type_errors
    {|
      import typing
      def foo(x: typing.Any) -> None:
        assert x
    |}
    [];

  assert_default_type_errors
    {|
      class Derp: ...
      def derp(x: Derp) -> None:
        assert not isinstance(x, Herp)
    |}
    ["Unbound name [10]: Name `Herp` is used but not defined in the current scope."];
  assert_default_type_errors
    {|
      import typing
      class Derp: ...
      def derp(x: Derp, y: typing.Type[typing.Any]) -> None:
        assert not isinstance(x, y)
    |}
    [];
  assert_type_errors
    {|
      class Derp: ...
      def derp(x: Derp) -> None:
        assert not isinstance(x, int)
    |}
    [];
  assert_type_errors
    {|
      class Derp: ...
      def derp() -> None:
        assert not isinstance(42, Derp)
    |}
    [];

  (* Fabricated asserts won't get the type error surfaced. *)
  assert_type_errors
    {|
     from typing import Union, Dict, Any

     def foo(x: Dict[str, Any]) -> None:
       if isinstance(x, dict):
         pass
   |}
    [];
  assert_type_errors
    {|
     from typing import Union, Dict, Any

     def foo() -> None:
       x = None
       if x is None:
         pass
   |}
    [];
  ()


let () =
  "assert"
  >::: [
         "check_assert" >:: test_check_assert;
         "check_assert_functions" >:: test_check_assert_functions;
         "check_all" >:: test_check_all;
         "check_impossible_assert" >:: test_check_impossible_assert;
       ]
  |> Test.run
