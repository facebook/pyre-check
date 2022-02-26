(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
      "Incompatible parameter type [6]: In call `len`, for 1st positional only parameter expected \
       `Sized` but got `Optional[str]`.";
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
  (* TODO(T93984519): Uninitialized local should not throw an error in these. *)
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
    ["Uninitialized local [61]: Local variable `x` is undefined, or not always defined."];
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
    [];
  assert_type_errors
    {|
      from typing import Tuple, Any

      def foo() -> None:
        # pyre-ignore[33]: Explicit Any.
        stringish_types: Tuple[Any, ...]

        y: str
        if isinstance(y, stringish_types):
          pass
   |}
    [];
  ()


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
      "Incompatible parameter type [6]: In call `str.join`, for 1st positional only parameter \
       expected `Iterable[typing_extensions.LiteralString]` but got `Iterable[Optional[str]]`.";
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
      "Incompatible return type [7]: Expected `Dict[int, Variable[_T]]` but got \
       `Dict[Optional[int], Variable[_T]]`.";
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
  assert_type_errors
    {|
      def f() -> None:
        a = None
        b = None

        for c in [2, 3, 4]:
          a = c
          b = c

        reveal_type(a)
        assert a is not None and b is not None
        reveal_type(a)

        if a >= 0:
          pass
    |}
    [
      "Revealed type [-1]: Revealed type for `a` is `typing.Optional[int]`.";
      "Revealed type [-1]: Revealed type for `a` is `int`.";
    ];
  ()


let test_if_statement context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Optional

      def foo(some_int: int) -> None:
        x: Optional[int] = None
        y: Optional[int] = None

        for _ in [1]:
          x = some_int
          y = some_int

        if x is None or y is None:
          return None

        reveal_type(x)
        reveal_type(y)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `Optional[int]` (inferred: `int`).";
      "Revealed type [-1]: Revealed type for `y` is `Optional[int]` (inferred: `int`).";
    ];
  (* No revealed types because that code is unreachable. *)
  assert_type_errors
    {|
      from typing import Optional

      def foo(some_int: int) -> None:
        x: Optional[int] = None
        y: Optional[int] = None

        if x is None or y is None:
          return None

        reveal_type(x)
        reveal_type(y)
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
         "if_statement" >:: test_if_statement;
       ]
  |> Test.run
