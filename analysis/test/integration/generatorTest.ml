(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_check_comprehensions context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import typing
      def foo(input: typing.List[str]) -> typing.List[str]:
        return [a for a in input]
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(input: typing.List[str]) -> typing.List[str]:
        return [a for a in input if len(a) < 5]
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(input: str) -> typing.List[int]:
        return [a for a in input]
    |}
    ["Incompatible return type [7]: Expected `typing.List[int]` but got `typing.List[str]`."];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.List[str]:
        return [x for x in [4,3,None, 1] if x]
    |}
    ["Incompatible return type [7]: Expected `typing.List[str]` but got `typing.List[int]`."];
  assert_type_errors
    {|
      import typing
      def foo(l: typing.List[int]) -> None:
        a = [x > 0 and x < 0 for x in l]
    |}
    [];
  assert_type_errors
    {|
      x: str = ""
      [x for x in [1,2,3,4,5] if isinstance(x, int)]
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(l: typing.Dict[int, str]) -> None:
        [x if y else 0 for x, y in l.items()]
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Dict[int, int]:
        return { 0: x for x in [4,3,2] }
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(d: typing.Dict[int, str]) -> typing.Dict[int, str]:
        return { k: v for k, v in d }
    |}
    [ "Incompatible return type [7]: Expected `typing.Dict[int, str]` but got " ^ "`typing.Dict[]`.";
      "Unable to unpack [23]: Unable to unpack `int` into 2 values." ];
  assert_type_errors
    {|
      import typing
      def foo(d: typing.Dict[int, str]) -> typing.Dict[int, str]:
        return { k: v for k, v in d.items() }
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(input: typing.List[str]) -> typing.List[str]:
        return [a.lower() for a in input]
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(input: typing.List[str]) -> typing.List[int]:
        return [str_to_int(a) for a in input]
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(input: typing.Set[str]) -> typing.Set[str]:
        return {a for a in input}
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(input: typing.Set[str]) -> typing.Set[str]:
        return {a.lower() for a in input}
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(a: typing.List[str], b: typing.List[str]) -> int:
        return {x + y for x in a for y in b}
    |}
    ["Incompatible return type [7]: Expected `int` but got `typing.Set[str]`."];
  assert_type_errors
    {|
      import typing
      def foo(a: typing.Dict[str, int]) -> typing.List[int]:
        return [x for x in a]
    |}
    ["Incompatible return type [7]: Expected `typing.List[int]` but got `typing.List[str]`."];
  assert_type_errors
    {|
      def f() -> int:
          x = {
              "a": [k[0] for x in {1: 1}] for k in [[1]]
          }
          return 0
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(a: typing.Dict[str, int]) -> typing.Dict[str, str]:
        return { x:x for x, y in a.items() }
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(a: typing.Dict[str, int]) -> typing.Dict[str, int]:
        return { y:x for x, y in a.items() }
    |}
    [ "Incompatible return type [7]: Expected `typing.Dict[str, int]` but got "
      ^ "`typing.Dict[int, str]`." ];
  assert_type_errors
    {|
      def f() -> None:
        l = lambda y: y
        l(1)
        lambda *y: y
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(a: typing.Dict[str, typing.Optional[int]]) -> typing.Dict[str, int]:
        return { x: y for (x, y) in a.items() if y }
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(a: typing.Dict[str, typing.Optional[int]]) -> typing.Dict[str, int]:
        return { x: y for (x, y) in a.items() }
    |}
    [ "Incompatible return type [7]: Expected `typing.Dict[str, int]` but got `typing.Dict[str, \
       typing.Optional[int]]`." ];
  assert_type_errors
    {|
      import typing
      def foo(a: typing.Dict[str, typing.Optional[int]]) -> typing.Dict[str, int]:
        return { x: int_to_int(y) for (x, y) in a.items() if y }
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(d: typing.Dict[str, int]) -> None:
        { k: v for k, v in d }
    |}
    []


let test_check_generators context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import typing
      x: typing.Generator[typing.Any, typing.Any, typing.Any]
      def foo() -> typing.Generator:
        return x
    |}
    [ "Missing global annotation [5]: Globally accessible variable `x` must be specified "
      ^ "as type that does not contain `Any`.";
      "Invalid type parameters [24]: Generic type `typing.Generator` expects 3 type parameters." ];
  assert_type_errors
    {|
      import typing
      x: typing.Generator[int, int, int]
      def foo() -> typing.Generator:
        return x
    |}
    [ "Invalid type parameters [24]: Generic type `typing.Generator` expects 3 type parameters.";
      "Incompatible return type [7]: Expected `typing.Generator[typing.Any, typing.Any, "
      ^ "typing.Any]` but got `typing.Generator[int, int, int]`." ];
  assert_type_errors
    {|
      import typing
      def foo(l: typing.List[int])->typing.Generator[int, None, None]:
        return (x for x in l)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(l: typing.List[typing.Optional[int]])->typing.Generator[int, None, None]:
        return (x for x in l if x)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(l: typing.List[typing.Optional[int]])->typing.Generator[str, None, None]:
        return (x for x in l if x is not None)
    |}
    [ "Incompatible return type [7]: Expected `typing.Generator[str, None, None]` "
      ^ "but got `typing.Generator[int, None, None]`." ];
  assert_type_errors
    {|
      import typing
      def foo(l: typing.Iterable[typing.Any])->typing.Generator[typing.Any, None, None]:
        return (x for x in l)
    |}
    [ "Missing return annotation [3]: Return type must be specified as type "
      ^ "that does not contain `Any`.";
      "Missing parameter annotation [2]: Parameter `l` must have a type "
      ^ "that does not contain `Any`." ];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Generator[int, None, None]:
        yield 1
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(i: int) -> typing.Iterable[int]:
        if i > 2:
          return
        else:
          yield i
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Generator[int, None, None]:
        yield 1.0
    |}
    [ "Incompatible return type [7]: Expected `typing.Generator[int, None, None]` "
      ^ "but got `typing.Generator[float, None, None]`." ];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Generator[int, None, None]:
        yield from [1]
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Generator[int, None, None]:
        yield from [""]
    |}
    [ "Incompatible return type [7]: Expected `typing.Generator[int, None, None]` "
      ^ "but got `typing.Generator[str, None, None]`." ];
  assert_type_errors
    {|
      import typing
      def generator() -> typing.Generator[int, None, None]:
        yield 1
      def wrapper() -> typing.Generator[int, None, None]:
        yield from generator()
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Generator[None, None, None]:
        yield
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Iterable[None]:
        yield
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Generator[None, None, None]:
        yield
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(flag: bool) -> typing.Generator[int, None, None]:
        if flag:
          return
        yield 1
    |}
    [];
  assert_type_errors
    {|
      import typing
      async def foo(flag: bool) -> typing.AsyncGenerator[int, None]:
        if flag:
          return
        yield 1
    |}
    [];
  assert_type_errors
    {|
      import typing
      import asyncio.coroutines

      @asyncio.coroutines.coroutine
      def get() -> typing.Generator[typing.Any, None, int]: ...
      async def foo() -> int:
        awaited = await get()
        return awaited
    |}
    [];
  assert_type_errors
    {|
      import typing
      async def foo() -> typing.AsyncGenerator[int, None]:
        yield 1
    |}
    [];
  assert_type_errors
    {|
      import typing
      def takes_int(x: int) -> int:
        return x
      async def loop(g: typing.AsyncGenerator[str, None]) -> typing.AsyncGenerator[int, None]:
        async for item in g:
          yield takes_int(item)
    |}
    [ "Incompatible parameter type [6]: Expected `int` for 1st anonymous parameter to call \
       `takes_int` but got `str`." ]


let () =
  "generator"
  >::: [ "check_comprehensions" >:: test_check_comprehensions;
         "check_yield" >:: test_check_generators ]
  |> Test.run
