(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_check_async context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  let assert_strict_type_errors = assert_strict_type_errors ~context in
  assert_type_errors
    {|
      async def foo() -> int: return 1
      def bar() -> None:
        await foo()
    |}
    [];
  assert_type_errors
    {|
      import typing
      def bar(a: typing.Awaitable[int]) -> int:
        return await a
    |}
    [];
  assert_type_errors
    {|
      from builtins import IsAwaitable
      def bar(a: IsAwaitable) -> int:
        await a
        return 0
    |}
    [];
  assert_type_errors
    {|
      from builtins import IsAwaitable
      import typing
      T = typing.TypeVar("T")
      class C(typing.Awaitable[T]): ...

      def foo(c: C) -> int:
        return (await c)
    |}
    ["Invalid type parameters [24]: Generic type `C` expects 1 type parameter."];
  assert_strict_type_errors
    {|
      import typing
      T = typing.TypeVar("T")
      class C(typing.Awaitable[T]): ...

      def foo(c: C) -> int:
        return (await c)
    |}
    ["Invalid type parameters [24]: Generic type `C` expects 1 type parameter."];
  assert_type_errors
    {|
      from builtins import IsAwaitable
      def bar(a: IsAwaitable) -> int:
        return (await a)
    |}
    [];
  assert_type_errors
    {|
      def bar(a: int) -> None:
        await a
    |}
    ["Incompatible awaitable type [12]: Expected an awaitable but got `int`."];
  assert_default_type_errors
    {|
      import typing
      def bar(a: typing.Any) -> None:
        await a
    |}
    [];
  assert_type_errors
    {|
      import typing
      async def read(file: typing.AsyncIterable[str]) -> typing.List[str]:
        return [data async for data in file]
    |}
    [];
  assert_type_errors
    {|
      async def foo() -> None:
        pass
      reveal_type(foo())
    |}
    [
      "Revealed type [-1]: Revealed type for `test.foo()` is `typing.Coroutine[typing.Any, \
       typing.Any, None]`.";
    ];
  assert_type_errors
    {|
      from typing import Generator
      async def foo() -> None:
        def bar() -> Generator[None, None, None]:
          yield
        pass
      reveal_type(foo())
    |}
    [
      "Revealed type [-1]: Revealed type for `test.foo()` is `typing.Coroutine[typing.Any, \
       typing.Any, None]`.";
    ];
  assert_type_errors
    {|
      from typing import Generator
      async def foo() -> None:
        class Inner:
          def bar(self) -> Generator[None, None, None]:
            yield
        pass
      reveal_type(foo())
    |}
    [
      "Revealed type [-1]: Revealed type for `test.foo()` is `typing.Coroutine[typing.Any, \
       typing.Any, None]`.";
    ];
  assert_type_errors
    {|
      import typing
      async def foo() -> typing.AsyncGenerator[bool, None]:
        # not a generator; this gets wrapped in a coroutine
        ...

      reveal_type(foo())
      def bar() -> None:
        async for x in foo():
            pass
    |}
    [
      "Revealed type [-1]: Revealed type for `test.foo()` is "
      ^ "`typing.Coroutine[typing.Any, typing.Any, typing.AsyncGenerator[bool, None]]`.";
      "Undefined attribute [16]: `typing.Coroutine` has no attribute `__aiter__`.";
    ];
  assert_type_errors
    {|
      import typing
      async def foo() -> typing.AsyncGenerator[bool, None]:
        yield True

      reveal_type(foo())
      def bar() -> None:
        async for x in foo():
            pass
    |}
    ["Revealed type [-1]: Revealed type for `test.foo()` is `typing.AsyncGenerator[bool, None]`."];
  assert_type_errors
    {|
      import typing
      async def foo(x: typing.AsyncGenerator[int, None]) -> None:
        async for a in x:
          reveal_type(a)
    |}
    ["Revealed type [-1]: Revealed type for `a` is `int`."];
  assert_type_errors
    {|
      import typing
      class C:
          async def foo(self) -> typing.AsyncGenerator[bool, None]:
              yield True

      reveal_type(C().foo())
      def bar(c: C) -> None:
        async for x in c.foo():
            pass
    |}
    [
      "Revealed type [-1]: Revealed type for `test.C().foo()` is `typing.AsyncGenerator[bool, \
       None]`.";
    ];
  assert_type_errors
    {|
      import typing
      class C:
          async def foo(self) -> typing.AsyncGenerator[bool, None]:
            # not a generator; this gets wrapped in a coroutine
            ...

      reveal_type(C().foo())
      def bar(c: C) -> None:
        async for x in c.foo():
            pass
    |}
    [
      "Revealed type [-1]: Revealed type for `test.C().foo()` is "
      ^ "`typing.Coroutine[typing.Any, typing.Any, typing.AsyncGenerator[bool, None]]`.";
      "Undefined attribute [16]: `typing.Coroutine` has no attribute `__aiter__`.";
    ];
  assert_type_errors
    {|
        import typing
        class A:
            async def f(self) -> typing.AsyncIterator[str]:
                yield "A"
        class B(A):
            async def f(self) -> typing.AsyncIterator[str]:
                yield "B"
        class C(A):
            async def f(self) -> typing.AsyncIterator[int]:
                yield 42
    |}
    [
      "Inconsistent override [15]: `test.C.f` overrides method defined in `A` "
      ^ "inconsistently. Returned type `typing.AsyncIterator[int]` is not a "
      ^ "subtype of the overridden return `typing.AsyncIterator[str]`.";
    ];
  assert_type_errors
    {|
      from typing import AsyncContextManager
      async def bar(async_context_manager: AsyncContextManager[int]) -> int:
        async with async_context_manager:
          return 0
    |}
    [];
  assert_type_errors
    {|
      async def foo() -> int:
        async with 1:
          return 0
    |}
    ["Undefined attribute [16]: `int` has no attribute `__aenter__`."];
  assert_type_errors
    {|
      from typing import Iterable, AsyncGenerator
      async def foo() -> Iterable[str]:
        yield "a"
    |}
    [
      "Incompatible async generator return type [57]: Expected return annotation to be"
      ^ " AsyncGenerator or a superclass "
      ^ "but got `Iterable[str]`.";
      "Incompatible return type [7]: Expected `Iterable[str]` but got `AsyncGenerator[str, \
       typing.Any]`.";
    ];
  assert_type_errors
    {|
      from typing import Iterable, AsyncGenerator
      async def foo() -> object:
        yield "a"
    |}
    [];
  assert_type_errors
    {|
      from typing import Iterable, AsyncGenerator
      async def foo() -> AsyncGenerator[str, str]:
        yield "a"
    |}
    [];
  assert_type_errors
    {|
      from typing import Iterable, AsyncGenerator
      async def foo() -> AsyncGenerator[str, None]:
        yield "a"
    |}
    [];
  assert_type_errors
    {|
      from typing import Iterable, AsyncGenerator
      class MyExtendedAsyncGenerator(AsyncGenerator[int,int]):
        pass
      async def foo() -> MyExtendedAsyncGenerator:
        yield "a"
    |}
    [
      "Incompatible async generator return type [57]: Expected return annotation to be"
      ^ " AsyncGenerator or a superclass "
      ^ "but got `MyExtendedAsyncGenerator`.";
      "Incompatible return type [7]: Expected `MyExtendedAsyncGenerator` but got \
       `AsyncGenerator[str, typing.Any]`.";
    ];
  assert_type_errors
    {|
      from typing import AsyncIterator
      async def foo() -> AsyncIterator[str]:
        yield 1
    |}
    [
      "Incompatible return type [7]: Expected `AsyncIterator[str]` but got `AsyncGenerator[int, \
       typing.Any]`.";
    ];
  assert_type_errors
    {|
      from typing import AsyncIterator
      async def foo() -> AsyncIterator[str]:
        yield ""
    |}
    []


let () = "async" >::: ["check_async" >:: test_check_async] |> Test.run
