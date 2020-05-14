(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Test
open OUnit2
open IntegrationTest

let test_check_nested context =
  assert_default_type_errors
    ~context
    {|
     from typing import Callable, Any

     def wraps(wrapped: Callable[[...], Any]) -> Callable[[...], Any]: ...

     def foo(f: Callable[[int], int]) -> Callable[[int], int]:
       @wraps(f)
       def decorated(x: int) -> int:
         return f(x)
       return decorated
    |}
    [];
  assert_default_type_errors
    ~context
    {|
      from typing import Callable
      def decorator(x: Callable[[int], str]) -> Callable[[str], int]: ...

      def outer() -> None:
          @decorator
          def inner(x: int) -> str:
              return "A"
          reveal_type(inner)
    |}
    ["Revealed type [-1]: Revealed type for `inner` is `typing.Callable[[str], int]`."];
  ()


let test_check_contextmanager context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import typing
      import contextlib
      @contextlib.contextmanager
      def f()->typing.Iterator[int]:
        yield 1

      def g()->int:
        with f() as number:
          return number
    |}
    [];
  assert_type_errors
    {|
      import typing
      import contextlib
      @contextlib.contextmanager
      def f()->typing.Iterator[int]:
        yield 1

      def g()->str:
        with f() as number:
          return number
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      import typing
      import contextlib
      @contextlib.contextmanager
      def f() -> typing.Iterable[int]:
        yield 1

      def g() -> int:
        with f() as number:
          return number
    |}
    [
      (* TODO(T27138096): Iterable should have attribute `__enter__`. *)
      "Undefined attribute [16]: `typing.Iterable` has no attribute `__enter__`.";
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
    ];
  assert_type_errors
    {|
      import typing
      import contextlib
      @contextlib.contextmanager
      def f() -> typing.Generator[int, None, None]:
        yield 1

      def g() -> int:
        with f() as number:
          return number
    |}
    [];

  (* Decorators are chained properly. *)
  assert_type_errors
    {|
      import typing
      import contextlib

      @click.command
      @contextlib.contextmanager
      def f() -> typing.Generator[int, None, None]:
        yield 1
      def g() -> None:
        reveal_type(f)
    |}
    [
      "Revealed type [-1]: Revealed type for `test.f` is \
       `typing.Callable(f)[[Variable(typing.Any), Keywords(typing.Any)], \
       contextlib._GeneratorContextManager[int]]`.";
    ];
  assert_type_errors
    {|
      import typing
      import contextlib
      class C:
        @contextlib.contextmanager
        def f(self) -> typing.Iterator[int]:
          yield 1
      def foo(c: C) -> str:
        with c.f() as manager:
          return manager
        return ""
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      from typing import Iterator
      from contextlib import contextmanager
      @contextmanager
      def f(x: int) -> Iterator[None]:
        yield
      f()
    |}
    ["Missing argument [20]: Call `f` expects argument `x`."]


let test_check_asynccontextmanager context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import typing
      import contextlib
      @contextlib.asynccontextmanager
      async def f() -> typing.AsyncIterator[int]:
        yield 1

      async def g() -> int:
        async with f() as number:
          return number
    |}
    [];
  assert_type_errors
    {|
      import typing
      import contextlib
      @contextlib.asynccontextmanager
      async def f() -> typing.AsyncIterator[int]:
        yield 1

      async def g() -> str:
        async with f() as number:
          return number
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      import typing
      import contextlib
      @contextlib.asynccontextmanager
      async def f() -> typing.AsyncIterable[int]:
        yield 1

      async def g() -> int:
        async with f() as number:
          return number
    |}
    [
      (* TODO(T41786660): AsyncIterable should have attribute `__aenter__` ? *)
      "Incompatible awaitable type [12]: Expected an awaitable but got `unknown`.";
      "Undefined attribute [16]: `typing.AsyncIterable` has no attribute `__aenter__`.";
    ];
  assert_type_errors
    {|
      import typing
      import contextlib
      @contextlib.asynccontextmanager
      async def f() -> typing.AsyncGenerator[int, None]:
        yield 1

      async def g() -> int:
        async with f() as number:
          return number
    |}
    [];
  assert_type_errors
    {|
      import typing
      import contextlib
      class C:
        @contextlib.asynccontextmanager
        async def f(self) -> typing.AsyncIterator[int]:
          yield 1
      async def foo(c: C) -> str:
        async with c.f() as value:
          return value
        return ""
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."];
  assert_type_errors
    {|
      from typing import Iterator
      from contextlib import asynccontextmanager
      @asynccontextmanager
      def f(x: int) -> Iterator[None]:
        yield
      f()
    |}
    ["Missing argument [20]: Call `f` expects argument `x`."]


let test_check_click_command context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_type_errors =
    let update_environment_with =
      [
        {
          handle = "click.pyi";
          (* This is just a mock stub of click and is not meant to be accurate or complete *)
          source =
            {|
            from typing import Any

            def command() -> Any: ...
            def group() -> Any: ...
            def pass_context(f: Any) -> Any: ...
            def pass_obj(f: Any) -> Any: ...
            def option( *param_decls, **attrs) -> Any: ...
            def argument( *param_decls, **attrs) -> Any: ...
            class Context: ...
        |};
        };
      ]
    in
    assert_type_errors ~update_environment_with
  in
  assert_type_errors
    {|
      import click
      @click.command()
      @click.option('--flag', is_flag=True, help='Test flag')
      def main(flag: bool) -> bool:
          return flag

      main()
    |}
    [];
  assert_type_errors
    {|
      import click
      @click.command()
      @click.argument('filename')
      def main(filename: str) -> str:
          return filename

      main()
    |}
    [];
  assert_type_errors
    {|
      import typing
      import click
      def common_params(
        func: typing.Callable[[bool, int], int]
      ) -> typing.Callable[[bool, bool], int]:
          @click.option('--foo', is_flag=True, help='Test flag')
          @click.option('--bar', is_flag=True, help='Another test flag')
          def wrapper(foo: bool, bar: bool) -> int:
              bar_int = 1 if bar else 2
              return func(foo, bar_int)
          return wrapper

      @click.command()
      @common_params
      def main(foo: bool, bar: int) -> int:
          return bar if foo else 0

      main()
    |}
    [];
  assert_type_errors
    {|
      import click

      @click.group()
      @click.pass_context
      def main(ctx: click.Context) -> None:
          pass

      @main.command()
      @click.pass_context
      def run(ctx: click.Context, x: int) -> None:
          pass

      @main.command()
      @click.pass_obj
      def run2(ctx: click.Context) -> None:
          pass

      # Pyre should not raise any errors on the arguments with the presence of the click decorators
      main()
      main(obj={})
      run(1)
      run(x=1)
      run2()
    |}
    (* These errors are filtered in production. *)
    [
      "Undefined attribute [16]: Callable `main` has no attribute `command`.";
      "Undefined attribute [16]: Callable `main` has no attribute `command`.";
    ];

  assert_type_errors
    {|
      def main(flag: bool) -> bool:
          return flag

      main()
    |}
    ["Missing argument [20]: Call `main` expects argument `flag`."]


let test_decorators context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Optional
      def overloaded() -> Optional[int]:
        pass
    |}
    [];
  assert_type_errors
    {|
      import typing
      @typing.overload
      def overloaded() -> int:
        pass
    |}
    [
      "Missing overload implementation [42]: Overloaded function `overloaded` must have an \
       implementation.";
    ];
  assert_type_errors
    {|
      from typing import overload
      @overload
      def overloaded() -> int:
        pass
    |}
    [
      "Missing overload implementation [42]: Overloaded function `overloaded` must have an \
       implementation.";
    ];
  assert_type_errors
    {|
      class Derp:
        @property
        async def get_int(self) -> int:
          return 5

        def test(self) -> int:
          x = await self.get_int
          return x
    |}
    [];
  assert_type_errors
    {|
      from typing import Callable
      def my_decorator(f: Callable[[int], int]) -> Callable[[int], int]:
        return f
      @my_decorator
      def f(x: int) -> int:
        return x
    |}
    [];
  assert_type_errors
    {|
      @my_decorator
      def f(x: int) -> int:
        return x
    |}
    [
      "Undefined name [18]: Global name `my_decorator` is not defined, or there is at least one \
       control flow path that doesn't define `my_decorator`.";
    ];
  assert_type_errors
    {|
      from typing import Any
      def my_decorator(x: int) -> Any: ...
      @my_decorator(1)
      def f(x: int) -> int:
        return x
    |}
    ["Missing return annotation [3]: Return type must be specified as type other than `Any`."];
  assert_type_errors
    {|
      from typing import Any
      def my_decorator(x: int) -> Any: ...
      @my_decorator(1 + "foo")
      def f(x: int) -> int:
        return x
    |}
    [
      "Missing return annotation [3]: Return type must be specified as type other than `Any`.";
      "Incompatible parameter type [6]: Expected `int` for 1st positional only "
      ^ "parameter to call `int.__add__` but got `str`.";
    ];

  (* We only apply the implementation. I'm not sure why this is our strategy *)
  assert_type_errors
    {|
      from typing import overload, Callable
      @overload
      def overloaded_decorator(f: Callable[[int], int]) -> Callable[[int], int]: ...
      def overloaded_decorator(f: Callable[[int], float]) -> Callable[[int], object]: ...

      @overloaded_decorator
      def foo(x: int) -> int:
        return x

      reveal_type(foo)
    |}
    ["Revealed type [-1]: Revealed type for `test.foo` is `typing.Callable(foo)[[int], object]`."];
  ()


let test_check_user_decorators context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      import typing
      def decorate(f: typing.Callable[[int], str]) -> typing.Callable[[str], int]:
        ...
      @decorate
      def f(x: int) -> str:
        return str(x)
      reveal_type(f)
    |}
    ["Revealed type [-1]: Revealed type for `test.f` is `typing.Callable(f)[[str], int]`."];

  (* We currently ignore decorating decorators. *)
  assert_type_errors
    {|
      import typing
      meta_type = typing.Callable[[typing.Callable[[int], int]], typing.Callable[[str], str]]
      def meta_decorate(f: typing.Any) -> meta_type:
        ...
      @meta_decorate
      def decorate(f: typing.Callable[[int], str]) -> typing.Callable[[str], int]:
        ...

      @decorate
      def f(x: int) -> str:
        return str(x)
      reveal_type(f)
    |}
    [
      "Missing parameter annotation [2]: Parameter `f` must have a type other than `Any`.";
      "Revealed type [-1]: Revealed type for `test.f` is `typing.Callable(f)[[str], int]`.";
    ];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T")

      # lets AstLintRule ignore these no_op implementations
      def decorate(f: typing.Callable[['C', T], None]) -> typing.Callable[['C', T], None]:
        ...

      class C:
        @decorate
        def f(self, x: int) -> None: # registered type is typing.Callable[[C, int], None]
          pass

      class D(C):
        def f(self, y: int) -> None:
          pass
      reveal_type(C.f)
      reveal_type(D.f)
    |}
    [
      "Revealed type [-1]: Revealed type for `test.C.f` is `typing.Callable(C.f)[[C, int], None]`.";
      "Revealed type [-1]: Revealed type for `test.D.f` is `typing.Callable(D.f)[[Named(self, D), \
       Named(y, int)], None]`.";
    ];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T")
      def synchronize(
        coroutine: typing.Callable[..., typing.Coroutine[typing.Any, typing.Any, T]]
      ) -> typing.Callable[..., T]: ...

      @synchronize
      async def am_i_async(x: int) -> str:
        return str(x)

      reveal_type(am_i_async)
    |}
    [
      "Missing parameter annotation [2]: Parameter `coroutine` must have a type that does not \
       contain `Any`.";
      "Revealed type [-1]: Revealed type for `test.am_i_async` is \
       `typing.Callable(am_i_async)[..., str]`.";
    ];
  assert_type_errors
    {|
      from typing import Callable
      def not_a_decorator(x: int) -> str: ...

      @not_a_decorator
      def function_returning_callable() -> Callable[[int], str]:
       ...

      reveal_type(function_returning_callable)
    |}
    [
      "Revealed type [-1]: Revealed type for `test.function_returning_callable` is \
       `typing.Callable(function_returning_callable)[[], typing.Callable[[int], str]]`.";
    ];
  ()


let test_check_callable_class_decorators context =
  let assert_type_errors = assert_type_errors ~context in
  (* This should not work because that's a __call__ on the *instance* not the class. In principle we
     could support metaclass __call__ methods, but we're not now *)
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T")
      class synchronize:
        def __call__(
           self,
           coroutine: typing.Callable[..., typing.Coroutine[typing.Any, typing.Any, T]]
        ) -> typing.Callable[..., T]: ...

      @synchronize
      async def am_i_async(x: int) -> str:
        return str(x)
      reveal_type(am_i_async)
    |}
    [
      "Missing parameter annotation [2]: Parameter `coroutine` must have a type that does not \
       contain `Any`.";
      "Revealed type [-1]: Revealed type for `test.am_i_async` is \
       `typing.Callable(am_i_async)[[Named(x, int)], typing.Coroutine[typing.Any, typing.Any, \
       str]]`.";
    ];

  (* We don't support overloaded callable classes either. *)
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar("T")
      class synchronize:
        @typing.overload
        def __call__(
           self,
           coroutine: typing.Callable[..., typing.Coroutine[typing.Any, typing.Any, T]]
        ) -> typing.Callable[..., T]: ...
        @typing.overload
        def __call__(self, coroutine: int) -> int: ...
        def __call__(self, coroutine: typing.Any) -> typing.Any: ...
      @synchronize
      async def am_i_async(x: int) -> str:
        return str(x)
      reveal_type(am_i_async)
    |}
    [
      "Missing parameter annotation [2]: Parameter `coroutine` must have a type other than `Any`.";
      "Missing return annotation [3]: Return type must be specified as type other than `Any`.";
      "Revealed type [-1]: Revealed type for `test.am_i_async` is \
       `typing.Callable(am_i_async)[[Named(x, int)], typing.Coroutine[typing.Any, typing.Any, \
       str]]`.";
    ]


let test_decorator_factories context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
     from typing import Callable

     def decorator_factory() -> Callable[[Callable[[str], int]], Callable[[], str]]:
         def decorator(func: Callable[[str], int]) -> Callable[[], str]:
             def inner() -> str:
                 return str(func("foo"))
             return inner
         return decorator

     @decorator_factory()
     def foo(name: str) -> int:
         return len(name)

     reveal_type(foo)
    |}
    ["Revealed type [-1]: Revealed type for `test.foo` is `typing.Callable(foo)[[], str]`."];
  assert_type_errors
    {|
     from typing import Callable

     def decorator_factory(name: str) -> Callable[[Callable[[str], int]], Callable[[], str]]: ...

     @decorator_factory("literal")
     def foo(name: str) -> int:
         return len(name)

     reveal_type(foo)
    |}
    ["Revealed type [-1]: Revealed type for `test.foo` is `typing.Callable(foo)[[], str]`."];
  assert_type_errors
    {|
     from typing import Callable

     def decorator_factory(name: str) -> Callable[[Callable[[str], int]], Callable[[], str]]: ...

     @decorator_factory(name="literal")
     def foo(name: str) -> int:
         return len(name)

     reveal_type(foo)
    |}
    ["Revealed type [-1]: Revealed type for `test.foo` is `typing.Callable(foo)[[], str]`."];
  assert_type_errors
    {|
     from typing import Callable

     def decorator_factory(index: int) -> Callable[[Callable[[str], int]], Callable[[], str]]: ...

     @decorator_factory(3 + 4)
     def foo(name: str) -> int:
         return len(name)

     reveal_type(foo)
    |}
    [
      (* We don't handle non-literal expressions as arguments *)
      "Revealed type [-1]: Revealed type for `test.foo` is `typing.Callable(foo)[[Named(name, \
       str)], int]`.";
    ];
  assert_type_errors
    {|
     from typing import Callable

     def decorator_factory(index: int) -> Callable[[Callable[[str], int]], Callable[[], str]]: ...

     global_value = 3

     @decorator_factory(global_value)
     def foo(name: str) -> int:
         return len(name)

     reveal_type(foo)
    |}
    [
      (* We don't handle globals as arguments *)
      "Revealed type [-1]: Revealed type for `test.foo` is `typing.Callable(foo)[[Named(name, \
       str)], int]`.";
    ];
  ()


let () =
  "decorator"
  >::: [
         "check_nested" >:: test_check_nested;
         "check_contextmanager" >:: test_check_contextmanager;
         "check_asynccontextmanager" >:: test_check_asynccontextmanager;
         "check_click_command" >:: test_check_click_command;
         "check_user_decorators" >:: test_check_user_decorators;
         "check_callable_class_decorators" >:: test_check_callable_class_decorators;
         "decorators" >:: test_decorators;
         "decorator_factories" >:: test_decorator_factories;
       ]
  |> Test.run
