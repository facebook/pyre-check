(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


open OUnit2
open IntegrationTest


let test_check_contextmanager _ =
  assert_type_errors
    {|
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
      @contextlib.contextmanager
      def f() -> typing.Iterable[int]:
        yield 1

      def g() -> int:
        with f() as number:
          return number
    |}
    [
      (* TODO(T27138096): Iterable should have attribute `__enter__`. *)
      "Undefined attribute [16]: `typing.Iterable[typing.Any]` has no attribute `__enter__`.";
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
    ];

  assert_type_errors
    {|
      @contextlib.contextmanager
      def f() -> typing.Generator[int, None, None]:
        yield 1

      def g() -> int:
        with f() as number:
          return number
    |}
    [];

  assert_type_errors
    {|
      class C:
        @contextlib.contextmanager
        def f(self) -> typing.Iterator[int]:
          yield 1
      def foo(c: C) -> str:
        with c.f() as manager:
          return manager
        return ""
    |}
    ["Incompatible return type [7]: Expected `str` but got `int`."]

let test_check_click_command _ =
  assert_type_errors
    {|
      @click.command()
      @click.option('--flag', is_flag=True, help='Test flag')
      def main(flag: bool) -> bool:
          return flag

      main()
    |}
    [];

  assert_type_errors
    {|
      @click.command()
      @click.argument('filename')
      def main(filename: str) -> str:
          return filename

      main()
    |}
    [];

  assert_type_errors
    {|
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
      class Context: ...  # Emulate click.Context

      @click.group()
      @click.pass_context
      def main(ctx: Context) -> None:
          pass

      @test.command()
      @click.pass_context
      def run(ctx: Context, x: int) -> None:
          pass

      @test.command()
      @click.pass_obj
      def run2(ctx: Context) -> None:
          pass

      # Pyre should not raise any errors on the arguments with the presence of the click decorators
      main()
      main(obj={})
      run(1)
      run(x=1)
      run2()
    |}
    [];

  assert_type_errors
    {|
      def main(flag: bool) -> bool:
          return flag

      main()
    |}
    ["Missing argument [20]: Call `main` expects argument `flag`."]


let test_decorators _ =
  assert_type_errors
    {|
      @typing.overload
      def overloaded()->int:
        pass
    |}
    [];

  assert_type_errors
    {|
      from typing import overload
      @overload
      def overloaded()->int:
        pass
    |}
    [];

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
    []


let () =
  "decorator">:::[
    "check_contextmanager">::test_check_contextmanager;
    "check_click_command">::test_check_click_command;
    "decorators">::test_decorators;
  ]
  |> Test.run
