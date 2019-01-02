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
    "decorators">::test_decorators;
  ]
  |> Test.run
