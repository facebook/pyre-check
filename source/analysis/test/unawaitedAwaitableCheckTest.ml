(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Test

let assert_awaitable_errors source errors context =
  let check ~environment ~source =
    let type_environment = TypeEnvironment.read_only environment in
    let global_resolution =
      type_environment |> TypeEnvironment.ReadOnly.global_environment |> GlobalResolution.create
    in
    UnawaitedAwaitableCheck.check_module_TESTING_ONLY
      ~resolution:
        (TypeCheck.resolution
           global_resolution
           (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
           (module TypeCheck.DummyContext))
      ~local_annotations_for_define:
        (TypeEnvironment.ReadOnly.get_or_recompute_local_annotations type_environment)
      source
  in
  assert_errors ~check source errors context


let test_forward context =
  (* Test that a non-async `def` returning `Awaitable[int]` is checked correctly. *)
  assert_awaitable_errors
    {|
      from typing import Awaitable

      def awaitable() -> Awaitable[int]: ...

      async def main() -> None:
        unawaited = awaitable()
    |}
    ["Unawaited awaitable [1001]: Awaitable assigned to `unawaited` is never awaited."]
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def main() -> None:
        unawaited = awaitable()
    |}
    ["Unawaited awaitable [1001]: Awaitable assigned to `unawaited` is never awaited."]
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def main() -> None:
        awaited = awaitable()
        await awaited
    |}
    []
    context;
  assert_awaitable_errors
    {|
      from asyncio.futures import Future

      def future() -> Future[int]: ...

      async def main() -> None:
        unawaited = future()
    |}
    ["Unawaited awaitable [1001]: Awaitable assigned to `unawaited` is never awaited."]
    context;
  assert_awaitable_errors
    {|
      from asyncio.futures import Future

      def future() -> Future[int]: ...

      async def main() -> None:
        awaited = future()
        await awaited
    |}
    []
    context;

  (* Assert. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      awaited = awaitable()
      _ = await awaited
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      awaited = awaitable()
      assert (await awaited)
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def main() -> None:
        awaited = awaitable()
        assert awaited
    |}
    ["Unawaited awaitable [1001]: Awaitable assigned to `awaited` is never awaited."]
    context;

  (* Delete. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      awaited = awaitable()
      if "moon_is_high":
        del (await awaited)
    |}
    []
    context;

  (* Raise. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> Exception: ...

      awaited = awaitable()
      raise (await awaited)
    |}
    []
    context;

  (* Return. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def meta_awaitable() -> int:
        awaited = awaitable()
        return awaited
    |}
    []
    context;

  (* Yield. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...
      def meta_awaitable():
        awaited = awaitable()
        yield awaited
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...
      def meta_awaitable():
        awaited = awaitable()
        yield (await awaited)
    |}
    []
    context;

  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...
      def meta_awaitable():
        awaited = awaitable()
        yield from (await awaited)
    |}
    []
    context;

  (* Tuples. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...
      def meta_awaitable():
        awaited = awaitable()
        yield (await awaited, 3)
    |}
    []
    context;

  (* Any is not an awaitable. *)
  assert_awaitable_errors
    {|
    from typing import Any

    def returns_any() -> Any: ...

    async def foo() -> None:
      x = returns_any()
      return
    |}
    []
    context;
  assert_awaitable_errors
    {|
    from typing import Any

    async def foo(param: Any) -> None:
      return
    |}
    []
    context;

  (* Boolean operators. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...
      def meta_awaitable():
        awaited = awaitable()
        await awaited or Exception("You must await.")
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...
      def meta_awaitable():
        awaited = awaitable()
        1 and (2 and (await awaited))
    |}
    []
    context;

  (* We view parameters which flow into a call as having been awaited. *)
  assert_awaitable_errors
    {|
      from typing import Awaitable

      async def awaitable() -> int: ...

      async def takes_awaitable(x: Awaitable[int]): ...

      def meta_awaitable():
        awaited = awaitable()
        await takes_awaitable(awaited)
    |}
    []
    context;
  assert_awaitable_errors
    {|
      from typing import Awaitable

      async def awaitable() -> int: ...

      async def takes_awaitable(x: Awaitable[int]) -> None: ...

      def meta_awaitable():
        await takes_awaitable(awaitable())
    |}
    []
    context;
  assert_awaitable_errors
    {|
      from typing import Awaitable

      async def awaitable() -> int: ...

      async def takes_awaitable(x: Awaitable[int]) -> None: ...

      def meta_awaitable():
        await takes_awaitable({ "a": awaitable(), "b": awaitable()})
    |}
    []
    context;

  (* Comparison operators. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def meta_awaitable():
        awaited = awaitable()
        return (await awaited) > 2
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def meta_awaitable() -> bool:
        awaited = awaitable()
        return 0 == (await awaited)
    |}
    []
    context;

  (* Container literals. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def meta_awaitable():
        awaited = awaitable()
        return [1, await awaited, 2]
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def meta_awaitable():
        awaited = awaitable()
        return {1, await awaited, 2}
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def meta_awaitable():
        awaited = awaitable()
        return {await awaited: 1, 2: 2}
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def meta_awaitable():
        awaited = awaitable()
        return {"foo": [await awaited]}
    |}
    []
    context;

  (* Lambdas. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def meta_awaitable():
        awaited = awaitable()
        lambda x: (await awaited) or 42
    |}
    []
    context;

  (* Starred expressions. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Iterable[int]: ...

      async def meta_awaitable():
        awaited = awaitable()
        [1, *(await awaited)]
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Dict[int, str]: ...

      async def meta_awaitable():
        awaited = awaitable()
        {1: "x", **(await awaited)}
    |}
    []
    context;

  (* Unary operators. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      awaited = awaitable()
      -(not (await awaited))
    |}
    []
    context;

  (* Yield. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      awaited = awaitable()
      yield (await awaited) if 1 > 2 else False
    |}
    []
    context;

  (* Comprehensions. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      awaited = awaitable()
      [i for i in await awaited]
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      awaited = awaitable()
      [(await awaited) for i in [1, 2, 3]]
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      awaited = awaitable()
      {(await awaited) for i in [1, 2, 3]}
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      awaited = awaitable()
      {i: (await awaited) for i in [1, 2, 3]}
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      awaited = awaitable()
      ((await awaited) for i in [1, 2, 3])
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def other_awaitable() -> int: ...

      async def main() -> None:
        unawaited = awaitable()
        other_unawaited = other_awaitable()
        if True > False:
          unawaited = other_unawaited
    |}
    [
      "Unawaited awaitable [1001]: Awaitable assigned to `unawaited` is never awaited.";
      "Unawaited awaitable [1001]: Awaitable assigned to `unawaited`, `other_unawaited` is never \
       awaited.";
    ]
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def other_awaitable() -> int: ...

      async def main() -> None:
        unawaited = awaitable()
        other_unawaited = other_awaitable()
        unawaited = other_unawaited
        await unawaited
    |}
    ["Unawaited awaitable [1001]: `test.awaitable()` is never awaited."]
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def other_awaitable() -> int: ...

      async def main() -> None:
        unawaited = awaitable()
        other_unawaited = other_awaitable()
        if 1 > 2:
          unawaited = other_unawaited
        await unawaited
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def other_awaitable() -> int: ...

      async def main() -> None:
        unawaited = awaitable()
        other_unawaited = other_awaitable()
        if 1 > 2:
          unawaited = other_unawaited
        await other_unawaited
    |}
    ["Unawaited awaitable [1001]: Awaitable assigned to `unawaited` is never awaited."]
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def foo():
        awaitable()
    |}
    ["Unawaited awaitable [1001]: `test.awaitable()` is never awaited."]
    context;

  (* Ensure that we don't crash when attempting to await a non-simple name. *)
  assert_awaitable_errors
    {|
      class C:
        async def awaitable(self) -> int: ...

      async def foo():
        await C().awaitable()
    |}
    []
    context;
  assert_awaitable_errors
    {|
       import asyncio

       async def awaitable() -> typing.Tuple[int, int]: ...

       async def foo() -> int:
         a = awaitable()
         b = awaitable()
         _, c = await asyncio.gather(a, b)
    |}
    []
    context;
  assert_awaitable_errors
    {|
       import asyncio

       async def awaitable() -> typing.Tuple[int, int]: ...

       async def foo() -> int:
         a = awaitable()
         b = awaitable()
         _, c = await asyncio.gather(a, b)
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...
      async def foo():
        a, b = awaitable(), awaitable()
        await a
    |}
    ["Unawaited awaitable [1001]: Awaitable assigned to `b` is never awaited."]
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...
      async def foo():
        a, b = awaitable(), awaitable()
        await b
    |}
    ["Unawaited awaitable [1001]: Awaitable assigned to `a` is never awaited."]
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...
      async def foo():
        [a, (b, [c, d], e)] = (awaitable(), (awaitable(), (awaitable(), awaitable()), awaitable()))
    |}
    [
      "Unawaited awaitable [1001]: Awaitable assigned to `a` is never awaited.";
      "Unawaited awaitable [1001]: Awaitable assigned to `b` is never awaited.";
      "Unawaited awaitable [1001]: Awaitable assigned to `c` is never awaited.";
      "Unawaited awaitable [1001]: Awaitable assigned to `d` is never awaited.";
      "Unawaited awaitable [1001]: Awaitable assigned to `e` is never awaited.";
    ]
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def foo():
        a, *b, c = awaitable(), awaitable(), awaitable()
        await a
    |}
    [
      "Unawaited awaitable [1001]: `test.awaitable()` is never awaited.";
      "Unawaited awaitable [1001]: Awaitable assigned to `c` is never awaited.";
    ]
    context;

  (* We don't validate that every expression in a starred one is awaited. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def foo():
        a, *b, c = awaitable(), awaitable(), awaitable()
        await asyncio.gather(a, c)
    |}
    ["Unawaited awaitable [1001]: `test.awaitable()` is never awaited."]
    context;

  assert_awaitable_errors
    {|
      from typing import Awaitable

      async def awaitable() -> int: ...

      def meta_awaitable() -> typing.Tuple[Awaitable[int], int]:
        awaited = awaitable()
        return awaited, 1
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def foo() -> None:
        d = {
          awaitable(): 2,
          3: awaitable(),
        }
    |}
    [
      "Unawaited awaitable [1001]: Awaitable assigned to `d` is never awaited.";
      "Unawaited awaitable [1001]: Awaitable assigned to `d` is never awaited.";
    ]
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def foo() -> None:
        d = {
          awaitable(): 2,
          3: awaitable(),
        }
        await d
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def foo() -> None:
        l = [1, {2: awaitable()}]
    |}
    ["Unawaited awaitable [1001]: Awaitable assigned to `l` is never awaited."]
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def foo() -> None:
        l = [1, {2: awaitable()}]
        await l
    |}
    []
    context;
  assert_awaitable_errors
    {|
      import asyncio

      async def awaitable() -> int: ...

      async def foo() -> None:
        l = [awaitable(), awaitable()]
        await asyncio.gather( *l)
    |}
    []
    context;
  assert_awaitable_errors
    {|
      import asyncio

      async def awaitable() -> int: ...

      async def foo() -> None:
        l = [awaitable(), awaitable()]
        await asyncio.gather(awaitable(), *l)
    |}
    []
    context;
  assert_awaitable_errors
    {|
      import asyncio

      async def awaitable() -> int: ...

      async def foo() -> None:
        l = [awaitable(), awaitable()]
        await asyncio.gather(l if l is not None else awaitable())
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def foo() -> None:
        [] + [awaitable()]
    |}
    ["Unawaited awaitable [1001]: `test.awaitable()` is never awaited."]
    context;

  (* Multiple assignment targets. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...
      def meta_awaitable():
        x = y = await awaitable()
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...
      async def meta_awaitable():
        x = y = awaitable()
        a = b = c = awaitable()
    |}
    [
      "Unawaited awaitable [1001]: Awaitable assigned to `y`, `x` is never awaited.";
      "Unawaited awaitable [1001]: Awaitable assigned to `c`, `b`, `a` is never awaited.";
    ]
    context;

  (* Walrus operator. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...
      async def meta_awaitable():
        x = y := await awaitable()
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...
      async def meta_awaitable():
        x = (y := awaitable())
    |}
    (* TODO(T53600647): Mention y in the error message. *)
    ["Unawaited awaitable [1001]: Awaitable assigned to `x` is never awaited."]
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...
      async def meta_awaitable():
        if y := await awaitable():
          pass
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...
      async def meta_awaitable():
        if y := awaitable():
          pass
    |}
    (* TODO(T53600647): Mention y in the error message. *)
    ["Unawaited awaitable [1001]: `test.awaitable()` is never awaited."]
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> bool: ...

      async def foo():
        if awaitable():
          pass
    |}
    ["Unawaited awaitable [1001]: `test.awaitable()` is never awaited."]
    context;
  ()


let test_initial context =
  (* Technically, just defining this function will not cause a `RuntimeWarning`.

     However, calling this function *will* cause a `RuntimeWarning`.

     So, our emitting an unawaited-awaitable error in the function definition is the same as our
     emitting an incompatible-variable error for a function where we assign `x: str = 1`. As a
     static type checker, we have to be conservative. *)
  (* TODO(T140344232): Emit error when an awaitable parameter is not awaited. *)
  assert_awaitable_errors
    {|
      from typing import Awaitable

      async def awaitable(x: Awaitable[int]) -> int:
        return 0
    |}
    []
    context;
  assert_awaitable_errors
    {|
      from typing import Awaitable

      async def awaitable(x: Awaitable[int]) -> int:
        return (await x)
    |}
    []
    context;
  assert_awaitable_errors
    {|
      from typing import Awaitable

      async def awaitable( *x: Awaitable[int]) -> int:
        return 0
    |}
    []
    context;
  assert_awaitable_errors
    {|
      import asyncio
      from typing import Awaitable

      async def awaitable( *x: Awaitable[int]) -> int:
        value, *_others = asyncio.gather( *x)
        return value
    |}
    []
    context;
  assert_awaitable_errors
    {|
      from typing import Awaitable

      async def awaitable( **x: Awaitable[int]) -> int:
        return 0
    |}
    []
    context;
  assert_awaitable_errors
    {|
      import asyncio
      from typing import Awaitable, Iterable

      def await_list(x: Iterable[Awaitable[int]]]) -> Iterable[int]: ...

      async def awaitable( **d: Awaitable[int]) -> int:
        value, *_others = await_list(d.values())
        return value
    |}
    []
    context;
  ()


let test_state context =
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def main() -> None:
        if True:
          unawaited = awaitable()
    |}
    ["Unawaited awaitable [1001]: Awaitable assigned to `unawaited` is never awaited."]
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def main() -> None:
        unawaited = awaitable()
        if True:
          await unawaited
    |}
    []
    context;
  ()


let ( >>: ) test_name test_function = test_name >:: fun context -> test_function context

let test_method_call =
  [
    "don't await non-async method of non-awaitable - should not error"
    >>: assert_awaitable_errors
          {|
            def non_async() -> int: ...

            async def foo() -> None:
              non_async()
          |}
          [];
    "await the async method of a non-awaitable object"
    >>: assert_awaitable_errors
          {|
            class C:
              async def foo() -> int: ...

            async def foo(c: C, c2: C) -> None:
              await c.foo()
              c2.foo()
           |}
          ["Unawaited awaitable [1001]: `c2.foo()` is never awaited."];
    "await an unawaited instance of an awaitable class"
    >>: assert_awaitable_errors
          {|
            from typing import Awaitable

            class C(Awaitable[int]): ...

            def awaitable() -> C: ...

            async def foo() -> None:
              await awaitable()
          |}
          [];
    "don't await an unawaited instance of an awaitable class - should not emit error"
    >>: assert_awaitable_errors
          {|
            from typing import Awaitable

            class C(Awaitable[int]): ...

            def awaitable() -> C: ...

            async def foo() -> None:
              awaitable()
          |}
          [];
    "await the async method of a variable having an unawaited awaitable object"
    >>: assert_awaitable_errors
          {|
            from typing import Awaitable

            class C(Awaitable[int]):
              async def method(self) -> int: ...

            def awaitable() -> C: ...

            async def foo() -> None:
              unawaited = awaitable()
              await unawaited.method()
          |}
          [];
    "don't await the async method of a variable having an unawaited awaitable object"
    >>: assert_awaitable_errors
          {|
            from typing import Awaitable

            class C(Awaitable[int]):
              async def method(self) -> int: ...

            def awaitable() -> C: ...

            async def foo() -> None:
              unawaited = awaitable()
              unawaited.method()
          |}
          ["Unawaited awaitable [1001]: `unawaited.method()` is never awaited."];
    "await chained async method of unawaited awaitable object"
    >>: assert_awaitable_errors
          {|
            from typing import Awaitable

            class C(Awaitable[int]):
              def method(self) -> C: ...

              async def other(self) -> int: ...

            def awaitable() -> C: ...

            async def foo() -> None:
              unawaited = awaitable()
              await unawaited.method().other()
          |}
          [];
    "don't await the chained async method of unawaited awaitable object"
    >>: assert_awaitable_errors
          {|
            from typing import Awaitable

            class C(Awaitable[int]):
              def method(self) -> C: ...

              async def other(self) -> int: ...

            def awaitable() -> C: ...

            async def foo() -> None:
              unawaited = awaitable()
              unawaited.method().other()
          |}
          ["Unawaited awaitable [1001]: `unawaited.method().other()` is never awaited."];
    "await async method chain, part of which was assigned to a variable"
    >>: assert_awaitable_errors
          {|
            from typing import Awaitable

            class C(Awaitable[int]):
              def method(self) -> C: ...
              async def other(self) -> int: ...

            def awaitable() -> C: ...

            async def foo() -> None:
              unawaited = awaitable().method()
              await unawaited.other()
          |}
          [];
    "don't await an async method chain, part of which was assigned to a variable"
    >>: assert_awaitable_errors
          {|
            from typing import Awaitable

            class C(Awaitable[int]):
              def method(self) -> C: ...
              async def other(self) -> int: ...

            def awaitable() -> C: ...

            async def foo() -> None:
              unawaited = awaitable().method()
              unawaited.other()
          |}
          ["Unawaited awaitable [1001]: `unawaited.other()` is never awaited."];
    "If we can't resolve the type of the method as being an awaitable, be unsound and assume the \
     method awaits the awaitable."
    >>: assert_awaitable_errors
          {|
            async def awaitable() -> int: ...

            async def foo() -> None:
              await awaitable().method()
          |}
          [];
    "don't await method of unknown type on an awaitable"
    >>: assert_awaitable_errors
          {|
            async def awaitable() -> int: ...

            async def foo() -> None:
              awaitable().method()
          |}
          ["Unawaited awaitable [1001]: `test.awaitable()` is never awaited."];
    "don't await async method on an awaited awaitable - should emit error"
    >>: assert_awaitable_errors
          {|
            class Foo:
              async def foo(self) -> "Foo":
                return Foo()

              async def async_get_bool(self) -> bool:
                ...

            async def main() -> None:
              foo = await Foo().foo()

              if foo.async_get_bool():
                pass
          |}
          ["Unawaited awaitable [1001]: `foo.async_get_bool()` is never awaited."];
    "don't await async method on an awaitable that is awaited later - don't emit error"
    (* It's fine to not emit an error because `foo.async_get_bool()` will cause a type error
       anyway. *)
    >>: assert_awaitable_errors
          {|
            class Foo:
              async def foo(self) -> "Foo":
                return Foo()

              async def async_get_bool(self) -> bool:
                ...

            async def main() -> None:
              foo = Foo().foo()

              if foo.async_get_bool():
                pass

              await foo
          |}
          [];
    "don't await async method on the target of `async with` - should emit error"
    >>: assert_awaitable_errors
          {|
            from contextlib import asynccontextmanager
            from typing import AsyncGenerator

            class Bar:
              async def async_method(self) -> None: ...

            @asynccontextmanager
            async def get_bar() -> AsyncGenerator[Bar, None]:
              yield Bar()

            async def main() -> None:
              async with get_bar() as bar:
                bar.async_method()

              async with get_bar() as bar2:
                await bar2.async_method()
          |}
          ["Unawaited awaitable [1001]: `bar.async_method()` is never awaited."];
  ]


let test_aliases context =
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def foo() -> None:
        x = awaitable()
        x = 42
    |}
    ["Unawaited awaitable [1001]: Awaitable assigned to `x` is never awaited."]
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def foo() -> None:
        a = [awaitable()]
        b = [1]
        c = a + b
        await c
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def foo() -> None:
        a = [awaitable()]
        b = [1]
        c = a + b
    |}
    ["Unawaited awaitable [1001]: Awaitable assigned to `c`, `a` is never awaited."]
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def foo() -> None:
        a = [1]
        b = [awaitable()]
        c = a + b
        await c
    |}
    []
    context;
  assert_awaitable_errors
    {|
      from typing import Awaitable

      async def awaitable() -> int: ...

      class C:
        x: Awaitable[int] = ...

        def my_method(self) -> None:
          self.x = awaitable()
    |}
    []
    context;
  ()


let test_return context =
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      def foo() -> int:
        return awaitable()
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      def foo() -> int:
        x = [awaitable()]
        y = [awaitable()]
        return (x + y)
    |}
    []
    context;
  ()


let test_assign context =
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def foo() -> int:
        d = {}
        d["bar"] = awaitable()
        d["foo"] = awaitable()
    |}
    [
      "Unawaited awaitable [1001]: Awaitable assigned to `d` is never awaited.";
      "Unawaited awaitable [1001]: Awaitable assigned to `d` is never awaited.";
    ]
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def foo() -> int:
        d = {}
        d["bar"] = awaitable()
        d["foo"] = awaitable()
        await d
    |}
    []
    context;
  assert_awaitable_errors
    {|
      import asyncio

      async def awaitable() -> int: ...

      async def foo() -> int:
        d = {}
        d["bar"] = awaitable()
        d["foo"] = "not awaitable"
        await asyncio.gather( *d.values())
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def foo() -> int:
        return 0
      async def bar() -> None:
        async with foo():
          return
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def foo() -> int:
        return 0
      async def bar() -> None:
        with foo():
          return
    |}
    ["Unawaited awaitable [1001]: `test.foo()` is never awaited."]
    context;
  ()


let test_globals context =
  assert_awaitable_errors
    {|
      from typing import Awaitable

      MY_GLOBAL: Awaitable[int] = ...

      async def awaitable() -> Awaitable[int]: ...

      async def foo() -> int:
        MY_GLOBAL = awaitable()
    |}
    ["Unawaited awaitable [1001]: Awaitable assigned to `MY_GLOBAL` is never awaited."]
    context;
  ()


let test_if context =
  (* TODO(T79853064): This should emit an error about the awaitable sometimes not being awaited. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      def foo(b: bool) -> None:
        unawaited = awaitable()

        if b:
          await unawaited
    |}
    []
    context;
  ()


let test_pass_to_callee context =
  assert_awaitable_errors
    {|
      from typing import Awaitable

      def awaitable() -> Awaitable[int]: ...

      def expect_awaitable(x: Awaitable[int]) -> None: ...

      def foo(b: bool) -> None:
        unawaited = awaitable()
        expect_awaitable(unawaited)
    |}
    []
    context;
  ()


let test_placeholder_stubs context =
  assert_awaitable_errors
    {|
      from placeholder_stub import StubbedBase

      class Foo(StubbedBase): ...

      def main() -> None:
          x = Foo()
    |}
    []
    context;
  (* TODO(T79853064): To be consistent with our existing behavior, this should emit an error about
     `x` not being awaited. However, because we treat any class extending a placeholder class as a
     non-awaitable, we don't emit an error here. *)
  assert_awaitable_errors
    {|
      from placeholder_stub import StubbedBase
      from typing import Awaitable

      class Foo(StubbedBase, Awaitable[str]): ...

      def main() -> None:
          x = Foo()
    |}
    []
    context;
  ()


let test_getattr context =
  assert_awaitable_errors
    {|
      from typing import Any

      class Foo:
        def __getattr__(self, name: str) -> Any: ...

      def main() -> None:
        x = Foo()
    |}
    []
    context;
  (* TODO(T79853064): Ideally, this should emit an error about `x` not being awaited. However,
     because we treat any class with `__getattr__` as non-awaitable, we don't emit an error here. *)
  assert_awaitable_errors
    {|
      from typing import Any, Awaitable

      class Foo():
        def __await__(self) -> Generator[Any, None, str]: ...

        def __getattr__(self, name: str) -> Any: ...

      def main() -> None:
          x = Foo()
    |}
    []
    context;
  assert_awaitable_errors
    {|
      from typing import Any

      class Foo:
        def __getattr__(self, name: str) -> Any: ...

        def some_method(self) -> None:
          x = type(self)
    |}
    []
    context;
  ()


let test_attribute_assignment context =
  (* We don't error on methods for classes that are awaitable themselves. *)
  assert_awaitable_errors
    {|
      from typing import Awaitable

      async def awaitable() -> int: ...

      class C(Awaitable[int]):
        def __init__(self) -> None:
          self.x = awaitable()
    |}
    []
    context;
  assert_awaitable_errors
    {|
      from typing import Awaitable

      async def awaitable() -> int: ...

      class C(Awaitable[int]):
        pass

      class D(C):
        def __init__(self) -> None:
          self.x = awaitable()
    |}
    []
    context;
  assert_awaitable_errors
    {|
      from typing import Awaitable, Generic, TypeVar

      T = TypeVar("T")

      async def awaitable() -> int: ...

      class C(Awaitable[T], Generic[T):
        def __init__(self) -> None:
          self.x = awaitable()
    |}
    []
    context;
  assert_awaitable_errors
    {|
      from typing import Awaitable

      class MyQuery(Awaitable[str]): ...

      class Foo:
        def __init__(self) -> None:
          self.x: MyQuery = MyQuery()
    |}
    []
    context;
  assert_awaitable_errors
    {|
      from typing import Optional, Awaitable

      class MyQuery(Awaitable[str]): ...

      class Foo:
        x: Optional[MyQuery]

        def set_x(self) -> None:
          self.x = MyQuery()
    |}
    []
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      class C:
        a = awaitable()
        def await_the_awaitable(self):
          await self.a
    |}
    []
    context;
  ()


let test_ternary_expression context =
  assert_awaitable_errors
    {|
      async def awaitable() -> bool: ...

      async def main() -> None:
        unawaited = awaitable()
        awaited = awaitable()

        1 if unawaited else 2
        1 if await awaited else 2
    |}
    ["Unawaited awaitable [1001]: Awaitable assigned to `unawaited` is never awaited."]
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def main(some_bool: bool) -> None:
        unawaited = awaitable()
        awaited = awaitable()

        unawaited if some_bool else 2
        await awaited if some_bool else 2
    |}
    ["Unawaited awaitable [1001]: Awaitable assigned to `unawaited` is never awaited."]
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def main(some_bool: bool) -> None:
        unawaited = awaitable()
        awaited = awaitable()

        1 if some_bool else unawaited
        1 if some_bool else (await awaited)
    |}
    ["Unawaited awaitable [1001]: Awaitable assigned to `unawaited` is never awaited."]
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...

      async def main(some_bool: bool) -> None:
        unawaited1 = awaitable()
        unawaited2 = awaitable()
        awaited1 = awaitable()
        awaited2 = awaitable()

        unawaited1 if some_bool else unawaited2
        await (awaited1 if some_bool else awaited2)
    |}
    [
      "Unawaited awaitable [1001]: Awaitable assigned to `unawaited1` is never awaited.";
      "Unawaited awaitable [1001]: Awaitable assigned to `unawaited2` is never awaited.";
    ]
    context;
  assert_awaitable_errors
    {|
      async def awaitable() -> bool: ...

      async def main(some_bool: bool) -> None:
        unawaited1 = awaitable()
        awaited1 = awaitable()
        awaited2 = awaitable()

        await (awaited1 if unawaited1 else awaited2)
    |}
    (* TODO(T79853064): Ideally, this should warn about `unawaited1` not being awaited. But because
       we mark all nested awaitables as being awaited, we miss the conditional not being awaited. *)
    []
    context;
  ()


let test_bottom_type context =
  assert_awaitable_errors
    {|
      from typing import Callable, TypeVar

      T = TypeVar("T")

      def meet_of_parameter_types(x: Callable[[T], None], y: Callable[[T], None]) -> T: ...

      def f1(x: int) -> None: ...
      def f2(x: str) -> None: ...

      def main() -> None:
          bottom_type = meet_of_parameter_types(f1, f2)
    |}
    []
    context;
  (* TODO(T140446657): Addition of union of literals should not return bottom. *)
  assert_awaitable_errors
    {|
      from typing import Union
      from typing_extensions import Literal

      def main(some_bool: bool, y: Union[Literal[42], Literal[99]]) -> None:
        z = y + y
    |}
    []
    context;
  (* TODO(T140446657): Addition of union of literals should not return bottom. *)
  assert_awaitable_errors
    {|
      def main(some_bool: bool) -> None:
        y = 42 if some_bool else 0
        y + y
    |}
    []
    context;
  ()


let test_class_satisfying_awaitable context =
  assert_awaitable_errors
    {|
      from typing import Awaitable

      class C(Awaitable[int]): ...

      async def main() -> None:
        unawaited = C()
    |}
    []
    context;
  assert_awaitable_errors
    {|
      from typing import Any, Generator

      class C:
        def __await__(self) -> Generator[Any, None, str]: ...

      async def main() -> None:
        unawaited = C()
    |}
    []
    context;
  assert_awaitable_errors
    {|
      from typing import Awaitable

      class C(Awaitable[int]): ...

      def return_C() -> C: ...

      async def main() -> None:
        unawaited = return_C()
    |}
    []
    context;
  ()


let test_non_async_function context =
  assert_awaitable_errors
    {|
      from typing import Awaitable

      async def foo() -> int: ...

      def main() -> None:
        unawaited = foo()
    |}
    ["Unawaited awaitable [1001]: Awaitable assigned to `unawaited` is never awaited."]
    context;
  ()


let () =
  "unawaited"
  >::: [
         "forward" >:: test_forward;
         "initial" >:: test_initial;
         "state" >:: test_state;
         test_list test_method_call;
         "aliases" >:: test_aliases;
         "assign" >:: test_assign;
         "return" >:: test_return;
         "globals" >:: test_globals;
         "if" >:: test_if;
         "pass_to_callee" >:: test_pass_to_callee;
         "placeholder_stubs" >:: test_placeholder_stubs;
         "getattr" >:: test_getattr;
         "attribute_assignment" >:: test_attribute_assignment;
         "ternary" >:: test_ternary_expression;
         "bottom_type" >:: test_bottom_type;
         "class_satisfying_awaitable" >:: test_class_satisfying_awaitable;
         "non_async_function" >:: test_non_async_function;
       ]
  |> Test.run
