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


let test_forward =
  test_list
    [
      (* Test that a non-async `def` returning `Awaitable[int]` is checked correctly. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Awaitable

              def awaitable() -> Awaitable[int]: ...

              async def main() -> None:
                unawaited = awaitable()
            |}
           ["Unawaited awaitable [1001]: Awaitable assigned to `unawaited` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def main() -> None:
                unawaited = awaitable()
            |}
           ["Unawaited awaitable [1001]: Awaitable assigned to `unawaited` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def main() -> None:
                awaited = awaitable()
                await awaited
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from asyncio.futures import Future

              def future() -> Future[int]: ...

              async def main() -> None:
                unawaited = future()
            |}
           ["Unawaited awaitable [1001]: Awaitable assigned to `unawaited` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from _asyncio import Future

              def future() -> Future[int]: ...

              async def main() -> None:
                unawaited = future()
            |}
           ["Unawaited awaitable [1001]: Awaitable assigned to `unawaited` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from asyncio.futures import Future

              def future() -> Future[int]: ...

              async def main() -> None:
                awaited = future()
                await awaited
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* Assert. *)
      assert_awaitable_errors
        {|
              async def awaitable() -> int: ...

              awaited = awaitable()
              _ = await awaited
            |}
        [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              awaited = awaitable()
              assert (await awaited)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def main() -> None:
                awaited = awaitable()
                assert awaited
            |}
           ["Unawaited awaitable [1001]: Awaitable assigned to `awaited` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* Delete. *)
      assert_awaitable_errors
        {|
              async def awaitable() -> int: ...

              awaited = awaitable()
              if "moon_is_high":
                del (await awaited)
            |}
        [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* Raise. *)
      assert_awaitable_errors
        {|
              async def awaitable() -> Exception: ...

              awaited = awaitable()
              raise (await awaited)
            |}
        [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* Return. *)
      assert_awaitable_errors
        {|
              async def awaitable() -> int: ...

              async def meta_awaitable() -> int:
                awaited = awaitable()
                return awaited
            |}
        [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* Yield. *)
      assert_awaitable_errors
        {|
              async def awaitable() -> int: ...
              def meta_awaitable():
                awaited = awaitable()
                yield awaited
            |}
        [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...
              def meta_awaitable():
                awaited = awaitable()
                yield (await awaited)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...
              def meta_awaitable():
                awaited = awaitable()
                yield from (await awaited)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* Tuples. *)
      assert_awaitable_errors
        {|
              async def awaitable() -> int: ...
              def meta_awaitable():
                awaited = awaitable()
                yield (await awaited, 3)
            |}
        [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* Any is not an awaitable. *)
      assert_awaitable_errors
        {|
            from typing import Any

            def returns_any() -> Any: ...

            async def foo() -> None:
              x = returns_any()
              return
            |}
        [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
            from typing import Any

            async def foo(param: Any) -> None:
              return
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* Boolean operators. *)
      assert_awaitable_errors
        {|
              async def awaitable() -> int: ...
              def meta_awaitable():
                awaited = awaitable()
                await awaited or Exception("You must await.")
            |}
        [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...
              def meta_awaitable():
                awaited = awaitable()
                1 and (2 and (await awaited))
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* We view parameters which flow into a call as having been awaited. *)
      assert_awaitable_errors
        {|
              from typing import Awaitable

              async def awaitable() -> int: ...

              async def takes_awaitable(x: Awaitable[int]): ...

              def meta_awaitable():
                awaited = awaitable()
                await takes_awaitable(awaited)
            |}
        [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Awaitable

              async def awaitable() -> int: ...

              async def takes_awaitable(x: Awaitable[int]) -> None: ...

              def meta_awaitable():
                await takes_awaitable(awaitable())
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Awaitable

              async def awaitable() -> int: ...

              async def takes_awaitable(x: Awaitable[int]) -> None: ...

              def meta_awaitable():
                await takes_awaitable({ "a": awaitable(), "b": awaitable()})
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* Comparison operators. *)
      assert_awaitable_errors
        {|
              async def awaitable() -> int: ...

              async def meta_awaitable():
                awaited = awaitable()
                return (await awaited) > 2
            |}
        [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def meta_awaitable() -> bool:
                awaited = awaitable()
                return 0 == (await awaited)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* Container literals. *)
      assert_awaitable_errors
        {|
              async def awaitable() -> int: ...

              async def meta_awaitable():
                awaited = awaitable()
                return [1, await awaited, 2]
            |}
        [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def meta_awaitable():
                awaited = awaitable()
                return {1, await awaited, 2}
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def meta_awaitable():
                awaited = awaitable()
                return {await awaited: 1, 2: 2}
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def meta_awaitable():
                awaited = awaitable()
                return {"foo": [await awaited]}
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* Lambdas. *)
      assert_awaitable_errors
        {|
              async def awaitable() -> int: ...

              async def meta_awaitable():
                awaited = awaitable()
                lambda x: (await awaited) or 42
            |}
        [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* Starred expressions. *)
      assert_awaitable_errors
        {|
              async def awaitable() -> typing.Iterable[int]: ...

              async def meta_awaitable():
                awaited = awaitable()
                [1, *(await awaited)]
            |}
        [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> typing.Dict[int, str]: ...

              async def meta_awaitable():
                awaited = awaitable()
                {1: "x", **(await awaited)}
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* Unary operators. *)
      assert_awaitable_errors
        {|
              async def awaitable() -> int: ...

              awaited = awaitable()
              -(not (await awaited))
            |}
        [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* Yield. *)
      assert_awaitable_errors
        {|
              async def awaitable() -> int: ...

              awaited = awaitable()
              yield (await awaited) if 1 > 2 else False
            |}
        [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* Comprehensions. *)
      assert_awaitable_errors
        {|
              async def awaitable() -> int: ...

              awaited = awaitable()
              [i for i in await awaited]
            |}
        [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              awaited = awaitable()
              [(await awaited) for i in [1, 2, 3]]
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              awaited = awaitable()
              {(await awaited) for i in [1, 2, 3]}
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              awaited = awaitable()
              {i: (await awaited) for i in [1, 2, 3]}
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              awaited = awaitable()
              ((await awaited) for i in [1, 2, 3])
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
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
             "Unawaited awaitable [1001]: Awaitable assigned to `unawaited`, `other_unawaited` is \
              never awaited.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def other_awaitable() -> int: ...

              async def main() -> None:
                unawaited = awaitable()
                other_unawaited = other_awaitable()
                unawaited = other_unawaited
                await unawaited
            |}
           ["Unawaited awaitable [1001]: `test.awaitable()` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
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
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
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
           ["Unawaited awaitable [1001]: Awaitable assigned to `unawaited` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def foo():
                awaitable()
            |}
           ["Unawaited awaitable [1001]: `test.awaitable()` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* Ensure that we don't crash when attempting to await a non-simple name. *)
      assert_awaitable_errors
        {|
              class C:
                async def awaitable(self) -> int: ...

              async def foo():
                await C().awaitable()
            |}
        [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
               import asyncio

               async def awaitable() -> typing.Tuple[int, int]: ...

               async def foo() -> int:
                 a = awaitable()
                 b = awaitable()
                 _, c = await asyncio.gather(a, b)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
               import asyncio

               async def awaitable() -> typing.Tuple[int, int]: ...

               async def foo() -> int:
                 a = awaitable()
                 b = awaitable()
                 _, c = await asyncio.gather(a, b)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...
              async def foo():
                a, b = awaitable(), awaitable()
                await a
            |}
           ["Unawaited awaitable [1001]: Awaitable assigned to `b` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...
              async def foo():
                a, b = awaitable(), awaitable()
                await b
            |}
           ["Unawaited awaitable [1001]: Awaitable assigned to `a` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
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
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def foo():
                a, *b, c = awaitable(), awaitable(), awaitable()
                await a
            |}
           [
             "Unawaited awaitable [1001]: `test.awaitable()` is never awaited.";
             "Unawaited awaitable [1001]: Awaitable assigned to `c` is never awaited.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* We don't validate that every expression in a starred one is awaited. *)
      assert_awaitable_errors
        {|
              async def awaitable() -> int: ...

              async def foo():
                a, *b, c = awaitable(), awaitable(), awaitable()
                await asyncio.gather(a, c)
            |}
        ["Unawaited awaitable [1001]: `test.awaitable()` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Awaitable

              async def awaitable() -> int: ...

              def meta_awaitable() -> typing.Tuple[Awaitable[int], int]:
                awaited = awaitable()
                return awaited, 1
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
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
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def foo() -> None:
                d = {
                  awaitable(): 2,
                  3: awaitable(),
                }
                await d
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def foo() -> None:
                l = [1, {2: awaitable()}]
            |}
           ["Unawaited awaitable [1001]: Awaitable assigned to `l` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def foo() -> None:
                l = [1, {2: awaitable()}]
                await l
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              import asyncio

              async def awaitable() -> int: ...

              async def foo() -> None:
                l = [awaitable(), awaitable()]
                await asyncio.gather( *l)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              import asyncio

              async def awaitable() -> int: ...

              async def foo() -> None:
                l = [awaitable(), awaitable()]
                await asyncio.gather(awaitable(), *l)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              import asyncio

              async def awaitable() -> int: ...

              async def foo() -> None:
                l = [awaitable(), awaitable()]
                await asyncio.gather(l if l is not None else awaitable())
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def foo() -> None:
                [] + [awaitable()]
            |}
           ["Unawaited awaitable [1001]: `test.awaitable()` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* Multiple assignment targets. *)
      assert_awaitable_errors
        {|
              async def awaitable() -> int: ...
              def meta_awaitable():
                x = y = await awaitable()
            |}
        [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...
              async def meta_awaitable():
                x = y = awaitable()
                a = b = c = awaitable()
            |}
           [
             "Unawaited awaitable [1001]: Awaitable assigned to `y`, `x` is never awaited.";
             "Unawaited awaitable [1001]: Awaitable assigned to `c`, `b`, `a` is never awaited.";
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* Walrus operator. *)
      assert_awaitable_errors
        {|
              async def awaitable() -> int: ...
              async def meta_awaitable():
                x = y := await awaitable()
            |}
        [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...
              async def meta_awaitable():
                x = (y := awaitable())
            |}
           (* TODO(T53600647): Mention y in the error message. *)
           ["Unawaited awaitable [1001]: Awaitable assigned to `x` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...
              async def meta_awaitable():
                if y := await awaitable():
                  pass
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...
              async def meta_awaitable():
                if y := awaitable():
                  pass
            |}
           (* TODO(T53600647): Mention y in the error message. *)
           ["Unawaited awaitable [1001]: `test.awaitable()` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> bool: ...

              async def foo():
                if awaitable():
                  pass
            |}
           ["Unawaited awaitable [1001]: `test.awaitable()` is never awaited."];
      (* TODO(T197284307): False negative due to not handling splats in RHS of multi-assign. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Awaitable
              
              async def main() -> None:
                  w: tuple[Awaitable[int], str]
                  x, y, z = *w, 5
            |}
           [];
    ]


let test_initial =
  test_list
    [
      (* Technically, just defining this function will not cause a `RuntimeWarning`.

         However, calling this function *will* cause a `RuntimeWarning`.

         So, our emitting an unawaited-awaitable error in the function definition is the same as our
         emitting an incompatible-variable error for a function where we assign `x: str = 1`. As a
         static type checker, we have to be conservative. *)
      (* TODO(T140344232): Emit error when an awaitable parameter is not awaited. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Awaitable

              async def awaitable(x: Awaitable[int]) -> int:
                return 0
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Awaitable

              async def awaitable(x: Awaitable[int]) -> int:
                return (await x)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Awaitable

              async def awaitable( *x: Awaitable[int]) -> int:
                return 0
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              import asyncio
              from typing import Awaitable

              async def awaitable( *x: Awaitable[int]) -> int:
                value, *_others = asyncio.gather( *x)
                return value
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Awaitable

              async def awaitable( **x: Awaitable[int]) -> int:
                return 0
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              import asyncio
              from typing import Awaitable, Iterable

              def await_list(x: Iterable[Awaitable[int]]]) -> Iterable[int]: ...

              async def awaitable( **d: Awaitable[int]) -> int:
                value, *_others = await_list(d.values())
                return value
            |}
           [];
    ]


let test_state =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def main() -> None:
                if True:
                  unawaited = awaitable()
            |}
           ["Unawaited awaitable [1001]: Awaitable assigned to `unawaited` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def main() -> None:
                unawaited = awaitable()
                if True:
                  await unawaited
            |}
           [];
    ]


let ( >>: ) test_name test_function = test_name >:: fun context -> test_function context

let test_method_call =
  test_list
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


let test_aliases =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def foo() -> None:
                x = awaitable()
                x = 42
            |}
           ["Unawaited awaitable [1001]: Awaitable assigned to `x` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def foo() -> None:
                a = [awaitable()]
                b = [1]
                c = a + b
                await c
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def foo() -> None:
                a = [awaitable()]
                b = [1]
                c = a + b
            |}
           ["Unawaited awaitable [1001]: Awaitable assigned to `c`, `a` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def foo() -> None:
                a = [1]
                b = [awaitable()]
                c = a + b
                await c
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Awaitable

              async def awaitable() -> int: ...

              class C:
                x: Awaitable[int] = ...

                def my_method(self) -> None:
                  self.x = awaitable()
            |}
           [];
    ]


let test_return =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              def foo() -> int:
                return awaitable()
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              def foo() -> int:
                x = [awaitable()]
                y = [awaitable()]
                return (x + y)
            |}
           [];
    ]


let test_assign =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
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
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def foo() -> int:
                d = {}
                d["bar"] = awaitable()
                d["foo"] = awaitable()
                await d
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def foo() -> None:
                d = {}
                d["foo"], d["bar"] = awaitable(), 5
                await d
            |}
           [];
      (* Known failure case: we don't track ownership of awaitables in the RHS of an assign into the
         LHS when we cannot destructure a literal tuple. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> tuple[int, int]: ...

              async def foo() -> None:
                d = {}
                d["foo"], d["bar"] = awaitable()
                await d
            |}
           ["Unawaited awaitable [1001]: `test.awaitable()` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              import asyncio

              async def awaitable() -> int: ...

              async def foo() -> int:
                d = {}
                d["bar"] = awaitable()
                d["foo"] = "not awaitable"
                await asyncio.gather( *d.values())
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def foo() -> int:
                return 0
              async def bar() -> None:
                async with foo():
                  return
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def foo() -> int:
                return 0
              async def bar() -> None:
                with foo():
                  return
            |}
           ["Unawaited awaitable [1001]: `test.foo()` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              d: dict[str, str] = {}

              async def get_d() -> dict[str, str]:
                  return d
          
              async def f() -> int:
                is_awaited_simple = get_d()
                is_awaited_multi_target = get_d()
                is_not_awaited = get_d()
                (await is_awaited_simple)["k"] = "v"
                (await is_awaited_multi_target)["k"], y = "v", 42
            |}
           ["Unawaited awaitable [1001]: Awaitable assigned to `is_not_awaited` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def get_key() -> str:
                  return "k"
          
              async def f() -> int:
                is_awaited_simple = get_key()
                is_awaited_multi_target = get_key()
                is_not_awaited = get_key()
                d: dict[str, str] = {}
                d[await is_awaited_simple] = "v"
                d[await is_awaited_multi_target], y = "v", 42
            |}
           ["Unawaited awaitable [1001]: Awaitable assigned to `is_not_awaited` is never awaited."];
    ]


let test_globals =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Awaitable

              MY_GLOBAL: Awaitable[int] = ...

              async def awaitable() -> Awaitable[int]: ...

              async def foo() -> int:
                MY_GLOBAL = awaitable()
            |}
           ["Unawaited awaitable [1001]: Awaitable assigned to `MY_GLOBAL` is never awaited."];
    ]


let test_if =
  test_list
    [
      (* TODO(T79853064): This should emit an error about the awaitable sometimes not being
         awaited. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              def foo(b: bool) -> None:
                unawaited = awaitable()

                if b:
                  await unawaited
            |}
           [];
    ]


let test_pass_to_callee =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Awaitable

              def awaitable() -> Awaitable[int]: ...

              def expect_awaitable(x: Awaitable[int]) -> None: ...

              def foo(b: bool) -> None:
                unawaited = awaitable()
                expect_awaitable(unawaited)
            |}
           [];
    ]


let test_getattr =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Any

              class Foo:
                def __getattr__(self, name: str) -> Any: ...

              def main() -> None:
                x = Foo()
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* TODO(T79853064): Ideally, this should emit an error about `x` not being awaited.
            However, because we treat any class with `__getattr__` as non-awaitable, we don't emit
            an error here. *)
      assert_awaitable_errors
        {|
              from typing import Any, Awaitable

              class Foo():
                def __await__(self) -> Generator[Any, None, str]: ...

                def __getattr__(self, name: str) -> Any: ...

              def main() -> None:
                  x = Foo()
            |}
        [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Any

              class Foo:
                def __getattr__(self, name: str) -> Any: ...

                def some_method(self) -> None:
                  x = type(self)
            |}
           [];
    ]


let test_attribute_assignment =
  test_list
    [
      (* We don't error on methods for classes that are awaitable themselves. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Awaitable

              async def awaitable() -> int: ...

              class C(Awaitable[int]):
                def __init__(self) -> None:
                  self.x = awaitable()
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Awaitable

              async def awaitable() -> int: ...

              class C(Awaitable[int]):
                pass

              class D(C):
                def __init__(self) -> None:
                  self.x = awaitable()
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Awaitable, Generic, TypeVar

              T = TypeVar("T")

              async def awaitable() -> int: ...

              class C(Awaitable[T], Generic[T):
                def __init__(self) -> None:
                  self.x = awaitable()
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Awaitable

              class MyQuery(Awaitable[str]): ...

              class Foo:
                def __init__(self) -> None:
                  self.x: MyQuery = MyQuery()
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Optional, Awaitable

              class MyQuery(Awaitable[str]): ...

              class Foo:
                x: Optional[MyQuery]

                def set_x(self) -> None:
                  self.x = MyQuery()
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              class C:
                a = awaitable()
                def await_the_awaitable(self):
                  await self.a
            |}
           [];
    ]


let test_ternary_expression =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> bool: ...

              async def main() -> None:
                unawaited = awaitable()
                awaited = awaitable()

                1 if unawaited else 2
                1 if await awaited else 2
            |}
           ["Unawaited awaitable [1001]: Awaitable assigned to `unawaited` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def main(some_bool: bool) -> None:
                unawaited = awaitable()
                awaited = awaitable()

                unawaited if some_bool else 2
                await awaited if some_bool else 2
            |}
           ["Unawaited awaitable [1001]: Awaitable assigned to `unawaited` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> int: ...

              async def main(some_bool: bool) -> None:
                unawaited = awaitable()
                awaited = awaitable()

                1 if some_bool else unawaited
                1 if some_bool else (await awaited)
            |}
           ["Unawaited awaitable [1001]: Awaitable assigned to `unawaited` is never awaited."];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
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
           ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              async def awaitable() -> bool: ...

              async def main(some_bool: bool) -> None:
                unawaited1 = awaitable()
                awaited1 = awaitable()
                awaited2 = awaitable()

                await (awaited1 if unawaited1 else awaited2)
            |}
           (* TODO(T79853064): Ideally, this should warn about `unawaited1` not being awaited. But
              because we mark all nested awaitables as being awaited, we miss the conditional not
              being awaited. *)
           [];
    ]


let test_bottom_type =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Callable, TypeVar

              T = TypeVar("T")

              def meet_of_parameter_types(x: Callable[[T], None], y: Callable[[T], None]) -> T: ...

              def f1(x: int) -> None: ...
              def f2(x: str) -> None: ...

              def main() -> None:
                  bottom_type = meet_of_parameter_types(f1, f2)
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* TODO(T140446657): Addition of union of literals should not return bottom. *)
      assert_awaitable_errors
        {|
              from typing import Union
              from typing_extensions import Literal

              def main(some_bool: bool, y: Union[Literal[42], Literal[99]]) -> None:
                z = y + y
            |}
        [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ (* TODO(T140446657): Addition of union of literals should not return bottom. *)
      assert_awaitable_errors
        {|
              def main(some_bool: bool) -> None:
                y = 42 if some_bool else 0
                y + y
            |}
        [];
    ]


let test_class_satisfying_awaitable =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Awaitable

              class C(Awaitable[int]): ...

              async def main() -> None:
                unawaited = C()
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Any, Generator

              class C:
                def __await__(self) -> Generator[Any, None, str]: ...

              async def main() -> None:
                unawaited = C()
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Awaitable

              class C(Awaitable[int]): ...

              def return_C() -> C: ...

              async def main() -> None:
                unawaited = return_C()
            |}
           [];
    ]


let test_non_async_function =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_awaitable_errors
           {|
              from typing import Awaitable

              async def foo() -> int: ...

              def main() -> None:
                unawaited = foo()
            |}
           ["Unawaited awaitable [1001]: Awaitable assigned to `unawaited` is never awaited."];
    ]


let test_unawaited_awaitable_configuration_flag =
  let open IntegrationTest in
  let assert_type_errors ~enable_unawaited_awaitable_analysis source errors =
    assert_type_errors ~enable_unawaited_awaitable_analysis source errors
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           ~enable_unawaited_awaitable_analysis:false
           {|
              async def awaitable() -> int: ...

              async def foo() -> None:
                x = awaitable()
            |}
           [];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           ~enable_unawaited_awaitable_analysis:true
           {|
              async def awaitable() -> int: ...

              async def foo() -> None:
                x = awaitable()
            |}
           ["Unawaited awaitable [1001]: Awaitable assigned to `x` is never awaited."];
      (* Don't warn about unawaited awaitables for top-level assignments. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_type_errors
           ~enable_unawaited_awaitable_analysis:true
           {|
              async def awaitable() -> int: ...

              x = awaitable()
            |}
           [
             "Missing global annotation [5]: Globally accessible variable `x` has no type specified.";
           ];
    ]


let () =
  "unawaited"
  >::: [
         test_forward;
         test_initial;
         test_state;
         test_method_call;
         test_aliases;
         test_assign;
         test_return;
         test_globals;
         test_if;
         test_pass_to_callee;
         test_getattr;
         test_attribute_assignment;
         test_ternary_expression;
         test_bottom_type;
         test_class_satisfying_awaitable;
         test_non_async_function;
         test_unawaited_awaitable_configuration_flag;
       ]
  |> Test.run
