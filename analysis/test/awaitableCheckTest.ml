(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Analysis
open Test

let assert_awaitable_errors ~context =
  let check ~configuration ~global_resolution ~source =
    TypeCheck.run ~configuration ~global_resolution ~source |> ignore;
    AwaitableCheck.run ~configuration ~global_resolution ~source
  in
  assert_errors ~context ~check


let test_forward context =
  let assert_awaitable_errors = assert_awaitable_errors ~context in
  assert_awaitable_errors
    {|
      def awaitable() -> typing.Awaitable[int]: ...
      unawaited = awaitable()
    |}
    ["Unawaited awaitable [101]: Awaitable assigned to `unawaited` is never awaited."];
  assert_awaitable_errors
    {|
      def awaitable() -> typing.Awaitable[int]: ...
      awaited = awaitable()
      await awaited
    |}
    [];

  (* Assert. *)
  assert_awaitable_errors
    {|
      def awaitable() -> typing.Awaitable[int]: ...
      awaited = awaitable()
      _ = await awaited
    |}
    [];
  assert_awaitable_errors
    {|
      def awaitable() -> typing.Awaitable[int]: ...
      awaited = awaitable()
      assert (await awaited)
    |}
    [];
  assert_awaitable_errors
    {|
      def awaitable() -> typing.Awaitable[int]: ...
      awaited = awaitable()
      assert awaited
    |}
    ["Unawaited awaitable [101]: Awaitable assigned to `awaited` is never awaited."];

  (* Delete. *)
  assert_awaitable_errors
    {|
      def awaitable() -> typing.Awaitable[int]: ...
      awaited = awaitable()
      if "moon_is_high":
        del (await awaited)
    |}
    [];

  (* Raise. *)
  assert_awaitable_errors
    {|
      def awaitable() -> typing.Awaitable[Exception]: ...
      awaited = awaitable()
      raise (await awaited)
    |}
    [];

  (* Return. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      def meta_awaitable() -> typing.Awaitable[int]:
        awaited = awaitable()
        return awaited
    |}
    [];

  (* Yield. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      def meta_awaitable():
        awaited = awaitable()
        yield awaited
    |}
    [];
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      def meta_awaitable():
        awaited = awaitable()
        yield (await awaited)
    |}
    [];

  (* We aren't handling (await awaited).__iter__() correctly at the moment, causing this issue. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      def meta_awaitable():
        awaited = awaitable()
        yield from (await awaited)
    |}
    ["Unawaited awaitable [101]: Awaitable assigned to `awaited` is never awaited."];

  (* Tuples. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      def meta_awaitable():
        awaited = awaitable()
        yield (await awaited, 3)
    |}
    [];

  (* Boolean operators. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      def meta_awaitable():
        awaited = awaitable()
        await awaited or Exception("You must await.")
    |}
    [];
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      def meta_awaitable():
        awaited = awaitable()
        1 and (2 and (await awaited))
    |}
    [];

  (* We view parameters which flow into a call as having been awaited. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      async def takes_awaitable(x: typing.Awaitable[int]): ...
      def meta_awaitable():
        awaited = awaitable()
        await takes_awaitable(awaited)
    |}
    [];

  (* Comparison operators. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      async def meta_awaitable():
        awaited = awaitable()
        return (await awaited) > 2
    |}
    [];
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      async def meta_awaitable() -> bool:
        awaited = awaitable()
        return 0 == (await awaited)
    |}
    [];

  (* Container literals. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      async def meta_awaitable():
        awaited = awaitable()
        return [1, await awaited, 2]
    |}
    [];
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      async def meta_awaitable():
        awaited = awaitable()
        return {1, await awaited, 2}
    |}
    [];
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      async def meta_awaitable():
        awaited = awaitable()
        return {await awaited: 1, 2: 2}
    |}
    [];
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      async def meta_awaitable():
        awaited = awaitable()
        return {"foo": [await awaited]}
    |}
    [];

  (* Lambdas. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      async def meta_awaitable():
        awaited = awaitable()
        lambda x: (await awaited) or 42
    |}
    [];

  (* Starred expressions. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[typing.Iterable[int]]: ...
      async def meta_awaitable():
        awaited = awaitable()
        [1, *(await awaited)]
    |}
    [];
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[typing.Dict[int, str]]: ...
      async def meta_awaitable():
        awaited = awaitable()
        {1: "x", **(await awaited)}
    |}
    [];

  (* Ternaries. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      awaited = awaitable()
      1 if (await awaited) else 2
    |}
    [];
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      awaited = awaitable()
      (await awaited) if 1 else 2
    |}
    [];
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      awaited = awaitable()
      1 if 2 else (await awaited)
    |}
    [];

  (* Unary operators. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      awaited = awaitable()
      -(not (await awaited))
    |}
    [];

  (* Yield. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      awaited = awaitable()
      yield (await awaited) if 1 > 2 else False
    |}
    [];

  (* Comprehensions. *)
  assert_awaitable_errors
    {|
      def awaitable() -> typing.Awaitable[int]: ...
      awaited = awaitable()
      [i for i in await awaited]
    |}
    [];
  assert_awaitable_errors
    {|
      def awaitable() -> typing.Awaitable[int]: ...
      awaited = awaitable()
      [(await awaited) for i in [1, 2, 3]]
    |}
    [];
  assert_awaitable_errors
    {|
      def awaitable() -> typing.Awaitable[int]: ...
      awaited = awaitable()
      {(await awaited) for i in [1, 2, 3]}
    |}
    [];
  assert_awaitable_errors
    {|
      def awaitable() -> typing.Awaitable[int]: ...
      awaited = awaitable()
      {i: (await awaited) for i in [1, 2, 3]}
    |}
    [];
  assert_awaitable_errors
    {|
      def awaitable() -> typing.Awaitable[int]: ...
      awaited = awaitable()
      ((await awaited) for i in [1, 2, 3])
    |}
    [];
  assert_awaitable_errors
    {|
      def awaitable() -> typing.Awaitable[int]: ...
      def other_awaitable() -> typing.Awaitable[int]: ...
      unawaited = awaitable()
      other_unawaited = other_awaitable()
      if True > False:
        unawaited = other_unawaited
    |}
    [ "Unawaited awaitable [101]: Awaitable assigned to `unawaited` is never awaited.";
      "Unawaited awaitable [101]: Awaitable assigned to `unawaited`, `other_unawaited` is never \
       awaited." ];
  assert_awaitable_errors
    {|
      def awaitable() -> typing.Awaitable[int]: ...
      def other_awaitable() -> typing.Awaitable[int]: ...
      unawaited = awaitable()
      other_unawaited = other_awaitable()
      unawaited = other_unawaited
      await unawaited
    |}
    ["Unawaited awaitable [101]: `awaitable()` is never awaited."];
  assert_awaitable_errors
    {|
      def awaitable() -> typing.Awaitable[int]: ...
      def other_awaitable() -> typing.Awaitable[int]: ...
      unawaited = awaitable()
      other_unawaited = other_awaitable()
      if 1 > 2:
        unawaited = other_unawaited
      await unawaited
    |}
    [];
  assert_awaitable_errors
    {|
      def awaitable() -> typing.Awaitable[int]: ...
      def other_awaitable() -> typing.Awaitable[int]: ...
      unawaited = awaitable()
      other_unawaited = other_awaitable()
      if 1 > 2:
        unawaited = other_unawaited
      await other_unawaited
    |}
    ["Unawaited awaitable [101]: Awaitable assigned to `unawaited` is never awaited."];
  assert_awaitable_errors
    {|
      def awaitable() -> typing.Awaitable[int]: ...
      def foo():
        awaitable()
    |}
    ["Unawaited awaitable [101]: `awaitable()` is never awaited."];

  (* Ensure that we don't crash when attempting to await a non-simple name. *)
  assert_awaitable_errors
    {|
      class C:
        def awaitable(self) -> typing.Awaitable[int]: ...

      def foo():
        await C().awaitable()
    |}
    [];
  assert_awaitable_errors
    {|
       async def awaitable() -> typing.Tuple[int, int]: ...
       import asyncio
       async def foo() -> int:
         a = awaitable()
         b = awaitable()
         _, c = await asyncio.gather(a, b)
    |}
    [];
  assert_awaitable_errors
    {|
       async def awaitable() -> typing.Tuple[int, int]: ...
       import asyncio
       async def foo() -> int:
         a = awaitable()
         b = awaitable()
         _, c = await asyncio.gather(a, b)
    |}
    [];
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...
      async def foo():
        a, b = awaitable(), awaitable()
        await a
    |}
    ["Unawaited awaitable [101]: Awaitable assigned to `b` is never awaited."];
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...
      async def foo():
        a, b = awaitable(), awaitable()
        await b
    |}
    ["Unawaited awaitable [101]: Awaitable assigned to `a` is never awaited."];
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...
      async def foo():
        [a, (b, [c, d], e)] = (awaitable(), (awaitable(), (awaitable(), awaitable()), awaitable()))
    |}
    [ "Unawaited awaitable [101]: Awaitable assigned to `a` is never awaited.";
      "Unawaited awaitable [101]: Awaitable assigned to `b` is never awaited.";
      "Unawaited awaitable [101]: Awaitable assigned to `c` is never awaited.";
      "Unawaited awaitable [101]: Awaitable assigned to `d` is never awaited.";
      "Unawaited awaitable [101]: Awaitable assigned to `e` is never awaited." ];
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...
      async def foo():
        a, *b, c = awaitable(), awaitable(), awaitable(), awaitable(), awaitable()
        await a
    |}
    ["Unawaited awaitable [101]: Awaitable assigned to `c` is never awaited."];

  (* We don't validate that every expression in a starred one is awaited. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> int: ...
      async def foo():
        a, *b, c = awaitable(), awaitable(), awaitable(), awaitable(), awaitable()
        await asyncio.gather(a, c)
    |}
    [];

  (* We have limitations at the moment. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      def meta_awaitable() -> typing.Tuple[typing.Awaitable[int], int]:
        awaited = awaitable()
        return awaited, 1
    |}
    ["Unawaited awaitable [101]: Awaitable assigned to `awaited` is never awaited."];
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      class C:
        a = awaitable()
        def await_the_awaitable(self):
          await self.a
    |}
    ["Unawaited awaitable [101]: Awaitable assigned to `C.a` is never awaited."]


let test_state context =
  assert_awaitable_errors
    ~context
    {|
      def awaitable() -> typing.Awaitable[int]: ...

      if True:
        unawaited = awaitable()
    |}
    ["Unawaited awaitable [101]: Awaitable assigned to `unawaited` is never awaited."];
  assert_awaitable_errors
    ~context
    {|
      def awaitable() -> typing.Awaitable[int]: ...

      unawaited = awaitable()
      if True:
        await unawaited
    |}
    []


let () = "awaitableCheck" >::: ["forward" >:: test_forward; "state" >:: test_state] |> Test.run
