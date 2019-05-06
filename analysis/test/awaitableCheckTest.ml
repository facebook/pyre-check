(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Test


let assert_awaitable_errors =
  let check ~configuration ~environment ~source =
    TypeCheck.run ~configuration ~environment ~source |> ignore;
    AwaitableCheck.run ~configuration ~environment ~source
  in
  assert_errors ~check


let test_forward _ =
  assert_awaitable_errors
    {|
      def awaitable() -> typing.Awaitable[int]: ...
      unawaited = awaitable()
    |}
    ["Unawaited awaitable [101]: `unawaited` is never awaited."];
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
    ["Unawaited awaitable [101]: `awaited` is never awaited."];

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
      awaitable()
      raise (await awaitable)
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
    ["Unawaited awaitable [101]: `meta_awaitable.awaited` is never awaited."];

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

  (* Calls are blocked on the access fold refactor to be complete. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      async def takes_awaitable(x: typing.Awaitable[int]): ...
      def meta_awaitable():
        awaited = awaitable()
        takes_awaitable(awaited)
    |}
    ["Unawaited awaitable [101]: `meta_awaitable.awaited` is never awaited."];

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

  (* We have limitations at the moment. *)
  assert_awaitable_errors
    {|
      async def awaitable() -> typing.Awaitable[int]: ...
      def meta_awaitable() -> typing.Tuple[typing.Awaitable[int], int]:
        awaited = awaitable()
        return awaited, 1
    |}
    ["Unawaited awaitable [101]: `meta_awaitable.awaited` is never awaited."];

  assert_awaitable_errors
    {|
      def awaitable() -> typing.Awaitable[int]: ...
      awaited = awaitable()
      [i for i in await awaited]
    |}
    ["Unawaited awaitable [101]: `awaited` is never awaited."]


let test_state _ =
  assert_awaitable_errors
    {|
      def awaitable() -> typing.Awaitable[int]: ...

      if True:
        unawaited = awaitable()
    |}
    ["Unawaited awaitable [101]: `unawaited` is never awaited."];
  assert_awaitable_errors
    {|
      def awaitable() -> typing.Awaitable[int]: ...

      unawaited = awaitable()
      if True:
        await unawaited
    |}
    []


let () =
  "awaitableCheck">:::[
    "forward">::test_forward;
    "state">::test_state;
  ]
  |> Test.run
