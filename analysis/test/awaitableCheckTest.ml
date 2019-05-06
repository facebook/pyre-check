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
