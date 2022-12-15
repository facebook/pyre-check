(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_unawaited_awaitable_configuration_flag context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    ~enable_unawaited_awaitable_check:false
    {|
      async def awaitable() -> int: ...

      async def foo() -> None:
        x = awaitable()
    |}
    [];
  assert_type_errors
    ~enable_unawaited_awaitable_check:true
    {|
      async def awaitable() -> int: ...

      async def foo() -> None:
        x = awaitable()
    |}
    ["Unawaited awaitable [1001]: Awaitable assigned to `x` is never awaited."];
  ()


let () =
  "unawaited"
  >::: ["unawaited_awaitable_configuration_flag" >:: test_unawaited_awaitable_configuration_flag]
  |> Test.run
