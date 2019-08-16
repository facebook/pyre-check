(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Analysis
open Test

let assert_liveness_errors ~context =
  let check ~configuration ~global_resolution ~source =
    TypeCheck.run ~configuration ~global_resolution ~source |> ignore;
    LivenessCheck.run ~configuration ~global_resolution ~source
  in
  assert_errors ~context ~check


let test_forward context =
  let assert_liveness_errors = assert_liveness_errors ~context in
  assert_liveness_errors
    {|
      x = 1
    |}
    ["Dead store [1003]: Value assigned to `x` is never used."];
  assert_liveness_errors
    {|
      x = 1
      y = x
    |}
    ["Dead store [1003]: Value assigned to `y` is never used."];
  assert_liveness_errors
    {|
      def foo(t: typing.Tuple[int, int]) -> None:
        x, y = t
    |}
    [ "Dead store [1003]: Value assigned to `y` is never used.";
      "Dead store [1003]: Value assigned to `x` is never used." ];
  assert_liveness_errors
    {|
      def foo() -> None:
        x, (y, z) = 1, (2, 3)
    |}
    [ "Dead store [1003]: Value assigned to `z` is never used.";
      "Dead store [1003]: Value assigned to `y` is never used.";
      "Dead store [1003]: Value assigned to `x` is never used." ];
  assert_liveness_errors
    {|
      def foo() -> None:
        [x, *y, z] = [1, 2, 3, 4, 5]
    |}
    [ "Dead store [1003]: Value assigned to `z` is never used.";
      "Dead store [1003]: Value assigned to `y` is never used.";
      "Dead store [1003]: Value assigned to `x` is never used." ];
  assert_liveness_errors
    {|
      def foo(x: int) -> None:
        y = 1
    |}
    [ "Dead store [1003]: Value assigned to `x` is never used.";
      "Dead store [1003]: Value assigned to `y` is never used." ];
  assert_liveness_errors
    {|
      def foo(x: int, y: int, z: int) -> None:
        a = z
    |}
    [ "Dead store [1003]: Value assigned to `y` is never used.";
      "Dead store [1003]: Value assigned to `x` is never used.";
      "Dead store [1003]: Value assigned to `a` is never used." ]


let test_nested_defines context =
  let assert_liveness_errors = assert_liveness_errors ~context in
  assert_liveness_errors
    {|
      x = 1
      def foo() -> None:
        y = 1
    |}
    [ "Dead store [1003]: Value assigned to `x` is never used.";
      "Dead store [1003]: Value assigned to `y` is never used." ];
  assert_liveness_errors
    {|
      x = 1
      def foo() -> None:
        y = 1
        def bar() -> None:
          z = 1
    |}
    [ "Dead store [1003]: Value assigned to `x` is never used.";
      "Dead store [1003]: Value assigned to `y` is never used.";
      "Dead store [1003]: Value assigned to `z` is never used." ];
  assert_liveness_errors
    {|
      x = 1
      def foo() -> None:
        y = 1
        def bar() -> None:
          z = y
    |}
    [ "Dead store [1003]: Value assigned to `x` is never used.";
      "Dead store [1003]: Value assigned to `z` is never used." ]


let () =
  "livenessCheck"
  >::: ["forward" >:: test_forward; "nested_defines" >:: test_nested_defines]
  |> Test.run
