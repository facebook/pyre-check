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
  (* Assignments *)
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
      x = 1
      x = x + 1
    |}
    ["Dead store [1003]: Value assigned to `x` is never used."];
  assert_liveness_errors
    {|
      def foo(t: typing.Tuple[int, int]) -> None:
        x, y = t
    |}
    [ "Dead store [1003]: Value assigned to `x` is never used.";
      "Dead store [1003]: Value assigned to `y` is never used." ];
  assert_liveness_errors
    {|
      def foo() -> None:
        x, (y, z) = 1, (2, 3)
    |}
    [ "Dead store [1003]: Value assigned to `x` is never used.";
      "Dead store [1003]: Value assigned to `y` is never used.";
      "Dead store [1003]: Value assigned to `z` is never used." ];
  assert_liveness_errors
    {|
      def foo() -> None:
        [x, *y, z] = [1, 2, 3, 4, 5]
    |}
    [ "Dead store [1003]: Value assigned to `x` is never used.";
      "Dead store [1003]: Value assigned to `y` is never used.";
      "Dead store [1003]: Value assigned to `z` is never used." ];

  (* Parameters *)
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
    [ "Dead store [1003]: Value assigned to `x` is never used.";
      "Dead store [1003]: Value assigned to `y` is never used.";
      "Dead store [1003]: Value assigned to `a` is never used." ];

  (* Reassignment *)
  assert_liveness_errors
    {|
      x = 1
      x = 2
    |}
    [ "Dead store [1003]: Value assigned to `x` is never used.";
      "Dead store [1003]: Value assigned to `x` is never used." ];
  assert_liveness_errors
    {|
      x = 1
      x = 2
      x
    |}
    ["Dead store [1003]: Value assigned to `x` is never used."];

  (* Dead Code *)
  assert_liveness_errors
    {|
      def foo() -> None:
        x = 1
        return
        x
    |}
    ["Dead store [1003]: Value assigned to `x` is never used."];
  assert_liveness_errors
    {|
      def foo() -> None:
        x = 1
        if False:
          x
    |}
    ["Dead store [1003]: Value assigned to `x` is never used."];
  assert_liveness_errors
    {|
      def use(x: int) -> None:
        x

      def foo(test: bool) -> None:
        x = 1
        if test:
          sys.exit(0)
          use(x)
    |}
    ["Dead store [1003]: Value assigned to `x` is never used."];

  (* If *)
  assert_liveness_errors
    {|
      def foo(test: bool) -> None:
        if test:
          x = 2
        else:
          x = 3
        x
    |}
    [];
  assert_liveness_errors
    {|
      def foo(test: bool) -> None:
        x = 1
        if test:
          x = 2
        x
    |}
    [];
  assert_liveness_errors
    {|
      def foo(test: bool) -> None:
        x = 1
        if test:
          x = 2
        else:
          x = 3
        x
    |}
    ["Dead store [1003]: Value assigned to `x` is never used."];
  assert_liveness_errors
    {|
      def foo(test: bool) -> None:
        x = 1
        if test:
          x = 2
        else:
          x = 3
    |}
    [ "Dead store [1003]: Value assigned to `x` is never used.";
      "Dead store [1003]: Value assigned to `x` is never used.";
      "Dead store [1003]: Value assigned to `x` is never used." ];

  (* For *)
  assert_liveness_errors
    {|
      def foo(my_list: typing.List[int]) -> None:
        for item in my_list:
          x = 1
    |}
    [ "Dead store [1003]: Value assigned to `item` is never used.";
      "Dead store [1003]: Value assigned to `x` is never used." ];
  assert_liveness_errors
    {|
      def foo(my_list: typing.List[int]) -> None:
        for item in my_list:
          x = 1
        a = item
        b = x
    |}
    (* TODO(T52796841): Unused iterator not caught. *)
    [ "Dead store [1003]: Value assigned to `a` is never used.";
      "Dead store [1003]: Value assigned to `b` is never used." ];

  (* While *)
  assert_liveness_errors
    {|
      def foo() -> None:
        x = 1
        while x > 0:
          y = 1
    |}
    (* TODO(T52796841): While condition should mark 'x' as used. *)
    [ "Dead store [1003]: Value assigned to `x` is never used.";
      "Dead store [1003]: Value assigned to `y` is never used." ];
  assert_liveness_errors
    {|
      def foo() -> None:
        while True:
          y = 1
          y
          y = 2
    |}
    ["Dead store [1003]: Value assigned to `y` is never used."];

  (* Global *)
  assert_liveness_errors
    {|
      x = 1
      def foo() -> None:
        x = 2
        global x
        x
    |}
    (* TODO(T52796841): Usage in 'global' is not a use. *)
    [];

  (* Try *)
  assert_liveness_errors
    {|
      try:
        x = 1
      except:
        pass
      finally:
        x
    |}
    (* TODO(T52796841): Usage in 'finally' block should work. *)
    ["Dead store [1003]: Value assigned to `x` is never used."]


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
