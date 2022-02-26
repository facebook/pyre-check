(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_check_scoping context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors {|
      def foo(foo: str) -> str:
        return foo
    |} [];

  assert_type_errors
    {|
      class C:
        def method(self, foo: str) -> str:
          return foo
    |}
    [];
  ()


let test_uninitialized context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      def f() -> None:
        def g() -> None:
          x = y
          y = 5
    |}
    ["Uninitialized local [61]: Local variable `y` is undefined, or not always defined."];

  assert_type_errors
    {|
      def f() -> None:
        x: int = 0
        y: int = 0
        def g() -> None:
          nonlocal y, z
          _ = x    # Refers to local `x`, error as not yet initialized
          x = 1
          _ = y    # Refers to nonlocal `y`, hence OK
          y = 1
          _ = z    # Refers to nonlocal `z`, error as not defined in outer scope
    |}
    [
      "Uninitialized local [61]: Local variable `x` is undefined, or not always defined.";
      "Unbound name [10]: Name `z` is used but not defined in the current scope.";
    ];

  (* Extracted from a real-world example. *)
  assert_type_errors
    {|
      from dataclasses import dataclass
      from typing import Optional

      @dataclass
      class Task(object):
          harness_config: int

      def get_task(i: int) -> Task:
          return Task(harness_config=i)

      def foo(i: int) -> int:
          return i

      def outer() -> None:
          def inner(task_id: int) -> None:
              if True:
                  pass
              else:
                  task = get_task(task_id)

                  if task.harness_config:
                      harness_config = task.harness_config
                  foo(harness_config)
      |}
    [
      "Uninitialized local [61]: Local variable `harness_config` is undefined, or not always \
       defined.";
    ];

  ()


let () =
  "scope"
  >::: ["check_scoping" >:: test_check_scoping; "check_uninitialized" >:: test_uninitialized]
  |> Test.run
