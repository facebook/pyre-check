(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Test

let assert_uninitialized_errors ~context =
  let check ~configuration ~environment ~source =
    UninitializedLocalCheck.run
      ~configuration
      ~environment:(TypeEnvironment.read_only environment)
      ~source
  in
  assert_errors ~context ~check


let test_simple context =
  let assert_uninitialized_errors = assert_uninitialized_errors ~context in

  (* TODO (T69630394): Tests below document errors we do not report, but would like this check to. *)

  (* line 2, in f: local variable 'y' referenced before assignment *)
  assert_uninitialized_errors {|
      def f():
        x = y
        y = 5
    |} [];

  (* line 4, in f: local variable 'y' referenced before assignment *)
  assert_uninitialized_errors
    {|
      def f(x):
        if x > 5:
          y = 2
        return y
    |}
    [];

  (* line 4, in increment: local variable 'counter' referenced before assignment *)
  assert_uninitialized_errors
    {|
      counter = 0

      def increment() -> None:
        counter += 1
    |}
    [];

  (* line 6, in f: local variable 'x' referenced before assignment *)
  assert_uninitialized_errors
    {|
      def f() -> int:
          try:
              bad = 0 / 0
              x = 1  # `x` is defined here
          except ZeroDivisionError:
              return x
    |}
    [];

  (* Extracted from a real-world example. *)
  assert_uninitialized_errors
    {|
      from dataclasses import dataclass
      from typing import Optional
      import random

      @dataclass
      class Task(object):
          harness_config: int

      def get_task(i: int) -> Task:
          return Task(harness_config=i)

      def foo(i: int) -> int:
          return i

      def outer() -> None:
          def inner(task_id: int) -> None:
              if random.random():
                  pass
              else:
                  task = get_task(task_id)

                  if task.harness_config:
                      harness_config = task.harness_config
                  foo(harness_config)
      |}
    []


let () = "uninitializedCheck" >::: ["simple" >:: test_simple] |> Test.run
