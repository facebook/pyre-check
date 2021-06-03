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

  assert_uninitialized_errors
    {|
      def f():
        x = y
        y = 5
    |}
    ["Unbound name [10]: Name `y` is used but not defined in the current scope."];

  assert_uninitialized_errors {|
      def f(y):
        x = y
        y = 5
    |} [];

  assert_uninitialized_errors
    {|
      def f(x):
        if x > 5:
          return y   # Error
        y = 5
        z = 5
        return z   # OK
    |}
    ["Unbound name [10]: Name `y` is used but not defined in the current scope."];

  assert_uninitialized_errors
    {|
      def f(x):
        if x > 5:
          y = 2
        return y
    |}
    ["Unbound name [10]: Name `y` is used but not defined in the current scope."];

  assert_uninitialized_errors
    {|
      def f() -> int:
          try:
              bad = 0 / 0
              x = 1  # `x` is defined here
          except ZeroDivisionError:
              return x
    |}
    ["Unbound name [10]: Name `x` is used but not defined in the current scope."];

  (* TODO (T69630394): Tests below document either errors we do not report, but would like this
     check to; or non-errors this check is currently reporting *)

  (* no error *)
  assert_uninitialized_errors
    {|
       x: int = 5
       def f():
         return x
    |}
    ["Unbound name [10]: Name `x` is used but not defined in the current scope."];

  (* line 4, in increment: local variable 'counter' referenced before assignment *)
  assert_uninitialized_errors
    {|
      counter = 0

      def increment() -> None:
        counter += 1
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
    [];

  ()


let () = "uninitializedCheck" >::: ["simple" >:: test_simple] |> Test.run
