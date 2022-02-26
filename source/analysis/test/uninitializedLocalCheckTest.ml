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
    ["Uninitialized local [61]: Local variable `y` is undefined, or not always defined."];
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
    ["Uninitialized local [61]: Local variable `y` is undefined, or not always defined."];
  assert_uninitialized_errors
    {|
      def f(x):
        if x > 5:
          y = 2
        return y
    |}
    ["Uninitialized local [61]: Local variable `y` is undefined, or not always defined."];
  assert_uninitialized_errors
    {|
      def f() -> int:
          try:
              bad = 0 / 0
              x = 1  # `x` is defined here
          except ZeroDivisionError:
              return x
    |}
    ["Uninitialized local [61]: Local variable `x` is undefined, or not always defined."];
  assert_uninitialized_errors
    {|
      x, y, z = 0, 0, 0
      def access_global() -> int:
        global y
        _ = x      # Refers to local `x`, hence error
        x = 1
        _ = y      # Refers to global `y`, explictly specified
        y = 1
        _ = z      # Refers to global `z`, implicitly
    |}
    ["Uninitialized local [61]: Local variable `x` is undefined, or not always defined."];
  assert_uninitialized_errors
    {|
      class Foo(object):
        pass
      def f():
        return Foo()
    |}
    [];
  assert_uninitialized_errors
    {|
      def f():
        def g():
          pass
        g()
    |}
    [];
  assert_uninitialized_errors {|
      def f():
        x, y = 0, 0
        return x, y
    |} [];
  assert_uninitialized_errors
    {|
      def f( *args, **kwargs) -> None:
        print(args)
        print(list(kwargs.items()))
    |}
    [];
  assert_uninitialized_errors
    {|
      x = 0
      def f() -> None:
        global x
        if x == 0:
          x = 1
    |}
    [];
  assert_uninitialized_errors {|
      def f(x: str) -> None:
        assert True, x
    |} [];
  assert_uninitialized_errors
    {|
      from media import something
      def f():
        something()
        media = 1
    |}
    [];
  assert_uninitialized_errors {|
      def f():
        (x := 0)
    |} [];
  assert_uninitialized_errors {|
      def f():
        ((x := 0) and (y := x))
    |} [];

  (* TODO (T94201165): walrus operator same-expression false negative *)
  assert_uninitialized_errors {|
      def f():
        ((y := x) and (x := 0))
    |} [];

  assert_uninitialized_errors {|
      def f():
        [y for x in [1,2,3] if (y:=x) > 2]
    |} [];

  (* TODO(T94414920): attribute reads *)
  assert_uninitialized_errors {|
      def f():
        _ = x.field
        x = Foo()
    |} [];

  assert_uninitialized_errors
    {|
      def f():
        with open("x") as x:
          pass
        _ = x, y
        x, y = None, None
    |}
    ["Uninitialized local [61]: Local variable `y` is undefined, or not always defined."];
  assert_uninitialized_errors
    {|
      def f():
        y = [x for x in x]
        x = []
    |}
    ["Uninitialized local [61]: Local variable `x` is undefined, or not always defined."];
  assert_uninitialized_errors
    {|
      def f():
        _  = [(x, y) for x,y in []]
        x = None
        _ = x, y
        y = None
    |}
    ["Uninitialized local [61]: Local variable `y` is undefined, or not always defined."];

  ()


(* Tests about uninitialized locals reliant on correct CFG construction. *)
let test_cfg context =
  let assert_uninitialized_errors = assert_uninitialized_errors ~context in

  assert_uninitialized_errors
    {|
      def f():
        if True:
          x = 1
        else:
          raise AssertionError("error")
        return x

      def g():
        if True:
          y = 1
        else:
          assert False, "error"
        return y

      def h():
        if True:
          z = 1
        else:
          assert True, "error"
        return z
    |}
    ["Uninitialized local [61]: Local variable `z` is undefined, or not always defined."];
  assert_uninitialized_errors
    {|
      def baz() -> int:
          while True:
              b = 1
              break
          return b
    |}
    [];
  assert_uninitialized_errors
    {|
      def f():
        try:
          x = 1
        except Exception:
          x = 2
        finally:
          print(x)
    |}
    [];
  (* TODO(T106611060): False positive due to CFG construction for `finally`. *)
  assert_uninitialized_errors
    {|
      def f():
        try:
          x = 1
          return
        finally:
          print(x)
    |}
    ["Uninitialized local [61]: Local variable `x` is undefined, or not always defined."];
  (* TODO(T106611060): False positive due to CFG construction for `finally`. *)
  assert_uninitialized_errors
    {|
      def f():
        try:
          return (x := 1)
        finally:
          print(x)
    |}
    ["Uninitialized local [61]: Local variable `x` is undefined, or not always defined."];
  assert_uninitialized_errors
    {|
      def may_raise() -> bool:
        if 1 > 2:
          raise Exception()
        else:
          return True

      def f() -> bool:
        try:
          x = may_raise()
          return x
        finally:
          print(x)
    |}
    ["Uninitialized local [61]: Local variable `x` is undefined, or not always defined."];
  ()


let () = "uninitializedCheck" >::: ["simple" >:: test_simple; "cfg" >:: test_cfg] |> Test.run
