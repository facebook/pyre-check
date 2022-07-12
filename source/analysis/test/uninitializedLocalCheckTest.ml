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
open Ast

let assert_uninitialized_errors ~context =
  let check ~environment:_ ~source = UninitializedLocalCheck.check_module_for_testing ~source in
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

  (* Nested classes and defines *)
  assert_uninitialized_errors
    {|
      def f() -> None:
        x = 1
        def nested_f() -> None:
          a = b
    |}
    [];
  assert_uninitialized_errors
    {|
      def f() -> None:
        x = 1
        class Nested:
          def __init__(self) -> None:
            self.a = b
    |}
    [];

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


let assert_defined_locals source expected =
  let define = parse_single_define source in
  let open Statement in
  let open Expression in
  let open UninitializedLocalCheck in
  let actual_defined_locals =
    define |> Node.create_with_default_location |> defined_locals_at_each_statement
  in
  let sanitize_for_tests statement =
    (* Assert origin is `Assertion` if we parse "assert x" but `If` if we parse `if x: ...`. Just
       use a dummy origin. *)
    let with_dummy_assert_origin = function
      | { Node.value = Statement.Assert assert_; _ } as statement ->
          { statement with Node.value = Statement.Assert { assert_ with origin = Assertion } }
      | other -> other
    in
    (* Parsing `foo().__enter__` makes it have `special=false`. Just set it directly. *)
    let make_dunder_attribute_special = function
      | Expression.Name
          (Attribute ({ attribute = "__enter__" | "__next__" | "__iter__"; _ } as attribute)) ->
          Expression.Name (Attribute { attribute with special = true })
      | other -> other
    in
    with_dummy_assert_origin statement
    |> Node.map ~f:(Transform.transform_in_statement ~transform:make_dunder_attribute_special)
  in
  let item_equal (left_statement, left_identifiers) (right_statement, right_identifiers) =
    Statement.location_insensitive_compare
      (sanitize_for_tests left_statement)
      (sanitize_for_tests right_statement)
    = 0
    && [%compare.equal: (Identifier.t * Location.t) list] left_identifiers right_identifiers
  in
  let parse_statement_and_locals (statement_string, locals_with_locations) =
    ( parse_single_statement statement_string,
      List.map locals_with_locations ~f:(fun (local, location) -> local, parse_location location) )
  in
  let actual =
    let extract_name_and_location (_key, (statement, identifier_to_binding)) =
      ( statement,
        Map.data identifier_to_binding
        |> List.map ~f:(fun { Scope.Binding.name; location; _ } -> name, location) )
    in
    Map.to_alist actual_defined_locals |> List.map ~f:extract_name_and_location
  in
  assert_equal
    ~cmp:(List.equal item_equal)
    ~printer:[%show: (Statement.t * (Identifier.t * Location.t) list) list]
    (expected |> List.map ~f:parse_statement_and_locals)
    actual


(* Note: We currently compute locals that are defined *after* each statement. *)
let test_defined_locals_at_each_statement _ =
  assert_defined_locals
    {|
      def f():
        if True:
          x = 1
        else:
          raise AssertionError("error")
        return x
    |}
    [
      {| x = 1 |}, ["x", "4:4-4:5"];
      {| raise AssertionError("error") |}, [];
      {| assert True |}, [];
      {| return x |}, ["x", "4:4-4:5"];
      {| assert False |}, [];
    ];
  assert_defined_locals
    {|
      def f():
        x = y
        y = 5
    |}
    [{| y = 5 |}, ["x", "3:2-3:3"; "y", "4:2-4:3"]; {| x = y |}, ["x", "3:2-3:3"]];
  assert_defined_locals
    {|
      def f(y):
        x = y
        y = 5
    |}
    [{| y = 5 |}, ["x", "3:2-3:3"; "y", "4:2-4:3"]; {| x = y |}, ["x", "3:2-3:3"; "y", "2:6-2:7"]];
  assert_defined_locals
    {|
      def f(x):
        if x > 5:
          return y
        y = 5
        z = 5
        return z
    |}
    [
      {| return z |}, ["x", "2:6-2:7"; "y", "5:2-5:3"; "z", "6:2-6:3"];
      {| z = 5 |}, ["x", "2:6-2:7"; "y", "5:2-5:3"; "z", "6:2-6:3"];
      {| return y |}, ["x", "2:6-2:7"];
      {| assert x > 5 |}, ["x", "2:6-2:7"];
      {| y = 5 |}, ["x", "2:6-2:7"; "y", "5:2-5:3"];
      {| assert x <= 5 |}, ["x", "2:6-2:7"];
    ];
  assert_defined_locals
    {|
      def f(x):
        if x > 5:
          y = 2
        return y
    |}
    [
      {| y = 2 |}, ["x", "2:6-2:7"; "y", "4:4-4:5"];
      {| assert x > 5 |}, ["x", "2:6-2:7"];
      {| return y |}, ["x", "2:6-2:7"];
      {| assert x <= 5 |}, ["x", "2:6-2:7"];
    ];
  assert_defined_locals
    {|
      def f() -> int:
          try:
              bad = 0 / 0
              x = 1
          except ZeroDivisionError:
              return x
    |}
    [
      {| return x |}, [];
      {| x = 1 |}, ["bad", "4:8-4:11"; "x", "5:8-5:9"];
      {| ZeroDivisionError |}, [];
      {| bad = 0 / 0 |}, ["bad", "4:8-4:11"];
    ];
  assert_defined_locals
    {|
      def access_global() -> int:
        global y
        _ = x      # Refers to local `x` (defined below)
        x = 1
        _ = y      # Refers to global `y`
        y = 1
        _ = z      # Refers to global `z`
    |}
    [
      {| x = 1 |}, ["_", "4:2-4:3"; "x", "5:2-5:3"];
      {| _ = z |}, ["_", "8:2-8:3"; "x", "5:2-5:3"; "y", "7:2-7:3"];
      {| _ = x |}, ["_", "4:2-4:3"];
      {| _ = y |}, ["_", "6:2-6:3"; "x", "5:2-5:3"];
      {| global y |}, [];
      {| y = 1 |}, ["_", "6:2-6:3"; "x", "5:2-5:3"; "y", "7:2-7:3"];
    ];
  assert_defined_locals {|
      def f():
        return Foo()
    |} [{| return Foo() |}, []];
  assert_defined_locals
    {|
      def f():
        def g():
          pass
        g()
    |}
    [{| g() |}, ["g", "3:2-4:8"]; {| def g(): pass |}, ["g", "3:2-4:8"]];
  assert_defined_locals
    {|
      def f():
        x, y = 0, 0
        return x, y
    |}
    [
      {| return (x, y) |}, ["x", "3:2-3:3"; "y", "3:5-3:6"];
      {| x, y = 0, 0 |}, ["x", "3:2-3:3"; "y", "3:5-3:6"];
    ];
  assert_defined_locals
    {|
      def f( *args, **kwargs) -> None:
        print(args)
        print(list(kwargs.items()))
    |}
    [
      {| print(list(kwargs.items())) |}, ["args", "2:8-2:12"; "kwargs", "2:16-2:22"];
      {| print(args) |}, ["args", "2:8-2:12"; "kwargs", "2:16-2:22"];
    ];
  assert_defined_locals
    {|
      def f() -> None:
        global x
        if x == 0:
          x = 1
    |}
    [
      {| x = 1 |}, ["x", "5:4-5:5"];
      {| assert x != 0 |}, [];
      {| assert x == 0 |}, [];
      {| global x |}, [];
    ];
  assert_defined_locals
    {|
      def f(x: str) -> None:
        assert True, x
    |}
    [{| assert True, x |}, ["x", "2:6-2:12"]];
  assert_defined_locals {|
      def f():
        (x := 0)
    |} [{| (x := 0) |}, ["x", "3:3-3:4"]];
  assert_defined_locals
    {|
      def f():
        ((x := 0) and (y := x))
    |}
    [{| ((x := 0) and (y := x)) |}, ["x", "3:4-3:5"; "y", "3:17-3:18"]];
  assert_defined_locals
    {|
      def f():
        ((y := x) and (x := 0))
    |}
    [{| ((y := x) and (x := 0)) |}, ["x", "3:17-3:18"; "y", "3:4-3:5"]];
  assert_defined_locals
    {|
      def f():
        [y for x in [1,2,3] if (y:=x) > 2]
    |}
    [{| [y for x in [1,2,3] if (y:=x) > 2] |}, ["y", "3:26-3:27"]];
  assert_defined_locals
    {|
      def f():
        _ = x.field
        x = Foo()
    |}
    [{| x = Foo() |}, ["_", "3:2-3:3"; "x", "4:2-4:3"]; {| _ = x.field |}, ["_", "3:2-3:3"]];
  assert_defined_locals
    {|
      def f():
        with open("x", "1:2-3:4") as x:
          pass
        _ = x, y
        x, y = None, None
    |}
    [
      {| x, y = None, None |}, ["_", "5:2-5:3"; "x", "6:2-6:3"; "y", "6:5-6:6"];
      {| pass |}, ["x", "3:31-3:32"];
      {| _ = x, y |}, ["_", "5:2-5:3"; "x", "3:31-3:32"];
      {| x = open("x", "1:2-3:4").__enter__() |}, ["x", "3:31-3:32"];
    ];
  assert_defined_locals
    {|
      def f():
        y = [x for x in x]
        x = []
    |}
    [{| x = [] |}, ["x", "4:2-4:3"; "y", "3:2-3:3"]; {| y = [x for x in x] |}, ["y", "3:2-3:3"]];
  assert_defined_locals
    {|
      def f():
        _  = [(x, y) for x,y in []]
        x = None
        _ = x, y
        y = None
    |}
    [
      {| _ = x, y |}, ["_", "5:2-5:3"; "x", "4:2-4:3"];
      {| x = None |}, ["_", "3:2-3:3"; "x", "4:2-4:3"];
      {| y = None |}, ["_", "5:2-5:3"; "x", "4:2-4:3"; "y", "6:2-6:3"];
      {| _  = [(x, y) for x,y in []] |}, ["_", "3:2-3:3"];
    ];
  assert_defined_locals
    {|
      def f():
        if True:
          x = 1
        else:
          raise AssertionError("error")
        return x
    |}
    [
      {| x = 1 |}, ["x", "4:4-4:5"];
      {| raise AssertionError("error") |}, [];
      {| assert True |}, [];
      {| return x |}, ["x", "4:4-4:5"];
      {| assert False |}, [];
    ];
  assert_defined_locals
    {|
      def g():
        if True:
          y = 1
        else:
          assert False, "error"
        return y
    |}
    [
      {| y = 1 |}, ["y", "4:4-4:5"];
      {| assert False, "error" |}, [];
      {| assert True |}, [];
      {| return y |}, ["y", "4:4-4:5"];
      {| assert False |}, [];
    ];
  assert_defined_locals
    {|
      def h():
        if True:
          z = 1
        else:
          assert True, "error"
        return z
    |}
    [
      {| z = 1 |}, ["z", "4:4-4:5"];
      {| assert True, "error" |}, [];
      {| assert True |}, [];
      {| return z |}, [];
      {| assert False |}, [];
    ];
  assert_defined_locals
    {|
      def baz() -> int:
          while True:
              b = 1
              break
          return b
    |}
    [
      {| b = 1 |}, ["b", "4:8-4:9"];
      {| assert True |}, [];
      {| break |}, ["b", "4:8-4:9"];
      {| return b |}, ["b", "4:8-4:9"];
      {| assert False |}, [];
    ];
  assert_defined_locals
    {|
      def f():
        try:
          x = 1
        except Exception:
          x = 2
        finally:
          print(x)
    |}
    [
      {| x = 2 |}, ["x", "6:4-6:5"];
      {| print(x) |}, ["x", "4:4-4:5"];
      {| Exception |}, [];
      {| x = 1 |}, ["x", "4:4-4:5"];
    ];
  (* TODO(T106611060): `x` isn't defined at `print(x)` due to CFG construction for `finally`. *)
  assert_defined_locals
    {|
      def f():
        try:
          x = 1
          return
        finally:
          print(x)
    |}
    [{| return |}, ["x", "4:4-4:5"]; {| print(x) |}, []; {| x = 1 |}, ["x", "4:4-4:5"]];
  (* TODO(T106611060): `x` isn't defined at `print(x)` due to CFG construction for `finally`. *)
  assert_defined_locals
    {|
      def f():
        try:
          return (x := 1)
        finally:
          print(x)
    |}
    [{| print(x) |}, []; {| return (x := 1) |}, ["x", "4:12-4:13"]];
  assert_defined_locals
    {|
      def may_raise() -> bool:
        if 1 > 2:
          raise Exception()
        else:
          return True
    |}
    [
      {| raise Exception() |}, [];
      {| return True |}, [];
      {| assert 1 > 2 |}, [];
      {| assert 1 <= 2 |}, [];
    ];
  assert_defined_locals
    {|
      def f() -> bool:
        try:
          x = may_raise()
          return x
        finally:
          print(x)
    |}
    [{| return x |}, ["x", "4:4-4:5"]; {| print(x) |}, []; {| x = may_raise() |}, ["x", "4:4-4:5"]];
  assert_defined_locals
    {|
      def foo(x: str) -> None:
        for x in [1]:
          print(x)
        print(x, "outside")
    |}
    [
      {| print(x) |}, ["x", "3:6-3:7"];
      {| x = [1].__iter__().__next__() |}, ["x", "3:6-3:7"];
      {| print(x, "outside") |}, ["x", "2:8-2:14"];
    ];
  ()


(* All local actions that lead to jump conditions are reflected statically in the CFG, so that for
   example raising an uncaught exception in an if/else will cause that branch to be ignored in the
   join, which means that branch can fail to define locals used lower in the function.

   The one scenario where the CFG cannot do the right thing is a call to a function annotated with
   `typing.NoReturn`. This should be handled exactly like an exception, but the CFG does not have
   type information and therefore cannot account for it.

   As a result, it merits a separate test suite because the implementation is separate from the
   implementation of langauge features that can be represented in the CFG. *)
let test_no_return context =
  let assert_uninitialized_errors = assert_uninitialized_errors ~context in
  assert_uninitialized_errors
    {|
      from typing import NoReturn

      def does_not_return() -> NoReturn:
          raise Exception()

      def foo(flag: bool) -> None:
          if flag:
              x = 5
          else:
              does_not_return()
          print(x)
    |}
    ["Uninitialized local [61]: Local variable `x` is undefined, or not always defined."];
  assert_uninitialized_errors
    {|
      from typing import NoReturn

      async def does_not_return() -> NoReturn:
          raise Exception()

      async def foo(flag: bool) -> None:
          if flag:
              x = 5
          else:
              await does_not_return()
          print(x)
    |}
    ["Uninitialized local [61]: Local variable `x` is undefined, or not always defined."];
  ()


let () =
  "uninitializedCheck"
  >::: [
         "simple" >:: test_simple;
         "cfg" >:: test_cfg;
         "defined_locals_at_each_statement" >:: test_defined_locals_at_each_statement;
         "no_return" >:: test_no_return;
       ]
  |> Test.run
