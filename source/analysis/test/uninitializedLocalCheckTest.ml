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
    let make_dunder_enter_special = function
      | Expression.Name (Attribute ({ attribute = "__enter__"; _ } as attribute)) ->
          Expression.Name (Attribute { attribute with special = true })
      | other -> other
    in
    with_dummy_assert_origin statement
    |> Node.map ~f:(Transform.transform_expressions ~transform:make_dunder_enter_special)
  in
  let item_equal (left_statement, left_identifiers) (right_statement, right_identifiers) =
    Statement.location_insensitive_compare
      (sanitize_for_tests left_statement)
      (sanitize_for_tests right_statement)
    = 0
    && [%compare.equal: Identifier.t list] left_identifiers right_identifiers
  in
  let parse_statement_and_locals (statement_string, locals) =
    parse_single_statement statement_string, locals
  in
  assert_equal
    ~cmp:(List.equal item_equal)
    ~pp_diff:
      (diff ~print:(fun format x ->
           Format.fprintf
             format
             "%s"
             ([%sexp_of: (Statement.t * Identifier.t list) list] x |> Sexp.to_string)))
    ~printer:[%show: (Statement.t * Identifier.t list) list]
    (expected |> List.map ~f:parse_statement_and_locals)
    (Map.to_alist actual_defined_locals
    |> List.map ~f:(fun (_key, (statement, set)) -> statement, Set.to_list set))


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
      {| x = 1 |}, ["x"];
      {| raise AssertionError("error") |}, [];
      {| assert True |}, [];
      {| return x |}, ["x"];
      {| assert False |}, [];
    ];
  assert_defined_locals
    {|
      def f():
        x = y
        y = 5
    |}
    [{| y = 5 |}, ["x"; "y"]; {| x = y |}, ["x"]];
  assert_defined_locals
    {|
      def f(y):
        x = y
        y = 5
    |}
    [{| y = 5 |}, ["x"; "y"]; {| x = y |}, ["x"; "y"]];
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
      {| return z |}, ["x"; "y"; "z"];
      {| z = 5 |}, ["x"; "y"; "z"];
      {| return y |}, ["x"];
      {| assert x > 5 |}, ["x"];
      {| y = 5 |}, ["x"; "y"];
      {| assert x <= 5 |}, ["x"];
    ];
  assert_defined_locals
    {|
      def f(x):
        if x > 5:
          y = 2
        return y
    |}
    [
      {| y = 2 |}, ["x"; "y"];
      {| assert x > 5 |}, ["x"];
      {| return y |}, ["x"];
      {| assert x <= 5 |}, ["x"];
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
      {| x = 1 |}, ["bad"; "x"];
      {| ZeroDivisionError |}, [];
      {| bad = 0 / 0 |}, ["bad"];
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
      {| x = 1 |}, ["_"; "x"];
      {| _ = z |}, ["_"; "x"; "y"];
      {| _ = x |}, ["_"];
      {| _ = y |}, ["_"; "x"];
      {| global y |}, [];
      {| y = 1 |}, ["_"; "x"; "y"];
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
    [{| g() |}, ["g"]; {| def g(): pass |}, ["g"]];
  assert_defined_locals
    {|
      def f():
        x, y = 0, 0
        return x, y
    |}
    [{| return (x, y) |}, ["x"; "y"]; {| x, y = 0, 0 |}, ["x"; "y"]];
  assert_defined_locals
    {|
      def f( *args, **kwargs) -> None:
        print(args)
        print(list(kwargs.items()))
    |}
    [{| print(list(kwargs.items())) |}, ["args"; "kwargs"]; {| print(args) |}, ["args"; "kwargs"]];
  assert_defined_locals
    {|
      def f() -> None:
        global x
        if x == 0:
          x = 1
    |}
    [{| x = 1 |}, ["x"]; {| assert x != 0 |}, []; {| assert x == 0 |}, []; {| global x |}, []];
  assert_defined_locals
    {|
      def f(x: str) -> None:
        assert True, x
    |}
    [{| assert True, x |}, ["x"]];
  assert_defined_locals {|
      def f():
        (x := 0)
    |} [{| (x := 0) |}, ["x"]];
  assert_defined_locals
    {|
      def f():
        ((x := 0) and (y := x))
    |}
    [{| ((x := 0) and (y := x)) |}, ["x"; "y"]];
  assert_defined_locals
    {|
      def f():
        ((y := x) and (x := 0))
    |}
    [{| ((y := x) and (x := 0)) |}, ["x"; "y"]];
  assert_defined_locals
    {|
      def f():
        [y for x in [1,2,3] if (y:=x) > 2]
    |}
    [{| [y for x in [1,2,3] if (y:=x) > 2] |}, ["y"]];
  assert_defined_locals
    {|
      def f():
        _ = x.field
        x = Foo()
    |}
    [{| x = Foo() |}, ["_"; "x"]; {| _ = x.field |}, ["_"]];
  assert_defined_locals
    {|
      def f():
        with open("x") as x:
          pass
        _ = x, y
        x, y = None, None
    |}
    [
      {| x, y = None, None |}, ["_"; "x"; "y"];
      {| pass |}, ["x"];
      {| _ = x, y |}, ["_"; "x"];
      {| x = open("x").__enter__() |}, ["x"];
    ];
  assert_defined_locals
    {|
      def f():
        y = [x for x in x]
        x = []
    |}
    [{| x = [] |}, ["x"; "y"]; {| y = [x for x in x] |}, ["y"]];
  assert_defined_locals
    {|
      def f():
        _  = [(x, y) for x,y in []]
        x = None
        _ = x, y
        y = None
    |}
    [
      {| _ = x, y |}, ["_"; "x"];
      {| x = None |}, ["_"; "x"];
      {| y = None |}, ["_"; "x"; "y"];
      {| _  = [(x, y) for x,y in []] |}, ["_"];
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
      {| x = 1 |}, ["x"];
      {| raise AssertionError("error") |}, [];
      {| assert True |}, [];
      {| return x |}, ["x"];
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
      {| y = 1 |}, ["y"];
      {| assert False, "error" |}, [];
      {| assert True |}, [];
      {| return y |}, ["y"];
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
      {| z = 1 |}, ["z"];
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
      {| b = 1 |}, ["b"];
      {| assert True |}, [];
      {| break |}, ["b"];
      {| return b |}, ["b"];
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
    [{| x = 2 |}, ["x"]; {| print(x) |}, ["x"]; {| Exception |}, []; {| x = 1 |}, ["x"]];
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
    [{| return |}, ["x"]; {| print(x) |}, []; {| x = 1 |}, ["x"]];
  (* TODO(T106611060): `x` isn't defined at `print(x)` due to CFG construction for `finally`. *)
  assert_defined_locals
    {|
      def f():
        try:
          return (x := 1)
        finally:
          print(x)
    |}
    [{| print(x) |}, []; {| return (x := 1) |}, ["x"]];
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
    [{| return x |}, ["x"]; {| print(x) |}, []; {| x = may_raise() |}, ["x"]];
  ()


let () =
  "uninitializedCheck"
  >::: [
         "simple" >:: test_simple;
         "cfg" >:: test_cfg;
         "defined_locals_at_each_statement" >:: test_defined_locals_at_each_statement;
       ]
  |> Test.run
