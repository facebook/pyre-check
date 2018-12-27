(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Test


let test_forward _ =
  let assert_constant_propagation source expected =
    let environment = environment () in
    let configuration = mock_configuration in
    let actual =
      ConstantPropagationCheck.run ~configuration ~environment ~source:(parse source)
      |> function
      | [{ Error.kind = Error.ConstantPropagation actual; _ }] -> actual
      | _ -> failwith "Did not generate a source"
    in
    assert_equal ~cmp:Source.equal ~printer:Source.show (parse expected) actual
  in

  (* Basic propagation. *)
  assert_constant_propagation
    {|
      a = 1
      b = a
      c = b
    |}
    {|
      a = 1
      b = 1
      c = 1
    |};
  assert_constant_propagation
    {|
      a = 'string'
      b = a
    |}
    {|
      a = 'string'
      b = 'string'
    |};
  assert_constant_propagation
    {|
      a = True
      b = a
    |}
    {|
      a = True
      b = True
    |};
  assert_constant_propagation
    {|
      a = 1
      a = 2
      b = a
    |}
    {|
      a = 1
      a = 2
      b = 2
    |};
  assert_constant_propagation
    {|
      a = 1
      foo(a)
    |}
    {|
      a = 1
      foo(1)
    |};

  (* Deletion. *)
  assert_constant_propagation
    {|
      a = 1
      a = foo()
      b = a
    |}
    {|
      a = 1
      a = foo()
      b = a
    |};

  (* Control flow. *)
  assert_constant_propagation
    {|
      if True:
        a = 1
      else:
        a = 2
      b = a
    |}
    {|
      if True:
        a = 1
      else:
        a = 2
      b = a
    |};
  assert_constant_propagation
    {|
      if True:
        a = 1
      else:
        a = 1
      b = a
    |}
    {|
      if True:
        a = 1
      else:
        a = 1
      b = 1
    |}


let () =
  "constantPropagationCheck">:::[
    "forward">::test_forward;
  ]
  |> Test.run
