(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Test


let assert_constant_propagation source expected =
  let environment = environment () in
  let configuration = mock_configuration in
  let actual =
    let source = parse source in
    TypeCheck.run ~configuration ~environment ~source |> ignore;
    DeobfuscationCheck.run ~configuration ~environment ~source
    |> function
    | [{ Error.kind = Error.Deobfuscation actual; _ }] -> actual
    | _ -> failwith "Did not generate a source"
  in
  assert_equal ~cmp:Source.equal ~printer:Source.show (parse expected) actual


let test_forward _ =
  (* Basic propagation. *)
  assert_constant_propagation
    {|
      a = 1
      b = a
      c = b
      c
    |}
    {|
      1
    |};
  assert_constant_propagation
    {|
      a = 'string'
      a
    |}
    {|
      'string'
    |};
  assert_constant_propagation
    {|
      a = True
      a
    |}
    {|
      True
    |};
  assert_constant_propagation
    {|
      a = None
      a
    |}
    {|
      None
    |};
  assert_constant_propagation
    {|
      a = 1
      a = 2
      a
    |}
    {|
      a = 1
      2
    |};
  assert_constant_propagation
    {|
      a = 1
      foo(a)
    |}
    {|
      foo(1)
    |};

  (* Deletion. *)
  assert_constant_propagation
    {|
      a = 1
      a = foo()
      b = a
      b
    |}
    {|
      a = 1
      a = foo()
      b = a
      b
    |};
  assert_constant_propagation
    {|
      a = 1
      a = a + 1
      a
    |}
    {|
      a = 1
      a = 1 + 1
      a
    |};

  (* Control flow. *)
  assert_constant_propagation
    {|
      if True:
        a = 1
      else:
        a = 2
      a
    |}
    {|
      if True:
        a = 1
      else:
        a = 2
      a
    |};
  assert_constant_propagation
    {|
      if True:
        a = 1
      else:
        a = 1
      a
    |}
    {|
      if True:
        pass
      else:
        pass
      1
    |};

  (* Functions. *)
  assert_constant_propagation
    {|
      a = len
      a
    |}
    {|
      len
    |};
  assert_constant_propagation
    {|
      a = len
      a(b)
    |}
    {|
      len(b)
    |};
  assert_constant_propagation
    {|
      a = len
      a(b).imag
    |}
    {|
      len(b).imag
    |};

  (* Constructors. *)
  assert_constant_propagation
    {|
      import threading
      t = threading.Thread
      t()
    |}
    {|
      import threading
      threading.Thread()
    |};

  (* Global constants. *)
  assert_constant_propagation
    {|
      import logging
      d = logging.DEBUG
      d
      i = logging.INFO_1
      i
    |}
    {|
      import logging
      logging.DEBUG
      logging.INFO_1
    |}


let test_scheduling _ =
  assert_constant_propagation
    {|
      a = 1
      def nested():
        b
    |}
    {|
      def nested():
        b
    |};
  assert_constant_propagation
    {|
      def nested():
        a = 1
        a
    |}
    {|
      def nested():
        1
    |};
  assert_constant_propagation
    {|
      def nested():
        def nested():
          a = 1
          a
    |}
    {|
      def nested():
        def nested():
          1
    |};
  assert_constant_propagation
    {|
      def nested():
        a = 1
        def nested():
          a
    |}
    {|
      def nested():
        def nested():
          1
    |};
  assert_constant_propagation
    {|
      def nested():
        def nested():
          a
        a = 1
    |}
    {|
      def nested():
        def nested():
          a
    |}


let () =
  "DeobfuscationCheck">:::[
    "forward">::test_forward;
    "scheduling">::test_scheduling;
  ]
  |> Test.run
