(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Analysis
open Test


let assert_deobfuscation source expected =
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
  assert_deobfuscation
    {|
      a = 1
      b = a
      c = b
      c
    |}
    {|
      1
    |};
  assert_deobfuscation
    {|
      a = 'string'
      a
    |}
    {|
      'string'
    |};
  assert_deobfuscation
    {|
      a = True
      a
    |}
    {|
      True
    |};
  assert_deobfuscation
    {|
      a = None
      a
    |}
    {|
      None
    |};
  assert_deobfuscation
    {|
      a = 1
      a = 2
      a
    |}
    {|
      a = 1
      2
    |};
  assert_deobfuscation
    {|
      a = 1
      foo(a)
    |}
    {|
      foo(1)
    |};

  (* Deletion. *)
  assert_deobfuscation
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
  assert_deobfuscation
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
  assert_deobfuscation
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
  assert_deobfuscation
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
      1
    |};

  (* Assertions. *)
  assert_deobfuscation
    {|
      a = False
      assert a
    |}
    {|
      assert False
    |};
  assert_deobfuscation
    {|
      a = False
      if a:
        pass
    |}
    {|
      if False:
        pass
    |};

  (* Functions. *)
  assert_deobfuscation
    {|
      a = len
      a
    |}
    {|
      len
    |};
  assert_deobfuscation
    {|
      a = len
      a(b)
    |}
    {|
      len(b)
    |};
  assert_deobfuscation
    {|
      a = len
      a(b).imag
    |}
    {|
      len(b).imag
    |};

  (* Constructors. *)
  assert_deobfuscation
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
  assert_deobfuscation
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
  assert_deobfuscation
    {|
      a = 1
      def nested():
        b
    |}
    {|
      def nested():
        b
    |};
  assert_deobfuscation
    {|
      def nested():
        a = 1
        a
    |}
    {|
      def nested():
        1
    |};
  assert_deobfuscation
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
  assert_deobfuscation
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
  assert_deobfuscation
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


let test_fixup _ =
  (* Fix empty bodies. *)
  assert_deobfuscation
    {|
      if True:
        dead = 1
    |}
    {|
      if True:
        pass
    |};
  assert_deobfuscation
    {|
      def foo():
        dead = 1
    |}
    {|
      def foo():
        pass
    |};

  (* Remove docstrings. *)
  assert_deobfuscation
    {|
      def foo():
        "docstring"
        pass
    |}
    {|
      def foo():
        pass
    |};

  (* Sanitize accesses. *)
  assert_deobfuscation
    {|
      $local_qualifier$variable
    |}
    {|
      variable
    |};
  assert_deobfuscation
    {|
      def foo($parameter$parameter):
        pass
    |}
    {|
      def foo(parameter):
        pass
    |};

  (* Naming heuristics. *)
  assert_deobfuscation
    {|
      if True:
        FafJsUlzgBbRAOWSEqDLIQvnVrMkhCjGeXwioHKPutxTmNpdc = 1
      FafJsUlzgBbRAOWSEqDLIQvnVrMkhCjGeXwioHKPutxTmNpdc
      if True:
        FafJsUlzgBbRAOWSEqDLIQvnVrMkhCjGeXwioHKPutxTmNpYd = 2
      FafJsUlzgBbRAOWSEqDLIQvnVrMkhCjGeXwioHKPutxTmNpYd
    |}
    {|
      if True:
        a = 1
      a
      if True:
        b = 2
      b
    |};
  assert_deobfuscation
    {|
      def FafJsUlzgBbRAOWSEqDLIQvnVrMkhCjGeXwioHKPutxTmNpdc():
        pass
      FafJsUlzgBbRAOWSEqDLIQvnVrMkhCjGeXwioHKPutxTmNpdc()
    |}
    {|
      def a():
        pass
      a()
    |};
  assert_deobfuscation
    {|
      def qualifier.FafJsUlzgBbRAOWSEqDLIQvnVrMkhCjGeXwioHKPutxTmNpdc():
        pass
      qualifier.FafJsUlzgBbRAOWSEqDLIQvnVrMkhCjGeXwioHKPutxTmNpdc()
    |}
    {|
      def qualifier.a():
        pass
      qualifier.a()
    |};
  assert_deobfuscation
    {|
      FafJsUlzgBbRAOWSEqDLIQvnVrMkhCjGeXwioHKPutxTmNpdc()
      def FafJsUlzgBbRAOWSEqDLIQvnVrMkhCjGeXwioHKPutxTmNpdc():
        pass
    |}
    {|
      FafJsUlzgBbRAOWSEqDLIQvnVrMkhCjGeXwioHKPutxTmNpdc()  # current limitation
      def a():
        pass
    |}



let () =
  "deobfuscation">:::[
    "forward">::test_forward;
    "scheduling">::test_scheduling;
    "fixup">::test_fixup;
  ]
  |> Test.run
