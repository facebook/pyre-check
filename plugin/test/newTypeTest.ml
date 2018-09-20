(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Plugin

open Test


let test_transform_ast _ =
  let assert_expand ?(qualifier = "qualifier") source expected =
    let parse = parse ~qualifier:(Source.qualifier ~path:qualifier) in
    assert_source_equal
      (parse expected)
      (NewType.transform_ast (parse source))
  in
  assert_expand
    {|
      T = typing.NewType('T', int)
    |}
    {|
      class qualifier.T(int):
        def __init__(self, input: int):
          pass
    |};

  (* Don't transform non-toplevel statements. *)
  assert_expand
    {|
      def foo():
        T = typing.NewType('T', int)
    |}
    {|
      def foo():
        T = typing.NewType('T', int)
    |}


let () =
  "plugin_new_type">:::[
    "transform_ast">::test_transform_ast;
  ]
  |> Test.run
