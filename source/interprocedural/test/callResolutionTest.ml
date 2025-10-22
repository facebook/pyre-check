(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Interprocedural
open Test
open Ast
open Core

let test_resolve_ignoring_errors context =
  let assert_resolved ~source ~expression ~expected =
    let pyre_in_context =
      ScratchProject.setup ~context ["x.py", source]
      |> ScratchProject.pyre_pysa_read_only_api
      |> PyrePysaApi.ReadOnly.from_pyre1_api
      |> PyrePysaApi.InContext.create_at_global_scope
    in
    CallResolution.resolve_ignoring_errors
      ~pyre_in_context
      ~callables_to_definitions_map:
        (CallablesSharedMemory.ReadWrite.empty () |> CallablesSharedMemory.ReadOnly.read_only)
      (Test.parse_single_expression expression)
    |> assert_equal ~printer:Type.show expected
  in
  assert_resolved
    ~source:{|
    class Data:
      def __init__(self, x: int) -> None: ...
  |}
    ~expression:"x.Data()"
    ~expected:(Type.Primitive "x.Data");
  assert_resolved
    ~source:
      {|
    from pyre_extensions import PyreReadOnly
    from typing_extensions import Self

    class Foo:
      def readonly(self: PyreReadOnly[Self]) -> PyreReadOnly[Self]:
        return self
  |}
    ~expression:"x.Foo().readonly()"
    ~expected:(Type.Primitive "x.Foo")


let test_is_nonlocal context =
  let handle = "module.py" in
  let assert_qualify ~source ~expected () =
    let strip_whitespace line =
      line |> String.to_list |> List.drop_while ~f:Char.is_whitespace |> String.of_char_list
    in
    let strip_whitespace string =
      string
      |> String.split ~on:'\n'
      |> List.map ~f:strip_whitespace
      |> List.filter ~f:(Fn.non (String.equal ""))
      |> String.concat ~sep:"\n"
    in
    let processed_source =
      source |> Test.parse ~handle |> Preprocessing.qualify |> Source.show |> strip_whitespace
    in
    let processed_expected = strip_whitespace expected in
    assert_equal ~cmp:String.equal ~printer:Fn.id processed_source processed_expected
  in
  let source =
    {|
    global_variable = 1
    def outer():
      x, y, z = "", "", ""
      def inner():
        nonlocal x
        x = "str"
        z = y
        w = global_variable
  |}
  in
  let expected =
    {|
    $local_module$global_variable = 1
    def module.outer():
      ($local_module?outer$x, $local_module?outer$y, $local_module?outer$z) = ("", "", "")
      def $local_module?outer$inner():
        nonlocal x
        $local_module?outer$x = "str"
        $local_module?outer?inner$z = $local_module?outer$y
        $local_module?outer?inner$w = $local_module$global_variable
  |}
  in
  assert_qualify ~source ~expected ();
  let project = Test.ScratchProject.setup ~context [handle, source] in
  let pyre_in_context =
    ScratchProject.pyre_pysa_read_only_api project
    |> PyrePysaApi.ReadOnly.from_pyre1_api
    |> PyrePysaApi.InContext.create_at_global_scope
  in
  let assert_nonlocal define variable () =
    variable
    |> Reference.create
    |> CallResolution.is_nonlocal ~pyre_in_context ~define:(Reference.create define)
    |> assert_true
  in
  let assert_not_nonlocal define variable () =
    variable
    |> Reference.create
    |> CallResolution.is_nonlocal ~pyre_in_context ~define:(Reference.create define)
    |> assert_false
  in
  assert_nonlocal "$local_module?outer$inner" "$local_module?outer$x" ();
  assert_nonlocal "$local_module?outer$inner" "$local_module?outer$y" ();
  assert_not_nonlocal "$local_module?outer$inner" "$local_module?outer?inner$z" ();
  assert_not_nonlocal "$local_module?outer$inner" "$local_module?outer?inner$w" ();
  assert_not_nonlocal "$local_module?outer$inner" "$local_module$global_variable" ();

  assert_not_nonlocal "$local_module$outer" "$local_module?outer$x" ();
  assert_not_nonlocal "$local_module$outer" "$local_module?outer$y" ();
  assert_not_nonlocal "$local_module$outer" "$local_module?outer$z" ();
  assert_not_nonlocal "$local_module$outer" "$local_module$global_variable" ();
  ()


let () =
  "interproceduralCallResolution"
  >::: [
         "resolve_ignoring_errors" >:: test_resolve_ignoring_errors;
         "is_nonlocal" >:: test_is_nonlocal;
       ]
  |> Test.run
