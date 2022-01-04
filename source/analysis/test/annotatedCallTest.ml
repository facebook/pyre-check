(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Ast
open Test

let test_redirect context =
  let assert_redirects ~source call expected =
    Callgraph.DefaultBuilder.initialize ();
    let resolution =
      ScratchProject.setup ["test.py", source] ~include_typeshed_stubs:true ~context
      |> ScratchProject.build_resolution
    in
    let parse call =
      match parse_single_expression ~coerce_special_methods:true call |> Node.value with
      | Call call -> call
      | _ -> failwith "not call"
    in
    let printer call = Expression.Call.sexp_of_t call |> Sexp.to_string_hum in
    let cmp left right = Int.equal 0 (Expression.Call.location_insensitive_compare left right) in
    assert_equal
      ~cmp
      ~printer
      (Annotated.Call.redirect_special_calls ~resolution (parse call))
      (parse expected)
  in
  assert_redirects
    ~source:
      {|
      from typing import Callable
      class HasDunderStr:
        __str__: Callable("name")[[int], str]
     |}
    "str(test.HasDunderStr())"
    "test.HasDunderStr().__str__()";
  assert_redirects
    ~source:
      {|
      from typing import Callable
      class HasDunderStr:
        __str__: BoundMethod[Callable("name")[[int], str], HasDunderStr]
     |}
    "str(test.HasDunderStr())"
    "test.HasDunderStr().__str__()";
  ()


let () = "call" >::: ["redirect" >:: test_redirect] |> Test.run
