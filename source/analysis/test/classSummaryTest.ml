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
open ClassSummary
open Ast
open Statement

let test_is_final _ =
  let decorator ~name =
    { Decorator.name = Node.create_with_default_location (Reference.create name); arguments = None }
  in
  let class_summary ~decorators =
    {
      name = Reference.create "foo";
      qualifier = Reference.create "bar";
      bases = [];
      decorators;
      attribute_components = Class.AttributeComponents.empty ();
    }
  in
  assert_false (ClassSummary.is_final (class_summary ~decorators:[]));
  assert_true (ClassSummary.is_final (class_summary ~decorators:[decorator ~name:"typing.final"]));
  assert_true
    (ClassSummary.is_final (class_summary ~decorators:[decorator ~name:"typing_extensions.final"]));
  ()


let () = "classSummary" >::: ["is_final" >:: test_is_final] |> Test.run
