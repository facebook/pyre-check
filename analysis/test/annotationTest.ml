(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Analysis
open Annotation

let test_instantiate _ =
  assert_equal
    (instantiate (create_immutable ~final:true ~global:true Type.integer) ~constraints:(fun _ ->
         None))
    (create_immutable ~final:true ~global:true Type.integer);
  ()


let test_dequalify _ =
  assert_equal
    (dequalify Ast.Reference.Map.empty (create_immutable ~final:true ~global:true Type.integer))
    (create_immutable ~final:true ~global:true Type.integer);
  ()


let () =
  "annotation"
  >::: ["instantiate" >:: test_instantiate; "dequalify" >:: test_dequalify]
  |> Test.run
