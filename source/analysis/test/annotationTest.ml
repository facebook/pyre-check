(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Annotation
open Test

let test_instantiate _ =
  assert_equal
    (instantiate (create_immutable ~final:true Type.integer) ~constraints:(fun _ -> None))
    (create_immutable ~final:true Type.integer);
  ()


let test_dequalify _ =
  assert_equal
    (dequalify Ast.Reference.Map.empty (create_immutable ~final:true Type.integer))
    (create_immutable ~final:true Type.integer);
  ()


let global_resolution context =
  ScratchProject.setup ~context [] |> ScratchProject.build_global_resolution


let test_less_or_equal context =
  let type_less_or_equal = GlobalResolution.less_or_equal (global_resolution context) in
  assert_true
    (less_or_equal
       ~type_less_or_equal
       ~left:(Annotation.create_mutable Type.integer)
       ~right:(Annotation.create_mutable Type.integer));
  assert_true
    (less_or_equal
       ~type_less_or_equal
       ~left:(Annotation.create_mutable Type.integer)
       ~right:(Annotation.create_mutable Type.float));
  assert_false
    (less_or_equal
       ~type_less_or_equal
       ~left:(Annotation.create_mutable Type.float)
       ~right:(Annotation.create_mutable Type.integer));
  (* Mutable <= Immutable. *)
  assert_true
    (less_or_equal
       ~type_less_or_equal
       ~left:(Annotation.create_mutable Type.integer)
       ~right:(Annotation.create_immutable Type.integer));
  assert_true
    (less_or_equal
       ~type_less_or_equal
       ~left:(Annotation.create_immutable Type.integer)
       ~right:(Annotation.create_immutable Type.integer));
  assert_false
    (less_or_equal
       ~type_less_or_equal
       ~left:(Annotation.create_immutable Type.integer)
       ~right:(Annotation.create_mutable Type.integer));
  ()


let () =
  "annotation"
  >::: [
         "instantiate" >:: test_instantiate;
         "dequalify" >:: test_dequalify;
         "less_or_equal" >:: test_less_or_equal;
       ]
  |> Test.run
