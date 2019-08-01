(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Analysis
open Annotation
open Refinement
open Test

let resolution context =
  let _, _, environment = ScratchProject.setup ~context [] |> ScratchProject.build_environment in
  Environment.resolution environment ()


let test_refine context =
  let resolution = resolution context in
  assert_equal
    (refine ~resolution (create_immutable ~global:false Type.float) Type.integer)
    (create_immutable ~global:false ~original:(Some Type.float) Type.integer);
  assert_equal
    (refine ~resolution (create_immutable ~global:false Type.integer) Type.float)
    (create_immutable ~global:false Type.integer);
  assert_equal
    (refine ~resolution (create_immutable ~global:false Type.integer) Type.Bottom)
    (create_immutable ~global:false Type.integer);
  assert_equal
    (refine ~resolution (create_immutable ~global:false Type.integer) Type.Top)
    (create_immutable ~global:false ~original:(Some Type.integer) Type.Top)


let test_less_or_equal context =
  let resolution = resolution context in
  (* Type order is preserved. *)
  assert_true (less_or_equal ~resolution (create Type.integer) (create Type.integer));
  assert_true (less_or_equal ~resolution (create Type.integer) (create Type.float));
  assert_false (less_or_equal ~resolution (create Type.float) (create Type.integer));

  (* Mutable <= Local <= Local. *)
  assert_true
    (less_or_equal ~resolution (create Type.integer) (create_immutable ~global:false Type.integer));
  assert_true
    (less_or_equal
       ~resolution
       (create_immutable ~global:false Type.integer)
       (create_immutable ~global:false Type.integer));
  assert_true
    (less_or_equal
       ~resolution
       (create_immutable ~global:false Type.integer)
       (create_immutable ~global:true Type.integer));
  assert_false
    (less_or_equal ~resolution (create_immutable ~global:false Type.integer) (create Type.integer));
  assert_false
    (less_or_equal
       ~resolution
       (create_immutable ~global:true Type.integer)
       (create_immutable ~global:false Type.integer))


let test_join context =
  let resolution = resolution context in
  (* Type order is preserved. *)
  assert_equal (join ~resolution (create Type.integer) (create Type.integer)) (create Type.integer);
  assert_equal (join ~resolution (create Type.integer) (create Type.float)) (create Type.float);

  (* Mutability. *)
  assert_equal
    (join ~resolution (create Type.integer) (create_immutable ~global:false Type.integer))
    (create_immutable ~global:false Type.integer);
  assert_equal
    (join ~resolution (create_immutable ~global:false Type.integer) (create Type.integer))
    (create_immutable ~global:false Type.integer);
  assert_equal
    (join
       ~resolution
       (create_immutable ~global:false Type.integer)
       (create_immutable ~global:false Type.integer))
    (create_immutable ~global:false Type.integer);
  assert_equal
    (join
       ~resolution
       (create_immutable ~global:true Type.float)
       (create_immutable ~global:false Type.integer))
    (create_immutable ~global:true Type.float);
  assert_equal
    (join
       ~resolution
       (create_immutable ~global:true Type.float)
       (create_immutable ~global:true Type.integer))
    (create_immutable ~global:true Type.float)


let test_meet context =
  let resolution = resolution context in
  (* Type order is preserved. *)
  assert_equal (meet ~resolution (create Type.integer) (create Type.integer)) (create Type.integer);
  assert_equal (meet ~resolution (create Type.integer) (create Type.float)) (create Type.integer);

  (* Mutability. *)
  assert_equal
    (meet ~resolution (create Type.integer) (create_immutable ~global:false Type.integer))
    (create Type.integer);
  assert_equal
    (meet ~resolution (create_immutable ~global:false Type.integer) (create Type.integer))
    (create Type.integer);
  assert_equal
    (meet
       ~resolution
       (create_immutable ~global:false Type.integer)
       (create_immutable ~global:false Type.integer))
    (create_immutable ~global:false Type.integer);
  assert_equal
    (meet
       ~resolution
       (create_immutable ~global:true Type.float)
       (create_immutable ~global:false Type.integer))
    (create_immutable ~global:false Type.integer)


let () =
  "annotation"
  >::: [ "refine" >:: test_refine;
         "less_or_equal" >:: test_less_or_equal;
         "join" >:: test_join;
         "meet" >:: test_meet ]
  |> Test.run
