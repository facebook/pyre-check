(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open RefinementUnit
open Test

let test_create _ =
  let assert_create ~refinement_unit ~expected =
    assert_equal ~cmp:(Option.equal Annotation.equal) (base refinement_unit) expected
  in
  assert_create ~refinement_unit:empty ~expected:None;
  assert_create
    ~refinement_unit:(create (Annotation.create_mutable Type.Bottom))
    ~expected:(Some (Annotation.create_mutable Type.Bottom));
  assert_create
    ~refinement_unit:(create (Annotation.create_mutable Type.object_primitive))
    ~expected:(Some (Annotation.create_mutable (Type.Primitive "object")));
  ()


let test_add_attribute_refinement _ =
  let assert_attribute_refinement ~refinement_unit ~reference ~expected =
    assert_equal
      ~cmp:(Option.equal Annotation.equal)
      (annotation refinement_unit ~reference)
      expected
  in
  let refinement_unit =
    add_attribute_refinement
      empty
      ~reference:!&"a.b.c.d"
      ~annotation:(Annotation.create_mutable Type.integer)
  in
  assert_attribute_refinement ~refinement_unit ~reference:!&"a" ~expected:None;
  assert_attribute_refinement ~refinement_unit ~reference:!&"a.b" ~expected:None;
  assert_attribute_refinement ~refinement_unit ~reference:!&"a.b.c" ~expected:None;
  assert_attribute_refinement
    ~refinement_unit
    ~reference:!&"a.b.c.d"
    ~expected:(Some (Annotation.create_mutable (Type.Primitive "int")));
  assert_attribute_refinement ~refinement_unit ~reference:!&"a.b.c.d.e" ~expected:None;

  assert_attribute_refinement
    ~refinement_unit:
      (add_attribute_refinement
         refinement_unit
         ~reference:!&"a.b.c.d"
         ~annotation:(Annotation.create_mutable Type.bool))
    ~reference:!&"a.b.c.d"
    ~expected:(Some (Annotation.create_mutable (Type.Primitive "bool")));
  ()


let resolution context = ScratchProject.setup ~context [] |> ScratchProject.build_global_resolution

let add_mutable_attribute_refinement reference type_ refinement_unit =
  add_attribute_refinement refinement_unit ~reference ~annotation:(Annotation.create_mutable type_)


let test_less_or_equal context =
  let global_resolution = resolution context in
  (* Bases are compared *)
  assert_true
    (less_or_equal ~global_resolution (create_mutable Type.integer) (create_mutable Type.integer));
  assert_false
    (less_or_equal ~global_resolution (create_mutable Type.float) (create_mutable Type.integer));
  (* Attributes are compared *)
  assert_true
    (less_or_equal
       ~global_resolution
       (create_mutable Type.object_primitive
       |> add_mutable_attribute_refinement !&"a.x" Type.integer)
       (create_mutable Type.object_primitive
       |> add_mutable_attribute_refinement !&"a.x" Type.integer));
  assert_true
    (less_or_equal
       ~global_resolution
       (create_mutable Type.object_primitive
       |> add_mutable_attribute_refinement !&"a.x" Type.integer)
       (create_mutable Type.object_primitive |> add_mutable_attribute_refinement !&"a.x" Type.float));
  assert_false
    (less_or_equal
       ~global_resolution
       (create_mutable Type.object_primitive |> add_mutable_attribute_refinement !&"a.x" Type.float)
       (create_mutable Type.object_primitive
       |> add_mutable_attribute_refinement !&"a.x" Type.integer));
  (* Nested attributes are compared *)
  assert_false
    (less_or_equal
       ~global_resolution
       (create_mutable Type.object_primitive
       |> add_mutable_attribute_refinement !&"a.x" Type.object_primitive)
       (create_mutable Type.object_primitive
       |> add_mutable_attribute_refinement !&"a.x" Type.object_primitive
       |> add_mutable_attribute_refinement !&"a.x.b" Type.integer));
  ()


let test_join context =
  let global_resolution = resolution context in
  (* Bases are compared *)
  assert_equal
    (join ~global_resolution (create_mutable Type.integer) (create_mutable Type.integer))
    (create_mutable Type.integer);
  assert_equal
    (join ~global_resolution (create_mutable Type.integer) (create_mutable Type.float))
    (create_mutable Type.float);
  (* Attributes are compared *)
  assert_equal
    (join
       ~global_resolution
       (create_mutable Type.object_primitive
       |> add_mutable_attribute_refinement !&"a.x" Type.integer)
       (create_mutable Type.object_primitive
       |> add_mutable_attribute_refinement !&"a.x" Type.integer))
    (create_mutable Type.object_primitive |> add_mutable_attribute_refinement !&"a.x" Type.integer);
  assert_equal
    (join
       ~global_resolution
       (create_mutable Type.object_primitive
       |> add_mutable_attribute_refinement !&"a.x" Type.integer)
       (create_mutable Type.object_primitive |> add_mutable_attribute_refinement !&"a.x" Type.string))
    (create_mutable Type.object_primitive
    |> add_mutable_attribute_refinement !&"a.x" Type.(Union [integer; string]));
  (* Nested attributes are compared *)
  assert_equal
    (join
       ~global_resolution
       (create_mutable Type.object_primitive
       |> add_mutable_attribute_refinement !&"a.x" Type.object_primitive
       |> add_mutable_attribute_refinement !&"a.x.b" Type.integer)
       (create_mutable Type.object_primitive |> add_mutable_attribute_refinement !&"a.x" Type.string))
    (create_mutable Type.object_primitive
    |> add_mutable_attribute_refinement !&"a.x" Type.object_primitive);
  ()


let test_meet context =
  let global_resolution = resolution context in
  (* Compare bases *)
  assert_equal
    (meet ~global_resolution (create_mutable Type.integer) (create_mutable Type.integer))
    (create_mutable Type.integer);
  assert_equal
    (meet ~global_resolution (create_mutable Type.integer) (create_mutable Type.float))
    (create_mutable Type.integer);
  (* Compare attributes *)
  assert_equal
    (meet
       ~global_resolution
       (create_mutable Type.object_primitive
       |> add_mutable_attribute_refinement !&"a.x" Type.integer)
       (create_mutable Type.object_primitive
       |> add_mutable_attribute_refinement !&"a.x" Type.integer))
    (create_mutable Type.object_primitive |> add_mutable_attribute_refinement !&"a.x" Type.integer);
  assert_equal
    (meet
       ~global_resolution
       (create_mutable Type.object_primitive
       |> add_mutable_attribute_refinement !&"a.x" Type.integer)
       (create_mutable Type.object_primitive |> add_mutable_attribute_refinement !&"a.x" Type.float))
    (create_mutable Type.object_primitive |> add_mutable_attribute_refinement !&"a.x" Type.integer);
  assert_equal
    (meet
       ~global_resolution
       (create_mutable Type.object_primitive
       |> add_mutable_attribute_refinement !&"a.x" Type.integer)
       (create_mutable Type.object_primitive |> add_mutable_attribute_refinement !&"a.x" Type.string))
    (create_mutable Type.object_primitive |> add_mutable_attribute_refinement !&"a.x" Type.Bottom);
  (* Compare nested attributes *)
  assert_equal
    (meet
       ~global_resolution
       (create_mutable Type.object_primitive
       |> add_mutable_attribute_refinement !&"a.x" Type.object_primitive
       |> add_mutable_attribute_refinement !&"a.x.b" Type.integer)
       (create_mutable Type.object_primitive |> add_mutable_attribute_refinement !&"a.x" Type.string))
    (create_mutable Type.object_primitive
    |> add_mutable_attribute_refinement !&"a.x" Type.string
    |> add_mutable_attribute_refinement !&"a.x.b" Type.integer);
  assert_equal
    (meet
       ~global_resolution
       (create_mutable Type.object_primitive
       |> add_mutable_attribute_refinement !&"a.x" Type.string
       |> add_mutable_attribute_refinement !&"a.x.b" Type.object_primitive)
       (create_mutable Type.object_primitive
       |> add_mutable_attribute_refinement !&"a.x" Type.integer))
    (create_mutable Type.object_primitive |> add_mutable_attribute_refinement !&"a.x" Type.Bottom);
  (* Regression test for short-circuiting logic (which had a bug at one point) *)
  assert_equal
    (meet ~global_resolution (empty |> add_mutable_attribute_refinement !&"a.x" Type.string) empty)
    (empty |> add_mutable_attribute_refinement !&"a.x" Type.string);
  ()


let () =
  "refinementUnit"
  >::: [
         "create" >:: test_create;
         "add_attribute_refinement" >:: test_add_attribute_refinement;
         "less_or_equal" >:: test_less_or_equal;
         "join" >:: test_join;
         "meet" >:: test_meet;
       ]
  |> Test.run
