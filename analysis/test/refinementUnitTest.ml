(* Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Analysis
open Test

let test_create _ =
  let assert_create ~refinement_unit ~expected =
    assert_equal ~cmp:(Option.equal Annotation.equal) (RefinementUnit.base refinement_unit) expected
  in
  assert_create ~refinement_unit:(RefinementUnit.create ()) ~expected:None;
  assert_create
    ~refinement_unit:(RefinementUnit.create ~base:(Annotation.create Type.Bottom) ())
    ~expected:(Some { mutability = Mutable; annotation = Type.Bottom });
  assert_create
    ~refinement_unit:(RefinementUnit.create ~base:(Annotation.create Type.object_primitive) ())
    ~expected:(Some (Annotation.create (Type.Primitive "object")));
  ()


let test_add_attribute_refinement _ =
  let assert_attribute_refinement ~refinement_unit ~reference ~expected =
    assert_equal
      ~cmp:(Option.equal Annotation.equal)
      (RefinementUnit.annotation refinement_unit ~reference)
      expected
  in
  let refinement_unit =
    RefinementUnit.(
      add_attribute_refinement
        (create ())
        ~reference:!&"a.b.c.d"
        ~base:(Annotation.create Type.integer))
  in
  assert_attribute_refinement ~refinement_unit ~reference:!&"a" ~expected:None;
  assert_attribute_refinement ~refinement_unit ~reference:!&"a.b" ~expected:None;
  assert_attribute_refinement ~refinement_unit ~reference:!&"a.b.c" ~expected:None;
  assert_attribute_refinement
    ~refinement_unit
    ~reference:!&"a.b.c.d"
    ~expected:(Some { mutability = Mutable; annotation = Type.Primitive "int" });
  assert_attribute_refinement ~refinement_unit ~reference:!&"a.b.c.d.e" ~expected:None;

  assert_attribute_refinement
    ~refinement_unit:
      (RefinementUnit.add_attribute_refinement
         refinement_unit
         ~reference:!&"a.b.c.d"
         ~base:(Annotation.create Type.bool))
    ~reference:!&"a.b.c.d"
    ~expected:(Some { mutability = Mutable; annotation = Type.Primitive "bool" });
  ()


let () =
  "refinementUnit"
  >::: ["create" >:: test_create; "add_attribute_refinement" >:: test_add_attribute_refinement]
  |> Test.run
