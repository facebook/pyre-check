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
  assert_create ~refinement_unit:(create ()) ~expected:None;
  assert_create
    ~refinement_unit:(create ~base:(Annotation.create_mutable Type.Bottom) ())
    ~expected:(Some (Annotation.create_mutable Type.Bottom));
  assert_create
    ~refinement_unit:(create ~base:(Annotation.create_mutable Type.object_primitive) ())
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
      (create ())
      ~reference:!&"a.b.c.d"
      ~base:(Annotation.create_mutable Type.integer)
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
         ~base:(Annotation.create_mutable Type.bool))
    ~reference:!&"a.b.c.d"
    ~expected:(Some (Annotation.create_mutable (Type.Primitive "bool")));
  ()


let resolution context = ScratchProject.setup ~context [] |> ScratchProject.build_global_resolution

let test_less_or_equal context =
  let global_resolution = resolution context in
  (* Type order is preserved. *)
  assert_true
    (less_or_equal
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.integer) ())
       (create ~base:(Annotation.create_mutable Type.integer) ()));
  assert_true
    (less_or_equal
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.integer) ())
       (create ~base:(Annotation.create_mutable Type.float) ()));
  assert_false
    (less_or_equal
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.float) ())
       (create ~base:(Annotation.create_mutable Type.integer) ()));
  assert_true
    (less_or_equal
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.integer)
       )
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.integer)
       ));
  assert_true
    (less_or_equal
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.integer)
       )
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.float)));
  assert_false
    (less_or_equal
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.float))
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.integer)
       ));
  assert_false
    (less_or_equal
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement
            ~reference:!&"a.x"
            ~base:(Annotation.create_mutable Type.object_primitive))
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement
            ~reference:!&"a.x"
            ~base:(Annotation.create_mutable Type.object_primitive)
       |> add_attribute_refinement
            ~reference:!&"a.x.b"
            ~base:(Annotation.create_mutable Type.integer)));

  (* Mutable <= Immutable. *)
  assert_true
    (less_or_equal
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.integer) ())
       (create ~base:(Annotation.create_immutable Type.integer) ()));
  assert_true
    (less_or_equal
       ~global_resolution
       (create ~base:(Annotation.create_immutable Type.integer) ())
       (create ~base:(Annotation.create_immutable Type.integer) ()));
  assert_false
    (less_or_equal
       ~global_resolution
       (create ~base:(Annotation.create_immutable Type.integer) ())
       (create ~base:(Annotation.create_mutable Type.integer) ()));
  ()


let test_join context =
  let global_resolution = resolution context in
  (* Type order is preserved. *)
  assert_equal
    (join
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.integer) ())
       (create ~base:(Annotation.create_mutable Type.integer) ()))
    (create ~base:(Annotation.create_mutable Type.integer) ());
  assert_equal
    (join
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.integer) ())
       (create ~base:(Annotation.create_mutable Type.float) ()))
    (create ~base:(Annotation.create_mutable Type.float) ());
  assert_equal
    (join
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.integer)
       )
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.integer)
       ))
    (create ~base:(Annotation.create_mutable Type.object_primitive) ()
    |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.integer));
  assert_equal
    (join
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.integer)
       )
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.string)
       ))
    (create ~base:(Annotation.create_mutable Type.object_primitive) ()
    |> add_attribute_refinement
         ~reference:!&"a.x"
         ~base:(Annotation.create_mutable Type.(Union [integer; string])));
  assert_equal
    (join
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement
            ~reference:!&"a.x"
            ~base:(Annotation.create_mutable Type.object_primitive)
       |> add_attribute_refinement
            ~reference:!&"a.x.b"
            ~base:(Annotation.create_mutable Type.integer))
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.string)
       ))
    (create ~base:(Annotation.create_mutable Type.object_primitive) ()
    |> add_attribute_refinement
         ~reference:!&"a.x"
         ~base:(Annotation.create_mutable Type.object_primitive));

  (* Mutability. *)
  assert_equal
    (join
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.integer) ())
       (create ~base:(Annotation.create_immutable Type.integer) ()))
    (create ~base:(Annotation.create_immutable Type.integer) ());
  assert_equal
    (join
       ~global_resolution
       (create ~base:(Annotation.create_immutable Type.integer) ())
       (create ~base:(Annotation.create_mutable Type.integer) ()))
    (create ~base:(Annotation.create_immutable Type.integer) ());
  assert_equal
    (join
       ~global_resolution
       (create ~base:(Annotation.create_immutable Type.integer) ())
       (create ~base:(Annotation.create_immutable Type.integer) ()))
    (create ~base:(Annotation.create_immutable Type.integer) ());
  assert_equal
    (join
       ~global_resolution
       (create ~base:(Annotation.create_immutable Type.float) ())
       (create ~base:(Annotation.create_immutable Type.integer) ()))
    (create ~base:(Annotation.create_immutable Type.float) ());
  assert_equal
    (join
       ~global_resolution
       (create ~base:(Annotation.create_immutable Type.float) ())
       (create ~base:(Annotation.create_immutable Type.integer) ()))
    (create ~base:(Annotation.create_immutable Type.float) ());
  assert_equal
    (join
       ~global_resolution
       (create ~base:(Annotation.create_immutable ~final:true Type.float) ())
       (create ~base:(Annotation.create_immutable Type.integer) ()))
    (create ~base:(Annotation.create_immutable ~final:true Type.float) ());
  ()


let test_meet context =
  let global_resolution = resolution context in
  let assert_equal actual expected =
    assert_equal ~printer:[%show: RefinementUnit.t] expected actual
  in
  (* Type order is preserved. *)
  assert_equal
    (meet
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.integer) ())
       (create ~base:(Annotation.create_mutable Type.integer) ()))
    (create ~base:(Annotation.create_mutable Type.integer) ());
  assert_equal
    (meet
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.integer) ())
       (create ~base:(Annotation.create_mutable Type.float) ()))
    (create ~base:(Annotation.create_mutable Type.integer) ());
  assert_equal
    (meet
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.integer)
       )
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.integer)
       ))
    (create ~base:(Annotation.create_mutable Type.object_primitive) ()
    |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.integer));
  assert_equal
    (meet
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.integer)
       )
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.float)))
    (create ~base:(Annotation.create_mutable Type.object_primitive) ()
    |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.integer));
  assert_equal
    (meet
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.integer)
       )
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.string)
       ))
    (create ~base:(Annotation.create_mutable Type.object_primitive) ()
    |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.Bottom));
  assert_equal
    (meet
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement
            ~reference:!&"a.x"
            ~base:(Annotation.create_mutable Type.object_primitive)
       |> add_attribute_refinement
            ~reference:!&"a.x.b"
            ~base:(Annotation.create_mutable Type.integer))
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.string)
       ))
    (create ~base:(Annotation.create_mutable Type.object_primitive) ()
    |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.string)
    |> add_attribute_refinement ~reference:!&"a.x.b" ~base:(Annotation.create_mutable Type.integer)
    );
  assert_equal
    (meet
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.string)
       |> add_attribute_refinement
            ~reference:!&"a.x.b"
            ~base:(Annotation.create_mutable Type.object_primitive))
       (create ~base:(Annotation.create_mutable Type.object_primitive) ()
       |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.integer)
       ))
    (create ~base:(Annotation.create_mutable Type.object_primitive) ()
    |> add_attribute_refinement ~reference:!&"a.x" ~base:(Annotation.create_mutable Type.Bottom));

  (* Mutability. *)
  assert_equal
    (meet
       ~global_resolution
       (create ~base:(Annotation.create_mutable Type.integer) ())
       (create ~base:(Annotation.create_immutable Type.integer) ()))
    (create ~base:(Annotation.create_mutable Type.integer) ());
  assert_equal
    (meet
       ~global_resolution
       (create ~base:(Annotation.create_immutable Type.integer) ())
       (create ~base:(Annotation.create_mutable Type.integer) ()))
    (create ~base:(Annotation.create_mutable Type.integer) ());
  assert_equal
    (meet
       ~global_resolution
       (create ~base:(Annotation.create_immutable Type.integer) ())
       (create ~base:(Annotation.create_immutable Type.integer) ()))
    (create ~base:(Annotation.create_immutable Type.integer) ());
  assert_equal
    (meet
       ~global_resolution
       (create ~base:(Annotation.create_immutable Type.float) ())
       (create ~base:(Annotation.create_immutable Type.integer) ()))
    (create ~base:(Annotation.create_immutable Type.integer) ());
  assert_equal
    (meet
       ~global_resolution
       (create ~base:(Annotation.create_immutable ~final:true Type.float) ())
       (create ~base:(Annotation.create_immutable Type.integer) ()))
    (create ~base:(Annotation.create_immutable Type.integer) ());
  assert_equal
    (meet
       ~global_resolution
       (create ~base:(Annotation.create_immutable ~final:true Type.float) ())
       (create ~base:(Annotation.create_immutable ~final:true Type.integer) ()))
    (create ~base:(Annotation.create_immutable ~final:true Type.integer) ());
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
