(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Test

let assert_equal_local_or_global = assert_equal ~printer:TypeInfo.LocalOrGlobal.show

module Unit = struct
  open TypeInfo.Unit

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
         ~left:(TypeInfo.Unit.create_mutable Type.integer)
         ~right:(TypeInfo.Unit.create_mutable Type.integer));
    assert_true
      (less_or_equal
         ~type_less_or_equal
         ~left:(TypeInfo.Unit.create_mutable Type.integer)
         ~right:(TypeInfo.Unit.create_mutable Type.float));
    assert_false
      (less_or_equal
         ~type_less_or_equal
         ~left:(TypeInfo.Unit.create_mutable Type.float)
         ~right:(TypeInfo.Unit.create_mutable Type.integer));
    (* Mutable <= Immutable. *)
    assert_true
      (less_or_equal
         ~type_less_or_equal
         ~left:(TypeInfo.Unit.create_mutable Type.integer)
         ~right:(TypeInfo.Unit.create_immutable Type.integer));
    assert_true
      (less_or_equal
         ~type_less_or_equal
         ~left:(TypeInfo.Unit.create_immutable Type.integer)
         ~right:(TypeInfo.Unit.create_immutable Type.integer));
    assert_false
      (less_or_equal
         ~type_less_or_equal
         ~left:(TypeInfo.Unit.create_immutable Type.integer)
         ~right:(TypeInfo.Unit.create_mutable Type.integer));
    ()


  let test_join context =
    let type_join = GlobalResolution.join (global_resolution context) in
    let assert_equal = assert_equal ~ctxt:context ~cmp:TypeInfo.Unit.equal in
    (* Type order is preserved. *)
    assert_equal
      (join
         ~type_join
         (TypeInfo.Unit.create_mutable Type.integer)
         (TypeInfo.Unit.create_mutable Type.integer))
      (TypeInfo.Unit.create_mutable Type.integer);
    assert_equal
      (join
         ~type_join
         (TypeInfo.Unit.create_mutable Type.integer)
         (TypeInfo.Unit.create_mutable Type.float))
      (TypeInfo.Unit.create_mutable Type.float);
    (* Mutability. *)
    assert_equal
      (join
         ~type_join
         (TypeInfo.Unit.create_mutable Type.integer)
         (TypeInfo.Unit.create_immutable Type.integer))
      (TypeInfo.Unit.create_immutable Type.integer);
    assert_equal
      (join
         ~type_join
         (TypeInfo.Unit.create_immutable Type.integer)
         (TypeInfo.Unit.create_mutable Type.integer))
      (TypeInfo.Unit.create_immutable Type.integer);
    assert_equal
      (join
         ~type_join
         (TypeInfo.Unit.create_immutable Type.integer)
         (TypeInfo.Unit.create_immutable Type.integer))
      (TypeInfo.Unit.create_immutable Type.integer);
    assert_equal
      (join
         ~type_join
         (TypeInfo.Unit.create_immutable Type.float)
         (TypeInfo.Unit.create_immutable Type.integer))
      (TypeInfo.Unit.create_immutable Type.float);
    assert_equal
      (join
         ~type_join
         (TypeInfo.Unit.create_immutable Type.float)
         (TypeInfo.Unit.create_immutable Type.integer))
      (TypeInfo.Unit.create_immutable Type.float);
    assert_equal
      (join
         ~type_join
         (TypeInfo.Unit.create_immutable ~final:true Type.float)
         (TypeInfo.Unit.create_immutable Type.integer))
      (TypeInfo.Unit.create_immutable ~final:true Type.float);
    ()


  let test_meet context =
    let type_meet = GlobalResolution.meet (global_resolution context) in
    let assert_equal = assert_equal ~ctxt:context ~cmp:TypeInfo.Unit.equal in
    assert_equal
      (meet
         ~type_meet
         (TypeInfo.Unit.create_mutable Type.integer)
         (TypeInfo.Unit.create_mutable Type.integer))
      (TypeInfo.Unit.create_mutable Type.integer);
    assert_equal
      (meet
         ~type_meet
         (TypeInfo.Unit.create_mutable Type.integer)
         (TypeInfo.Unit.create_mutable Type.float))
      (TypeInfo.Unit.create_mutable Type.integer);
    (* Mutability. *)
    assert_equal
      (meet
         ~type_meet
         (TypeInfo.Unit.create_mutable Type.integer)
         (TypeInfo.Unit.create_immutable Type.integer))
      (TypeInfo.Unit.create_mutable Type.integer);
    assert_equal
      (meet
         ~type_meet
         (TypeInfo.Unit.create_immutable Type.integer)
         (TypeInfo.Unit.create_mutable Type.integer))
      (TypeInfo.Unit.create_mutable Type.integer);
    assert_equal
      (meet
         ~type_meet
         (TypeInfo.Unit.create_immutable Type.integer)
         (TypeInfo.Unit.create_immutable Type.integer))
      (TypeInfo.Unit.create_immutable Type.integer);
    assert_equal
      (meet
         ~type_meet
         (TypeInfo.Unit.create_immutable Type.float)
         (TypeInfo.Unit.create_immutable Type.integer))
      (TypeInfo.Unit.create_immutable Type.integer);
    assert_equal
      (meet
         ~type_meet
         (TypeInfo.Unit.create_immutable ~final:true Type.float)
         (TypeInfo.Unit.create_immutable Type.integer))
      (TypeInfo.Unit.create_immutable Type.integer);
    assert_equal
      (meet
         ~type_meet
         (TypeInfo.Unit.create_immutable ~final:true Type.float)
         (TypeInfo.Unit.create_immutable ~final:true Type.integer))
      (TypeInfo.Unit.create_immutable ~final:true Type.integer);
    ()
end

module LocalOrGlobal = struct
  open TypeInfo.LocalOrGlobal

  let test_create _ =
    let assert_create ~refinement_unit ~expected =
      assert_equal ~cmp:(Option.equal TypeInfo.Unit.equal) (base refinement_unit) expected
    in
    assert_create ~refinement_unit:empty ~expected:None;
    assert_create
      ~refinement_unit:(create (TypeInfo.Unit.create_mutable Type.Bottom))
      ~expected:(Some (TypeInfo.Unit.create_mutable Type.Bottom));
    assert_create
      ~refinement_unit:(create (TypeInfo.Unit.create_mutable Type.object_primitive))
      ~expected:(Some (TypeInfo.Unit.create_mutable (Type.Primitive "object")));
    ()


  let test_set_annotation _ =
    let assert_attribute_refinement ~refinement_unit ~attribute_path ~expected =
      assert_equal
        ~cmp:(Option.equal TypeInfo.Unit.equal)
        (get_annotation refinement_unit ~attribute_path)
        expected
    in
    let refinement_unit =
      set_annotation
        empty
        ~attribute_path:!&"a.b.c.d"
        ~annotation:(TypeInfo.Unit.create_mutable Type.integer)
    in
    assert_attribute_refinement ~refinement_unit ~attribute_path:!&"a" ~expected:None;
    assert_attribute_refinement ~refinement_unit ~attribute_path:!&"a.b" ~expected:None;
    assert_attribute_refinement ~refinement_unit ~attribute_path:!&"a.b.c" ~expected:None;
    assert_attribute_refinement
      ~refinement_unit
      ~attribute_path:!&"a.b.c.d"
      ~expected:(Some (TypeInfo.Unit.create_mutable (Type.Primitive "int")));
    assert_attribute_refinement ~refinement_unit ~attribute_path:!&"a.b.c.d.e" ~expected:None;

    assert_attribute_refinement
      ~refinement_unit:
        (set_annotation
           refinement_unit
           ~attribute_path:!&"a.b.c.d"
           ~annotation:(TypeInfo.Unit.create_mutable Type.bool))
      ~attribute_path:!&"a.b.c.d"
      ~expected:(Some (TypeInfo.Unit.create_mutable (Type.Primitive "bool")));
    ()


  let resolution context =
    ScratchProject.setup ~context [] |> ScratchProject.build_global_resolution


  let add_mutable_attribute_refinement attribute_path type_ refinement_unit =
    set_annotation refinement_unit ~attribute_path ~annotation:(TypeInfo.Unit.create_mutable type_)


  let test_less_or_equal context =
    let global_resolution = resolution context in
    let type_less_or_equal = GlobalResolution.less_or_equal global_resolution in
    (* Bases are compared *)
    assert_true
      (less_or_equal
         ~type_less_or_equal
         ~left:(create_mutable Type.integer)
         ~right:(create_mutable Type.integer));
    assert_false
      (less_or_equal
         ~type_less_or_equal
         ~left:(create_mutable Type.float)
         ~right:(create_mutable Type.integer));
    (* Attributes are compared *)
    assert_true
      (less_or_equal
         ~type_less_or_equal
         ~left:
           (create_mutable Type.object_primitive
           |> add_mutable_attribute_refinement !&"a.x" Type.integer)
         ~right:
           (create_mutable Type.object_primitive
           |> add_mutable_attribute_refinement !&"a.x" Type.integer));
    assert_true
      (less_or_equal
         ~type_less_or_equal
         ~left:
           (create_mutable Type.object_primitive
           |> add_mutable_attribute_refinement !&"a.x" Type.integer)
         ~right:
           (create_mutable Type.object_primitive
           |> add_mutable_attribute_refinement !&"a.x" Type.float));
    assert_false
      (less_or_equal
         ~type_less_or_equal
         ~left:
           (create_mutable Type.object_primitive
           |> add_mutable_attribute_refinement !&"a.x" Type.float)
         ~right:
           (create_mutable Type.object_primitive
           |> add_mutable_attribute_refinement !&"a.x" Type.integer));
    (* If attributes differ at all, then left must be a subset of right because more data means more
       restrictions, so lower in the lattice *)
    assert_true
      (less_or_equal
         ~type_less_or_equal
         ~left:
           (create_mutable Type.object_primitive
           |> add_mutable_attribute_refinement !&"a.x" Type.object_primitive
           |> add_mutable_attribute_refinement !&"a.x.b" Type.integer)
         ~right:
           (create_mutable Type.object_primitive
           |> add_mutable_attribute_refinement !&"a.x" Type.object_primitive));
    assert_false
      (less_or_equal
         ~type_less_or_equal
         ~left:
           (create_mutable Type.object_primitive
           |> add_mutable_attribute_refinement !&"a.x" Type.object_primitive)
         ~right:
           (create_mutable Type.object_primitive
           |> add_mutable_attribute_refinement !&"a.x" Type.object_primitive
           |> add_mutable_attribute_refinement !&"a.x.b" Type.integer));
    ()


  let test_join context =
    let global_resolution = resolution context in
    let type_join = GlobalResolution.join global_resolution in
    (* Bases are compared *)
    assert_equal_local_or_global
      (join ~type_join (create_mutable Type.integer) (create_mutable Type.integer))
      (create_mutable Type.integer);
    assert_equal_local_or_global
      (join ~type_join (create_mutable Type.integer) (create_mutable Type.float))
      (create_mutable Type.float);
    (* Attributes are compared *)
    assert_equal_local_or_global
      (join
         ~type_join
         (create_mutable Type.object_primitive
         |> add_mutable_attribute_refinement !&"a.x" Type.integer)
         (create_mutable Type.object_primitive
         |> add_mutable_attribute_refinement !&"a.x" Type.integer))
      (create_mutable Type.object_primitive |> add_mutable_attribute_refinement !&"a.x" Type.integer);
    assert_equal_local_or_global
      (join
         ~type_join
         (create_mutable Type.object_primitive
         |> add_mutable_attribute_refinement !&"a.x" Type.integer)
         (create_mutable Type.object_primitive
         |> add_mutable_attribute_refinement !&"a.x" Type.string))
      (create_mutable Type.object_primitive
      |> add_mutable_attribute_refinement !&"a.x" Type.(Union [integer; string]));
    (* Nested attributes are compared *)
    assert_equal_local_or_global
      (join
         ~type_join
         (create_mutable Type.object_primitive
         |> add_mutable_attribute_refinement !&"a.x" Type.object_primitive
         |> add_mutable_attribute_refinement !&"a.x.b" Type.integer)
         (create_mutable Type.object_primitive
         |> add_mutable_attribute_refinement !&"a.x" Type.string))
      (create_mutable Type.object_primitive
      |> add_mutable_attribute_refinement !&"a.x" Type.object_primitive);
    ()


  let test_meet context =
    let global_resolution = resolution context in
    let type_meet = GlobalResolution.meet global_resolution in
    (* Compare bases *)
    assert_equal_local_or_global
      (meet ~type_meet (create_mutable Type.integer) (create_mutable Type.integer))
      (create_mutable Type.integer);
    assert_equal_local_or_global
      (meet ~type_meet (create_mutable Type.integer) (create_mutable Type.float))
      (create_mutable Type.integer);
    (* Compare attributes *)
    assert_equal_local_or_global
      (meet
         ~type_meet
         (create_mutable Type.object_primitive
         |> add_mutable_attribute_refinement !&"a.x" Type.integer)
         (create_mutable Type.object_primitive
         |> add_mutable_attribute_refinement !&"a.x" Type.integer))
      (create_mutable Type.object_primitive |> add_mutable_attribute_refinement !&"a.x" Type.integer);
    assert_equal_local_or_global
      (meet
         ~type_meet
         (create_mutable Type.object_primitive
         |> add_mutable_attribute_refinement !&"a.x" Type.integer)
         (create_mutable Type.object_primitive
         |> add_mutable_attribute_refinement !&"a.x" Type.float))
      (create_mutable Type.object_primitive |> add_mutable_attribute_refinement !&"a.x" Type.integer);
    assert_equal_local_or_global
      (meet
         ~type_meet
         (create_mutable Type.object_primitive
         |> add_mutable_attribute_refinement !&"a.x" Type.integer)
         (create_mutable Type.object_primitive
         |> add_mutable_attribute_refinement !&"a.x" Type.string))
      (create_mutable Type.object_primitive |> add_mutable_attribute_refinement !&"a.x" Type.Bottom);
    (* Compare nested attributes *)
    assert_equal_local_or_global
      (meet
         ~type_meet
         (create_mutable Type.object_primitive
         |> add_mutable_attribute_refinement !&"a.x" Type.object_primitive
         |> add_mutable_attribute_refinement !&"a.x.b" Type.integer)
         (create_mutable Type.object_primitive
         |> add_mutable_attribute_refinement !&"a.x" Type.string))
      (create_mutable Type.object_primitive
      |> add_mutable_attribute_refinement !&"a.x" Type.string
      |> add_mutable_attribute_refinement !&"a.x.b" Type.integer);
    assert_equal_local_or_global
      (meet
         ~type_meet
         (create_mutable Type.object_primitive
         |> add_mutable_attribute_refinement !&"a.x" Type.string
         |> add_mutable_attribute_refinement !&"a.x.b" Type.object_primitive)
         (create_mutable Type.object_primitive
         |> add_mutable_attribute_refinement !&"a.x" Type.integer))
      (create_mutable Type.object_primitive
      |> add_mutable_attribute_refinement !&"a.x" Type.Bottom
      |> add_mutable_attribute_refinement !&"a.x.b" Type.object_primitive);
    (* Check behavior when we meet and there are only attribute refinements, no base at all *)
    assert_equal_local_or_global
      (meet ~type_meet (empty |> add_mutable_attribute_refinement !&"a.x" Type.string) empty)
      (empty |> add_mutable_attribute_refinement !&"a.x" Type.string);
    ()
end

let () =
  "TypeInfo"
  >::: [
         "Unit.dequalify" >:: Unit.test_dequalify;
         "Unit.less_or_equal" >:: Unit.test_less_or_equal;
         "Unit.join" >:: Unit.test_join;
         "Unit.meet" >:: Unit.test_meet;
         "LocalOrGlobal.create" >:: LocalOrGlobal.test_create;
         "LocalOrGlobal.set_annotation" >:: LocalOrGlobal.test_set_annotation;
         "LocalOrGlobal.less_or_equal" >:: LocalOrGlobal.test_less_or_equal;
         "LocalOrGlobal.join" >:: LocalOrGlobal.test_join;
         "LocalOrGlobal.meet" >:: LocalOrGlobal.test_meet;
       ]
  |> Test.run
