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
open ReadOnlyness

let test_less_or_equal _ =
  let assert_less_or_equal ~expected ~left ~right =
    assert_bool_equals ~expected (less_or_equal ~left ~right)
  in
  assert_less_or_equal ~left:Mutable ~right:ReadOnly ~expected:true;
  assert_less_or_equal ~left:ReadOnly ~right:Mutable ~expected:false;
  assert_less_or_equal ~left:Mutable ~right:Mutable ~expected:true;
  assert_less_or_equal ~left:ReadOnly ~right:ReadOnly ~expected:true;
  ()


let test_join _ =
  let assert_join ~expected left right =
    let assert_equal = assert_equal ~cmp:[%compare.equal: t] ~printer:show in
    assert_equal expected (join left right);
    assert_equal expected (join right left)
  in
  assert_join Mutable ReadOnly ~expected:ReadOnly;
  assert_join ReadOnly Mutable ~expected:ReadOnly;
  assert_join Mutable Mutable ~expected:Mutable;
  assert_join ReadOnly ReadOnly ~expected:ReadOnly;
  ()


let test_meet _ =
  let assert_meet ~expected left right =
    let assert_equal = assert_equal ~cmp:[%compare.equal: t] ~printer:show in
    assert_equal expected (meet left right);
    assert_equal expected (meet right left)
  in
  assert_meet Mutable ReadOnly ~expected:Mutable;
  assert_meet ReadOnly Mutable ~expected:Mutable;
  assert_meet Mutable Mutable ~expected:Mutable;
  assert_meet ReadOnly ReadOnly ~expected:ReadOnly;
  ()


let test_of_type _ =
  let assert_of_type type_ expected =
    let assert_equal = assert_equal ~cmp:[%compare.equal: t] ~printer:show in
    assert_equal expected (of_type type_)
  in
  assert_of_type Type.integer Mutable;
  assert_of_type (Type.ReadOnly.create Type.integer) ReadOnly;
  ()


let () =
  "readOnly"
  >::: [
         "less_or_equal" >:: test_less_or_equal;
         "join" >:: test_join;
         "meet" >:: test_meet;
         "of_type" >:: test_of_type;
       ]
  |> Test.run
