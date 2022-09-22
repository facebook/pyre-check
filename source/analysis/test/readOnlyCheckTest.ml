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
open ReadOnlyCheck
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


let test_forward_expression _ =
  let assert_resolved ?(resolution = Resolution.of_list []) expression expected_type =
    let { Resolved.resolved; _ } =
      parse_single_expression expression |> State.forward_expression ~resolution
    in
    assert_equal ~cmp:[%compare.equal: t] ~printer:show expected_type resolved
  in
  assert_resolved "..." ReadOnly;
  assert_resolved "False" ReadOnly;
  assert_resolved "True" ReadOnly;
  assert_resolved "1.2" ReadOnly;
  assert_resolved "42" ReadOnly;
  assert_resolved "'hello'" ReadOnly;
  assert_resolved "b'hello'" ReadOnly;
  assert_resolved "None" ReadOnly;
  assert_resolved ~resolution:(Resolution.of_list [!&"x", ReadOnly]) "x" ReadOnly;
  assert_resolved ~resolution:(Resolution.of_list [!&"x", Mutable]) "x" Mutable;
  assert_resolved ~resolution:(Resolution.of_list []) "x" Mutable;
  ()


let () =
  "readOnly"
  >::: [
         "less_or_equal" >:: test_less_or_equal;
         "join" >:: test_join;
         "meet" >:: test_meet;
         "forward_expression" >:: test_forward_expression;
       ]
  |> Test.run
