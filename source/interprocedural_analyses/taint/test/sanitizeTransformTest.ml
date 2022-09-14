(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Taint

let test_source_set _ =
  let open SanitizeTransform in
  let assert_equal_sets ~expected actual =
    assert_equal ~cmp:SourceSet.equal ~printer:SourceSet.show actual expected
  in
  let source_a = Source.Named "A" in
  let source_b = Source.Named "B" in
  let source_c = Source.Named "C" in
  let source_a_set = SourceSet.singleton source_a in
  let source_b_set = SourceSet.singleton source_b in
  let source_ab_set = SourceSet.of_list [source_a; source_b] in
  let all = SourceSet.all in
  let empty = SourceSet.bottom in

  assert_equal_sets ~expected:source_a_set (SourceSet.diff source_a_set source_b_set);
  assert_equal_sets ~expected:SourceSet.all (SourceSet.diff all source_a_set);
  assert_equal_sets ~expected:SourceSet.empty (SourceSet.diff source_a_set all);
  assert_equal_sets ~expected:source_a_set (SourceSet.diff source_a_set empty);

  assert_equal_sets ~expected:all (SourceSet.join source_a_set all);
  assert_equal_sets ~expected:source_ab_set (SourceSet.join source_a_set source_b_set);
  assert_equal_sets ~expected:source_ab_set (SourceSet.join source_a_set source_ab_set);

  assert_equal (SourceSet.less_or_equal ~left:source_a_set ~right:all) true;
  assert_equal (SourceSet.less_or_equal ~left:source_a_set ~right:empty) false;
  assert_equal (SourceSet.less_or_equal ~left:all ~right:source_a_set) false;
  assert_equal (SourceSet.less_or_equal ~left:source_a_set ~right:source_ab_set) true;
  assert_equal (SourceSet.less_or_equal ~left:source_ab_set ~right:source_a_set) false;

  assert_equal (SourceSet.mem source_a source_ab_set) true;
  assert_equal (SourceSet.mem source_c source_ab_set) false;
  assert_equal (SourceSet.mem source_c all) true;
  assert_equal (SourceSet.mem source_c empty) false


let () = "sanitize_transform" >::: ["source_set" >:: test_source_set] |> Test.run
