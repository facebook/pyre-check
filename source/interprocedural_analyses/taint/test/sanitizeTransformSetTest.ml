(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Taint

let test_set _ =
  let assert_equal_sets ~expected actual =
    assert_equal ~cmp:SanitizeTransformSet.equal ~printer:SanitizeTransformSet.show actual expected
  in
  let source_a = SanitizeTransform.Source.Named "A" in
  let source_b = SanitizeTransform.Source.Named "B" in
  let source_c = SanitizeTransform.Source.Named "C" in
  let sink_d = SanitizeTransform.Sink.Named "D" in
  let sink_e = SanitizeTransform.Sink.Named "E" in
  let source_a_set = SanitizeTransform.SourceSet.singleton source_a in
  let source_b_set = SanitizeTransform.SourceSet.singleton source_b in
  let source_ab_set = SanitizeTransform.SourceSet.of_list [source_a; source_b] in
  let sink_d_set = SanitizeTransform.SinkSet.singleton sink_d in
  let sink_de_set = SanitizeTransform.SinkSet.of_list [sink_d; sink_e] in
  let source_ab_sink_d_set = { SanitizeTransformSet.sources = source_ab_set; sinks = sink_d_set } in
  let source_a_sink_de_set = { SanitizeTransformSet.sources = source_a_set; sinks = sink_de_set } in
  let all = SanitizeTransformSet.all in
  let empty = SanitizeTransformSet.bottom in

  assert_equal_sets
    ~expected:
      { SanitizeTransformSet.sources = source_b_set; sinks = SanitizeTransform.SinkSet.empty }
    (SanitizeTransformSet.diff source_ab_sink_d_set source_a_sink_de_set);
  assert_equal_sets
    ~expected:SanitizeTransformSet.all
    (SanitizeTransformSet.diff all source_ab_sink_d_set);
  assert_equal_sets
    ~expected:SanitizeTransformSet.empty
    (SanitizeTransformSet.diff source_ab_sink_d_set all);
  assert_equal_sets
    ~expected:source_ab_sink_d_set
    (SanitizeTransformSet.diff source_ab_sink_d_set empty);

  assert_equal_sets ~expected:all (SanitizeTransformSet.join source_ab_sink_d_set all);
  assert_equal_sets
    ~expected:{ SanitizeTransformSet.sources = source_ab_set; sinks = sink_de_set }
    (SanitizeTransformSet.join source_ab_sink_d_set source_a_sink_de_set);
  assert_equal_sets
    ~expected:source_ab_sink_d_set
    (SanitizeTransformSet.join
       { SanitizeTransformSet.sources = source_a_set; sinks = sink_d_set }
       source_ab_sink_d_set);

  assert_equal (SanitizeTransformSet.less_or_equal ~left:source_ab_sink_d_set ~right:all) true;
  assert_equal (SanitizeTransformSet.less_or_equal ~left:source_ab_sink_d_set ~right:empty) false;
  assert_equal (SanitizeTransformSet.less_or_equal ~left:all ~right:source_ab_sink_d_set) false;
  assert_equal
    (SanitizeTransformSet.less_or_equal
       ~left:{ SanitizeTransformSet.sources = source_a_set; sinks = sink_d_set }
       ~right:source_ab_sink_d_set)
    true;
  assert_equal
    (SanitizeTransformSet.less_or_equal
       ~left:source_ab_sink_d_set
       ~right:{ SanitizeTransformSet.sources = source_a_set; sinks = sink_d_set })
    false;

  assert_equal
    (SanitizeTransformSet.mem source_ab_sink_d_set (SanitizeTransform.Source source_a))
    true;
  assert_equal (SanitizeTransformSet.mem source_ab_sink_d_set (SanitizeTransform.Sink sink_e)) false;
  assert_equal (SanitizeTransformSet.mem all (SanitizeTransform.Source source_c)) true;
  assert_equal (SanitizeTransformSet.mem empty (SanitizeTransform.Source source_a)) false


let () = "sanitize_transform_set" >::: ["set" >:: test_set] |> Test.run
