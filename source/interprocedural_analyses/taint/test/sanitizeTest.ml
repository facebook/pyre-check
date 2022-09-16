(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Taint
open Core

let test_sanitize _ =
  let assert_sanitize_equal ~expected actual =
    assert_equal ~cmp:Taint.Sanitize.equal ~printer:Taint.Sanitize.show actual expected
  in
  let source_a = SanitizeTransform.Source.Named "a" in
  let source_a_set = SanitizeTransform.SourceSet.singleton source_a in
  let source_b = SanitizeTransform.Source.Named "b" in
  let source_b_set = SanitizeTransform.SourceSet.singleton source_b in
  let source_ab_set = SanitizeTransform.SourceSet.of_list [source_a; source_b] in
  let sink_a = SanitizeTransform.Sink.Named "a" in
  let sink_a_set = SanitizeTransform.SinkSet.singleton sink_a in
  let sink_b = SanitizeTransform.Sink.Named "b" in
  let sink_b_set = SanitizeTransform.SinkSet.singleton sink_b in
  let sink_ab_set = SanitizeTransform.SinkSet.of_list [sink_a; sink_b] in

  (* Test join *)
  assert_sanitize_equal (Sanitize.join Sanitize.bottom Sanitize.all) ~expected:Sanitize.all;
  assert_sanitize_equal (Sanitize.join Sanitize.all Sanitize.bottom) ~expected:Sanitize.all;
  assert_sanitize_equal
    (Sanitize.join
       {
         Sanitize.sources = source_a_set;
         sinks = SanitizeTransform.SinkSet.all;
         tito = { SanitizeTransformSet.sources = source_b_set; sinks = sink_a_set };
       }
       {
         Sanitize.sources = SanitizeTransform.SourceSet.all;
         sinks = SanitizeTransform.SinkSet.all;
         tito = SanitizeTransformSet.all;
       })
    ~expected:
      {
        Sanitize.sources = SanitizeTransform.SourceSet.all;
        sinks = SanitizeTransform.SinkSet.all;
        tito = SanitizeTransformSet.all;
      };
  assert_sanitize_equal
    (Sanitize.join
       {
         Sanitize.sources = source_a_set;
         sinks = sink_b_set;
         tito = { SanitizeTransformSet.sources = source_b_set; sinks = sink_a_set };
       }
       {
         Sanitize.sources = source_b_set;
         sinks = sink_a_set;
         tito = { SanitizeTransformSet.sources = source_a_set; sinks = sink_b_set };
       })
    ~expected:
      {
        Sanitize.sources = source_ab_set;
        sinks = sink_ab_set;
        tito = { SanitizeTransformSet.sources = source_ab_set; sinks = sink_ab_set };
      };

  (* Tess less_or_equal *)
  let assert_less_or_equal ~left ~right =
    assert_equal
      ~cmp:(fun left right -> Sanitize.less_or_equal ~left ~right)
      ~printer:Sanitize.show
      ~msg:"left is not less or equal than right"
      left
      right
  in
  let assert_not_less_or_equal ~left ~right =
    assert_equal
      ~cmp:(fun left right -> not (Sanitize.less_or_equal ~left ~right))
      ~printer:Sanitize.show
      ~msg:"left is less or equal than right"
      left
      right
  in
  assert_less_or_equal ~left:Sanitize.bottom ~right:Sanitize.all;
  assert_not_less_or_equal ~left:Sanitize.all ~right:Sanitize.bottom;
  assert_less_or_equal ~left:Sanitize.all ~right:Sanitize.all;
  assert_less_or_equal
    ~left:(Sanitize.from_sinks_only SanitizeTransform.SinkSet.all)
    ~right:Sanitize.all;
  assert_not_less_or_equal
    ~left:(Sanitize.from_sinks_only SanitizeTransform.SinkSet.all)
    ~right:
      {
        Sanitize.sources = SanitizeTransform.SourceSet.all;
        sinks = SanitizeTransform.SinkSet.empty;
        tito = SanitizeTransformSet.all;
      };
  assert_less_or_equal
    ~left:
      {
        Sanitize.sources = source_a_set;
        sinks = sink_a_set;
        tito = { SanitizeTransformSet.sources = source_a_set; sinks = sink_b_set };
      }
    ~right:Sanitize.all;
  assert_less_or_equal
    ~left:
      {
        Sanitize.sources = source_a_set;
        sinks = sink_b_set;
        tito = { SanitizeTransformSet.sources = source_a_set; sinks = sink_b_set };
      }
    ~right:
      {
        Sanitize.sources = source_ab_set;
        sinks = sink_ab_set;
        tito = { SanitizeTransformSet.sources = source_ab_set; sinks = sink_ab_set };
      };
  assert_not_less_or_equal
    ~left:
      {
        Sanitize.sources = source_a_set;
        sinks = sink_b_set;
        tito = { SanitizeTransformSet.sources = source_a_set; sinks = sink_b_set };
      }
    ~right:
      {
        Sanitize.sources = source_ab_set;
        sinks = sink_ab_set;
        tito = { SanitizeTransformSet.sources = source_b_set; sinks = sink_ab_set };
      }


let () = "sanitize" >::: ["sanitize_domain" >:: test_sanitize] |> Test.run
