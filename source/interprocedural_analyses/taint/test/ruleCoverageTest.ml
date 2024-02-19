(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Taint

let test_covered_rule _ =
  let open RuleCoverage in
  let assert_covered_rule ~expected ~actual =
    assert_equal
      ~cmp:(Option.equal CoveredRule.equal)
      ~printer:(Option.value_map ~default:"None" ~f:CoveredRule.show)
      expected
      actual
  in
  let kind_coverage =
    {
      KindCoverage.sources = KindCoverage.Sources.Set.of_list [Sources.NamedSource "SourceA"];
      sinks = KindCoverage.Sinks.Set.of_list [Sinks.NamedSink "SinkA"];
      transforms = KindCoverage.Transforms.Set.of_list [TaintTransform.Named "TransformX"];
    }
  in

  (* Not covered because the source is not used. *)
  let source_not_covered =
    {
      Rule.sources = [Sources.NamedSource "SourceB"];
      sinks = [Sinks.NamedSink "SinkA"];
      transforms = [TaintTransform.Named "TransformX"];
      code = 1234;
      name = "Test Rule";
      message_format = "";
      location = None;
    }
  in
  assert_covered_rule
    ~expected:None
    ~actual:(CoveredRule.is_covered ~kind_coverage source_not_covered);

  (* Not covered because the sink is not used. *)
  let sink_not_covered =
    {
      Rule.sources = [Sources.NamedSource "SourceA"];
      sinks = [Sinks.NamedSink "SinkB"];
      transforms = [TaintTransform.Named "TransformX"];
      code = 1234;
      name = "Test Rule";
      message_format = "";
      location = None;
    }
  in
  assert_covered_rule
    ~expected:None
    ~actual:(CoveredRule.is_covered ~kind_coverage sink_not_covered);

  (* Not covered because the transform is not used. *)
  let transform_not_covered =
    {
      Rule.sources = [Sources.NamedSource "SourceA"];
      sinks = [Sinks.NamedSink "SinkA"];
      transforms = [TaintTransform.Named "TransformY"];
      code = 1234;
      name = "Test Rule";
      message_format = "";
      location = None;
    }
  in
  assert_covered_rule
    ~expected:None
    ~actual:(CoveredRule.is_covered ~kind_coverage transform_not_covered);

  let covered_rule =
    {
      Rule.sources = [Sources.NamedSource "SourceA"];
      sinks = [Sinks.NamedSink "SinkA"; Sinks.NamedSink "SinkB"];
      transforms = [TaintTransform.Named "TransformX"];
      code = 1234;
      name = "Test Rule";
      message_format = "";
      location = None;
    }
  in
  assert_covered_rule
    ~expected:(Some { CoveredRule.rule_code = 1234; kind_coverage })
    ~actual:(CoveredRule.is_covered ~kind_coverage covered_rule);

  (* Don't have taint transforms, but is still considered as covered. *)
  let another_covered_rule =
    {
      Rule.sources = [Sources.NamedSource "SourceA"];
      sinks = [Sinks.NamedSink "SinkA"; Sinks.NamedSink "SinkB"];
      transforms = [];
      code = 1234;
      name = "Test Rule";
      message_format = "";
      location = None;
    }
  in
  assert_covered_rule
    ~expected:
      (Some
         {
           CoveredRule.rule_code = 1234;
           kind_coverage =
             {
               KindCoverage.sources =
                 KindCoverage.Sources.Set.of_list [Sources.NamedSource "SourceA"];
               sinks = KindCoverage.Sinks.Set.of_list [Sinks.NamedSink "SinkA"];
               transforms = KindCoverage.Transforms.Set.empty;
             };
         })
    ~actual:(CoveredRule.is_covered ~kind_coverage another_covered_rule)


let () = "rule_coverage" >::: ["covered_rule" >:: test_covered_rule] |> Test.run
