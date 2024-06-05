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
  let kind_coverage_from_models =
    {
      KindCoverage.sources = KindCoverage.Sources.Set.of_list [Sources.NamedSource "SourceA"];
      sinks = KindCoverage.Sinks.Set.of_list [Sinks.NamedSink "SinkA"];
      transforms = KindCoverage.Transforms.Set.of_list [TaintTransform.Named "TransformX"];
    }
  in
  let assert_covered_rule
      ?(kind_coverage_from_models = kind_coverage_from_models)
      ?(partial_sink_converter = TaintConfiguration.PartialSinkConverter.empty)
      ~expected
      ~actual
      ()
    =
    let actual = CoveredRule.is_covered ~partial_sink_converter ~kind_coverage_from_models actual in
    assert_equal
      ~cmp:(Option.equal CoveredRule.equal)
      ~printer:(Option.value_map ~default:"None" ~f:CoveredRule.show)
      expected
      actual
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
      filters = None;
      location = None;
    }
  in
  assert_covered_rule ~expected:None ~actual:source_not_covered ();

  (* Not covered because the sink is not used. *)
  let sink_not_covered =
    {
      Rule.sources = [Sources.NamedSource "SourceA"];
      sinks = [Sinks.NamedSink "SinkB"];
      transforms = [TaintTransform.Named "TransformX"];
      code = 1234;
      name = "Test Rule";
      message_format = "";
      filters = None;
      location = None;
    }
  in
  assert_covered_rule ~expected:None ~actual:sink_not_covered ();

  (* Not covered because the transform is not used. *)
  let transform_not_covered =
    {
      Rule.sources = [Sources.NamedSource "SourceA"];
      sinks = [Sinks.NamedSink "SinkA"];
      transforms = [TaintTransform.Named "TransformY"];
      code = 1234;
      name = "Test Rule";
      message_format = "";
      filters = None;
      location = None;
    }
  in
  assert_covered_rule ~expected:None ~actual:transform_not_covered ();

  let covered_rule =
    {
      Rule.sources = [Sources.NamedSource "SourceA"];
      sinks = [Sinks.NamedSink "SinkA"; Sinks.NamedSink "SinkB"];
      transforms = [TaintTransform.Named "TransformX"];
      code = 1234;
      name = "Test Rule";
      message_format = "";
      filters = None;
      location = None;
    }
  in
  assert_covered_rule
    ~expected:(Some { CoveredRule.rule_code = 1234; kind_coverage = kind_coverage_from_models })
    ~actual:covered_rule
    ();

  (* Don't have taint transforms, but is still considered as covered. *)
  let another_covered_rule =
    {
      Rule.sources = [Sources.NamedSource "SourceA"];
      sinks = [Sinks.NamedSink "SinkA"; Sinks.NamedSink "SinkB"];
      transforms = [];
      code = 1234;
      name = "Test Rule";
      message_format = "";
      filters = None;
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
    ~actual:another_covered_rule
    ()


let test_rule_coverage _ =
  let kind_coverage_from_models =
    {
      KindCoverage.sources =
        KindCoverage.Sources.Set.of_list
          [
            Sources.NamedSource "SourceA";
            Sources.NamedSource "SourceC";
            Sources.NamedSource "SourceD";
          ];
      sinks =
        KindCoverage.Sinks.Set.of_list
          [
            Sinks.NamedSink "SinkA";
            Sinks.TriggeredPartialSink
              { partial_sink = "SinkC[label_1]"; triggering_source = "SourceD" };
            Sinks.TriggeredPartialSink
              { partial_sink = "SinkC[label_2]"; triggering_source = "SourceC" };
          ];
      transforms = KindCoverage.Transforms.Set.of_list [TaintTransform.Named "TransformX"];
    }
  in
  let covered_rule_1 =
    {
      Rule.sources = [Sources.NamedSource "SourceA"];
      sinks = [Sinks.NamedSink "SinkA"; Sinks.NamedSink "SinkB"];
      transforms = [];
      code = 1000;
      name = "Rule 1";
      message_format = "";
      filters = None;
      location = None;
    }
  in
  let covered_rule_2 =
    {
      Rule.sources = [Sources.NamedSource "SourceA"];
      sinks = [Sinks.NamedSink "SinkA"];
      transforms = [TaintTransform.Named "TransformX"];
      code = 1001;
      name = "Rule 2";
      message_format = "";
      filters = None;
      location = None;
    }
  in
  (* Not covered because the source is not used. *)
  let uncovered_rule_1 =
    {
      Rule.sources = [Sources.NamedSource "SourceB"];
      sinks = [Sinks.NamedSink "SinkA"];
      transforms = [];
      code = 1002;
      name = "Rule 3";
      message_format = "";
      filters = None;
      location = None;
    }
  in
  (* Not covered because the transform is not used. *)
  let uncovered_rule_2 =
    {
      Rule.sources = [Sources.NamedSource "SourceA"];
      sinks = [Sinks.NamedSink "SinkA"];
      transforms = [TaintTransform.Named "TransformY"];
      code = 1004;
      name = "Rule 5";
      message_format = "";
      filters = None;
      location = None;
    }
  in
  let partial_sink_converter =
    TaintConfiguration.PartialSinkConverter.add
      ~first_sources:[Sources.NamedSource "SourceC"]
      ~first_sink:"SinkC[label_1]"
      ~second_sources:[Sources.NamedSource "SourceD"]
      ~second_sink:"SinkC[label_2]"
      TaintConfiguration.PartialSinkConverter.empty
  in
  let multi_source_rule_part_1 =
    {
      Rule.sources = [Sources.NamedSource "SourceC"];
      sinks =
        [
          Sinks.TriggeredPartialSink
            { partial_sink = "SinkC[label_1]"; triggering_source = "SourceD" };
        ];
      transforms = [];
      code = 1003;
      name = "Rule 4";
      message_format = "";
      filters = None;
      location = None;
    }
  in
  let multi_source_rule_part_2 =
    {
      Rule.sources = [Sources.NamedSource "SourceD"];
      sinks =
        [
          Sinks.TriggeredPartialSink
            { partial_sink = "SinkC[label_2]"; triggering_source = "SourceC" };
        ];
      transforms = [];
      code = 1003;
      name = "Rule 4";
      message_format = "";
      filters = None;
      location = None;
    }
  in
  let actual_category_coverage =
    RuleCoverage.from_rules
      ~partial_sink_converter
      ~kind_coverage_from_models
      [
        covered_rule_1;
        covered_rule_2;
        uncovered_rule_1;
        uncovered_rule_2;
        multi_source_rule_part_1;
        multi_source_rule_part_2;
      ]
  in
  let expected_category_coverage =
    {
      RuleCoverage.covered_rules =
        RuleCoverage.CoveredRule.Set.of_list
          [
            {
              RuleCoverage.CoveredRule.rule_code = 1000;
              kind_coverage =
                {
                  KindCoverage.sources =
                    KindCoverage.Sources.Set.of_list [Sources.NamedSource "SourceA"];
                  sinks = KindCoverage.Sinks.Set.of_list [Sinks.NamedSink "SinkA"];
                  transforms = KindCoverage.Transforms.Set.empty;
                };
            };
            {
              RuleCoverage.CoveredRule.rule_code = 1001;
              kind_coverage =
                {
                  KindCoverage.sources =
                    KindCoverage.Sources.Set.of_list [Sources.NamedSource "SourceA"];
                  sinks = KindCoverage.Sinks.Set.of_list [Sinks.NamedSink "SinkA"];
                  transforms =
                    KindCoverage.Transforms.Set.of_list [TaintTransform.Named "TransformX"];
                };
            };
            {
              RuleCoverage.CoveredRule.rule_code = 1003;
              kind_coverage =
                {
                  KindCoverage.sources =
                    KindCoverage.Sources.Set.of_list
                      [Sources.NamedSource "SourceC"; Sources.NamedSource "SourceD"];
                  sinks =
                    KindCoverage.Sinks.Set.of_list
                      [
                        Sinks.TriggeredPartialSink
                          { partial_sink = "SinkC[label_1]"; triggering_source = "SourceD" };
                        Sinks.TriggeredPartialSink
                          { partial_sink = "SinkC[label_2]"; triggering_source = "SourceC" };
                      ];
                  transforms = KindCoverage.Transforms.Set.empty;
                };
            };
          ];
      uncovered_rule_codes = RuleCoverage.IntSet.of_list [1002; 1004];
    }
  in
  assert_equal
    ~cmp:RuleCoverage.equal
    ~printer:RuleCoverage.show
    expected_category_coverage
    actual_category_coverage


let () =
  "rule_coverage"
  >::: ["covered_rule" >:: test_covered_rule; "rule_coverage" >:: test_rule_coverage]
  |> Test.run
