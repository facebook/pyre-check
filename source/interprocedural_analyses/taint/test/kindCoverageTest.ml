(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Taint
module AccessPath = Interprocedural.AccessPath

let test_from_source _ =
  let assert_sources ~expected ~actual =
    assert_equal
      ~cmp:(Option.equal Sources.equal)
      ~printer:(Option.value_map ~default:"None" ~f:Sources.show)
      expected
      actual
  in
  let source = Sources.NamedSource "SourceA" in
  assert_sources ~expected:(Some source) ~actual:(KindCoverage.Sources.from_source source);
  let source = Sources.Attach in
  assert_sources ~expected:None ~actual:(KindCoverage.Sources.from_source source);
  let source = Sources.ParametricSource { source_name = "SourceB"; subkind = "kind_1" } in
  assert_sources
    ~expected:(Some (Sources.NamedSource "SourceB"))
    ~actual:(KindCoverage.Sources.from_source source);
  let source =
    Sources.Transform
      {
        local = [TaintTransform.Named "TransformX1"];
        global =
          [
            TaintTransform.Named "TransformX2";
            TaintTransform.Sanitize
              (SanitizeTransformSet.add_sink
                 (SanitizeTransform.Sink.Named "SanitizeSinkA")
                 SanitizeTransformSet.bottom);
          ];
        base = Sources.NamedSource "SourceC";
      }
  in
  assert_sources
    ~expected:(Some (Sources.NamedSource "SourceC"))
    ~actual:(KindCoverage.Sources.from_source source)


let test_from_sink _ =
  let assert_sinks ~expected ~input () =
    assert_equal
      ~cmp:(Option.equal Sinks.Set.equal)
      ~printer:(Option.value_map ~default:"None" ~f:Sinks.Set.show)
      expected
      (KindCoverage.Sinks.from_sink input)
  in
  let sink = Sinks.NamedSink "SinkA" in
  assert_sinks ~expected:(Some (Sinks.Set.singleton sink)) ~input:sink ();
  let sink = Sinks.Attach in
  assert_sinks ~expected:None ~input:sink ();
  let sink = Sinks.PartialSink "SinkB[label_1]" in
  assert_sinks
    ~expected:(Some (Sinks.Set.singleton (Sinks.PartialSink "SinkB[label_1]")))
    ~input:sink
    ();
  let sink = Sinks.create_triggered_sink ~triggering_source:"SourceC" "SinkC[label_2]" in
  assert_sinks
    ~expected:(Some (Sinks.Set.singleton (Sinks.PartialSink "SinkC[label_2]")))
    ~input:sink
    ();
  let sink = Sinks.LocalReturn in
  assert_sinks ~expected:None ~input:sink ();
  let sink = Sinks.ParametricSink { sink_name = "SinkD"; subkind = "kind_2" } in
  assert_sinks ~expected:(Some (Sinks.Set.singleton (Sinks.NamedSink "SinkD"))) ~input:sink ();
  let sink =
    Sinks.ParameterUpdate
      (AccessPath.Root.PositionalParameter { position = 0; name = "x"; positional_only = false })
  in
  assert_sinks ~expected:None ~input:sink ();
  let sink = Sinks.AddFeatureToArgument in
  assert_sinks ~expected:None ~input:sink ();
  let sink = Sinks.ExtraTraceSink in
  assert_sinks ~expected:None ~input:sink ();
  let sink =
    Sinks.Transform
      {
        local =
          [
            TaintTransform.Named "TransformY1";
            TaintTransform.Sanitize
              (SanitizeTransformSet.add_source
                 (SanitizeTransform.Source.Named "SanitizeSourceA")
                 SanitizeTransformSet.bottom);
          ];
        global = [TaintTransform.Named "TransformY2"];
        base = Sinks.NamedSink "SinkE";
      }
  in
  assert_sinks ~expected:(Some (Sinks.Set.singleton (Sinks.NamedSink "SinkE"))) ~input:sink ()


let test_from_transform _ =
  let assert_transform ~expected ~actual =
    assert_equal
      ~cmp:(Option.equal TaintTransform.equal)
      ~printer:(Option.value_map ~default:"None" ~f:TaintTransform.show)
      expected
      actual
  in
  let transform = TaintTransform.Named "TransformX" in
  assert_transform
    ~expected:(Some (TaintTransform.Named "TransformX"))
    ~actual:(KindCoverage.NamedTransforms.from_transform transform);
  let transform = TaintTransform.TriggeredPartialSink { triggering_source = "SourceA" } in
  assert_transform ~expected:None ~actual:(KindCoverage.NamedTransforms.from_transform transform);
  let transform =
    TaintTransform.Sanitize
      (SanitizeTransformSet.add_sink
         (SanitizeTransform.Sink.Named "SanitizeSinkA")
         SanitizeTransformSet.bottom)
  in
  assert_transform ~expected:None ~actual:(KindCoverage.NamedTransforms.from_transform transform)


let test_from_model _ =
  let create_model ~generations ~taint_in_taint_out ~sink_taint =
    {
      Model.empty_model with
      forward = { generations };
      backward = { taint_in_taint_out; sink_taint };
      sanitizers =
        { global = Sanitize.bottom; parameters = Sanitize.bottom; roots = Sanitize.RootMap.bottom };
    }
  in
  let create_source_tree source_kind =
    Domains.ForwardState.Tree.create_leaf
      (Domains.ForwardTaint.singleton Domains.CallInfo.declaration source_kind Domains.Frame.bottom)
  in
  let create_sink_tree sink_kind =
    Domains.BackwardState.Tree.create_leaf
      (Domains.BackwardTaint.singleton Domains.CallInfo.declaration sink_kind Domains.Frame.bottom)
  in
  let model =
    create_model
      ~generations:
        (Domains.ForwardState.of_list
           [
             AccessPath.Root.Variable "x1", create_source_tree (Sources.NamedSource "SourceA");
             ( AccessPath.Root.Variable "x2",
               create_source_tree
                 (Sources.Transform
                    {
                      local = [TaintTransform.Named "TransformX1"];
                      global =
                        [
                          TaintTransform.Named "TransformX2";
                          TaintTransform.Sanitize
                            (SanitizeTransformSet.add_sink
                               (SanitizeTransform.Sink.Named "SanitizeSinkA")
                               SanitizeTransformSet.bottom);
                        ];
                      base = Sources.NamedSource "SourceB";
                    }) );
           ])
      ~taint_in_taint_out:
        (Domains.BackwardState.of_list
           [
             AccessPath.Root.Variable "y1", create_sink_tree (Sinks.NamedSink "SinkA");
             ( AccessPath.Root.Variable "y2",
               create_sink_tree
                 (Sinks.Transform
                    {
                      local =
                        [
                          TaintTransform.Named "TransformY1";
                          TaintTransform.Sanitize
                            (SanitizeTransformSet.add_source
                               (SanitizeTransform.Source.Named "SanitizeSourceA")
                               SanitizeTransformSet.bottom);
                        ];
                      global = [TaintTransform.Named "TransformY2"];
                      base = Sinks.NamedSink "SinkB";
                    }) );
           ])
      ~sink_taint:
        (Domains.BackwardState.of_list
           [
             AccessPath.Root.Variable "z1", create_sink_tree (Sinks.NamedSink "SinkC");
             ( AccessPath.Root.Variable "z2",
               create_sink_tree
                 (Sinks.Transform
                    {
                      local =
                        [
                          TaintTransform.Named "TransformZ1";
                          TaintTransform.Sanitize
                            (SanitizeTransformSet.add_source
                               (SanitizeTransform.Source.Named "SanitizeSourceB")
                               SanitizeTransformSet.bottom);
                        ];
                      global = [TaintTransform.Named "TransformZ2"];
                      base = Sinks.NamedSink "SinkD";
                    }) );
           ])
  in
  let actual_used_taint = KindCoverage.from_model model in
  let expected_used_taint =
    {
      KindCoverage.sources =
        KindCoverage.Sources.Set.of_list
          [Sources.NamedSource "SourceA"; Sources.NamedSource "SourceB"];
      sinks =
        KindCoverage.Sinks.Set.of_list
          [
            Sinks.NamedSink "SinkA";
            Sinks.NamedSink "SinkB";
            Sinks.NamedSink "SinkC";
            Sinks.NamedSink "SinkD";
          ];
      named_transforms =
        KindCoverage.NamedTransforms.Set.of_list
          [
            TaintTransform.Named "TransformX1";
            TaintTransform.Named "TransformX2";
            TaintTransform.Named "TransformY1";
            TaintTransform.Named "TransformY2";
            TaintTransform.Named "TransformZ1";
            TaintTransform.Named "TransformZ2";
          ];
    }
  in
  assert_equal
    ~cmp:KindCoverage.equal
    ~printer:KindCoverage.show
    expected_used_taint
    actual_used_taint


let test_from_rule _ =
  let rule =
    {
      Rule.sources = [Sources.NamedSource "SourceA"];
      sinks =
        [
          Sinks.NamedSink "SinkA";
          Sinks.PartialSink "SinkB[label_1]";
          Sinks.PartialSink "SinkB[label_2]";
        ];
      transforms =
        [
          TaintTransform.Named "TransformZ";
          TaintTransform.TriggeredPartialSink { triggering_source = "SourceB" };
        ];
      code = 1234;
      name = "Test Rule";
      message_format = "";
      filters = None;
      location = None;
    }
  in
  let actual_used_taint = KindCoverage.from_rule rule in
  let expected_used_taint =
    {
      KindCoverage.sources = KindCoverage.Sources.Set.of_list [Sources.NamedSource "SourceA"];
      sinks =
        KindCoverage.Sinks.Set.of_list
          [
            Sinks.NamedSink "SinkA";
            Sinks.PartialSink "SinkB[label_1]";
            Sinks.PartialSink "SinkB[label_2]";
          ];
      named_transforms =
        KindCoverage.NamedTransforms.Set.of_list [TaintTransform.Named "TransformZ"];
    }
  in
  assert_equal
    ~cmp:KindCoverage.equal
    ~printer:KindCoverage.show
    expected_used_taint
    actual_used_taint


let () =
  "kind_coverage"
  >::: [
         "from_source" >:: test_from_source;
         "from_sink" >:: test_from_sink;
         "from_transform" >:: test_from_transform;
         "from_model" >:: test_from_model;
         "from_rule" >:: test_from_rule;
       ]
  |> Test.run
