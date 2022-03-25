(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Taint
open Pyre
module Error = TaintConfiguration.Error
module Result = Core.Result

let parse configuration =
  let open Result in
  TaintConfiguration.parse
    [PyrePath.create_absolute "/taint.config", Yojson.Safe.from_string configuration]
  >>= TaintConfiguration.validate


let assert_parse configuration =
  match parse configuration with
  | Error errors ->
      let errors = List.map ~f:Error.show errors |> String.concat ~sep:"\n" in
      Format.sprintf "Unexpected error when parsing configuration: %s" errors |> assert_failure
  | Ok configuration -> configuration


let assert_parse_error ~errors configuration =
  let configuration =
    parse configuration |> Core.Result.map_error ~f:(List.map ~f:(fun { Error.kind; _ } -> kind))
  in
  let printer = function
    | Ok _ -> "Ok _"
    | Error errors -> List.map ~f:Error.show_kind errors |> String.concat ~sep:"\n"
  in
  assert_equal ~printer (Error errors) configuration


let named name = { AnnotationParser.name; kind = Named }

let test_simple _ =
  let configuration =
    assert_parse
      {|
    { sources: [
        { name: "A" },
        { name: "B" }
      ],
      sinks: [
        { name: "C" },
        { name: "D" }
      ],
      features: [
        { name: "E" },
        { name: "F" }
      ],
      rules: [
        {
           name: "test rule",
           sources: ["A"],
           sinks: ["D"],
           code: 2001,
           message_format: "whatever"
        }
      ],
      options: {
        maximum_overrides_to_analyze: 50
      }
    }
  |}
  in
  assert_equal configuration.sources [named "A"; named "B"];
  assert_equal configuration.sinks [named "C"; named "D"];
  assert_equal configuration.features ["E"; "F"];
  assert_equal (List.length configuration.rules) 1;
  assert_equal (List.hd_exn configuration.rules).code 2001;
  assert_equal
    ~cmp:(Option.equal Int.equal)
    configuration.analysis_model_constraints.maximum_overrides_to_analyze
    (Some 50);
  assert_equal
    (Sources.Map.of_alist_exn [Sources.NamedSource "A", Sinks.Set.of_list [Sinks.NamedSink "D"]])
    configuration.matching_sinks


let test_transform _ =
  let configuration =
    assert_parse
      {|
    { sources:[],
      sinks:[],
      rules:[
        {
           name: "test rule",
           sources: [],
           sinks: [],
           code: 2001,
           message_format: "transforms are optional"
        }
      ]
    }
  |}
  in
  assert_equal configuration.transforms [];
  assert_equal (List.length configuration.rules) 1;
  assert_equal (List.hd_exn configuration.rules).transforms [];
  let configuration =
    assert_parse
      {|
    { sources:[],
      sinks:[],
      transforms: [
        {name: "A"}
      ],
      rules:[
        {
           name: "test rule",
           sources: [],
           sinks: [],
           transforms: ["A", "A"],
           code: 2001,
           message_format: "transforms can repeat in rules"
        }
      ]
    }
  |}
  in
  assert_equal
    (List.hd_exn configuration.rules).transforms
    [TaintTransform.Named "A"; TaintTransform.Named "A"];
  ()


let test_transform_splits _ =
  assert_equal [[], []] (TaintConfiguration.transform_splits []);
  assert_equal [["T1"], []; [], ["T1"]] (TaintConfiguration.transform_splits ["T1"]);
  assert_equal
    [["T2"; "T1"], []; ["T1"], ["T2"]; [], ["T1"; "T2"]]
    (TaintConfiguration.transform_splits ["T1"; "T2"]);
  assert_equal
    [["T3"; "T2"; "T1"], []; ["T2"; "T1"], ["T3"]; ["T1"], ["T2"; "T3"]; [], ["T1"; "T2"; "T3"]]
    (TaintConfiguration.transform_splits ["T1"; "T2"; "T3"]);
  ()


let test_invalid_source _ =
  assert_parse_error
    ~errors:[Error.UnsupportedSource "C"]
    {|
    { sources: [
        { name: "A" },
        { name: "B" }
      ],
      sinks: [
      ],
      features: [
      ],
      rules: [
        {
           name: "test rule",
           sources: ["C"],
           sinks: [],
           code: 2001,
           message_format: "whatever"
        }
      ]
    }
    |}


let test_invalid_sink _ =
  assert_parse_error
    ~errors:[Error.UnsupportedSink "B"]
    {|
    { sources: [
      ],
      sinks: [
        { name: "A" }
      ],
      features: [
      ],
      rules: [
        {
           name: "test rule",
           sources: [],
           sinks: ["B"],
           code: 2001,
           message_format: "whatever"
        }
      ]
    }
    |}


let test_invalid_transform _ =
  assert_parse_error
    ~errors:[Error.UnsupportedTransform "B"]
    {|
    { sources: [
      ],
      sinks: [
        { name: "A" }
      ],
      transforms: [
        { name: "A" }
      ],
      features: [
      ],
      rules: [
        {
           name: "test rule",
           sources: [],
           sinks: [],
           transforms: ["B"],
           code: 2001,
           message_format: "whatever"
        }
      ]
    }
    |}


let test_combined_source_rules _ =
  assert_parse_error
    ~errors:
      [
        Error.UnexpectedJsonType
          { json = `Null; message = "Expected string, got null"; section = Some "partial_sink" };
      ]
    {|
      { sources: [
          { name: "A" },
          { name: "B" }
        ],
        combined_source_rules: [
          {
             name: "test combined rule",
             sources: {"a": "A", "b": "B"},
             sinks: ["C"],
             code: 2001,
             message_format: "some form"
          }
        ]
      }
    |};
  let configuration =
    assert_parse
      {|
    { sources: [
        { name: "A" },
        { name: "B" }
      ],
      combined_source_rules: [
        {
           name: "test combined rule",
           sources: {"a": "A", "b": "B"},
           partial_sink: "C",
           code: 2001,
           message_format: "some form"
        }
      ]
    }
  |}
  in
  assert_equal configuration.sources [named "A"; named "B"];
  assert_equal configuration.sinks [];
  assert_equal
    ~printer:(List.to_string ~f:Taint.TaintConfiguration.Rule.show)
    ~cmp:(List.equal [%compare.equal: Taint.TaintConfiguration.Rule.t])
    configuration.rules
    [
      {
        Taint.TaintConfiguration.Rule.sources = [Sources.NamedSource "A"];
        sinks = [Sinks.TriggeredPartialSink { kind = "C"; label = "a" }];
        transforms = [];
        code = 2001;
        message_format = "some form";
        name = "test combined rule";
      };
      {
        Taint.TaintConfiguration.Rule.sources = [Sources.NamedSource "B"];
        sinks = [Sinks.TriggeredPartialSink { kind = "C"; label = "b" }];
        transforms = [];
        code = 2001;
        message_format = "some form";
        name = "test combined rule";
      };
    ];
  assert_equal (List.hd_exn configuration.rules).code 2001;
  assert_equal (String.Map.Tree.to_alist configuration.partial_sink_labels) ["C", ["a"; "b"]];
  let configuration =
    assert_parse
      {|
    { sources: [
        { name: "A" },
        { name: "B" },
        { name: "C" }
      ],
      sinks: [],
      combined_source_rules: [
        {
           name: "test combined rule",
           sources: {"a": "A", "b": ["B", "C"]},
           partial_sink: "CombinedSink",
           code: 2001,
           message_format: "some form"
        }
      ]
    }
  |}
  in
  assert_equal configuration.sources [named "A"; named "B"; named "C"];
  assert_equal configuration.sinks [];
  assert_equal
    (String.Map.Tree.to_alist configuration.partial_sink_labels)
    ["CombinedSink", ["a"; "b"]];
  assert_equal
    ~printer:(List.to_string ~f:Taint.TaintConfiguration.Rule.show)
    ~cmp:(List.equal [%compare.equal: Taint.TaintConfiguration.Rule.t])
    configuration.rules
    [
      {
        Taint.TaintConfiguration.Rule.sources = [Sources.NamedSource "A"];
        sinks = [Sinks.TriggeredPartialSink { kind = "CombinedSink"; label = "a" }];
        transforms = [];
        code = 2001;
        message_format = "some form";
        name = "test combined rule";
      };
      {
        Taint.TaintConfiguration.Rule.sources = [Sources.NamedSource "B"; Sources.NamedSource "C"];
        sinks = [Sinks.TriggeredPartialSink { kind = "CombinedSink"; label = "b" }];
        transforms = [];
        code = 2001;
        message_format = "some form";
        name = "test combined rule";
      };
    ];
  assert_equal (List.hd_exn configuration.rules).code 2001;
  assert_parse_error
    ~errors:[Error.PartialSinkDuplicate "C"]
    {|
      { sources: [
          { name: "A" },
          { name: "B" }
        ],
        combined_source_rules: [
          {
             name: "test combined rule",
             sources: {"a": "A", "b": "B"},
             partial_sink: "C",
             code: 2001,
             message_format: "some form"
          },
          {
             name: "test combined rule",
             sources: {"a": "A", "b": "B"},
             partial_sink: "C",
             code: 2002,
             message_format: "other form"
          }
        ]
      }
    |}


let test_lineage_analysis _ =
  let configuration =
    assert_parse
      {|
        { sources:[],
          sinks: [],
          rules: []
         }
      |}
  in
  assert_equal configuration.lineage_analysis false;
  let configuration =
    assert_parse
      {|
          { sources:[],
            sinks: [],
            rules: [],
            lineage_analysis: true
          }
      |}
  in
  assert_equal configuration.lineage_analysis true


let test_partial_sink_converter _ =
  let assert_triggered_sinks configuration ~partial_sink ~source ~expected_sink =
    assert_parse configuration |> Taint.TaintConfiguration.register;
    Taint.TaintConfiguration.get_triggered_sink ~partial_sink ~source
    |> assert_equal
         ~cmp:(Option.equal Sinks.equal)
         ~printer:(fun value -> value >>| Sinks.show |> Option.value ~default:"None")
         expected_sink
  in
  assert_triggered_sinks
    {|
    {
      sources: [{ name: "A" }, { name: "B" }],
      sinks: [],
      combined_source_rules: [
        {
           name: "c rule",
           sources: {"ca": "A", "cb": "B"},
           partial_sink: "C",
           code: 2001,
           message_format: "some form"
        }
      ]
    }
  |}
    ~partial_sink:{ Sinks.kind = "C"; label = "ca" }
    ~source:(Sources.NamedSource "A")
    ~expected_sink:(Some (Sinks.TriggeredPartialSink { Sinks.kind = "C"; label = "cb" }));
  assert_triggered_sinks
    {|
    {
      sources: [{ name: "A" }, { name: "B" }],
      sinks: [],
      combined_source_rules: [
        {
           name: "c rule",
           sources: {"ca": "A", "cb": "B"},
           partial_sink: "C",
           code: 2001,
           message_format: "some form"
        }
      ]
    }
  |}
    ~partial_sink:{ Sinks.kind = "C"; label = "cb" }
    ~source:(Sources.NamedSource "B")
    ~expected_sink:(Some (Sinks.TriggeredPartialSink { Sinks.kind = "C"; label = "ca" }));
  assert_triggered_sinks
    {|
    {
      sources: [{ name: "A" }, { name: "B" }],
      sinks: [],
      combined_source_rules: [
        {
           name: "c rule",
           sources: {"ca": "A", "cb": "B"},
           partial_sink: "C",
           code: 2001,
           message_format: "some form"
        }
      ]
    }
  |}
    ~partial_sink:{ Sinks.kind = "C"; label = "ca" }
    ~source:(Sources.NamedSource "B")
    ~expected_sink:None;
  assert_triggered_sinks
    {|
    {
      sources: [{ name: "A" }, { name: "B" }],
      sinks: [],
      combined_source_rules: [
        {
           name: "c rule",
           sources: {"ca": "A", "cb": "B"},
           partial_sink: "C",
           code: 2001,
           message_format: "some form"
        }
      ]
    }
  |}
    ~partial_sink:{ Sinks.kind = "C"; label = "cb" }
    ~source:(Sources.NamedSource "A")
    ~expected_sink:None


let test_multiple_configurations _ =
  let configuration =
    TaintConfiguration.parse
      [
        ( PyrePath.create_absolute "/a.config",
          Yojson.Safe.from_string
            {|
              { sources: [
                  { name: "A" },
                  { name: "B" }
                ],
                sinks: [
                  { name: "C" },
                  { name: "D" }
                ],
                features: [
                  { name: "E" },
                  { name: "F" }
                ],
                rules: [],
                options: {
                  maximum_overrides_to_analyze: 42
                }
               }
           |}
        );
        ( PyrePath.create_absolute "/b.config",
          Yojson.Safe.from_string
            {|
              {
                sources: [],
                sinks: [],
                features: [],
                rules: [
                  {
                     name: "test rule",
                     sources: ["A"],
                     sinks: ["D"],
                     code: 2001,
                     message_format: "whatever"
                  }
                ]
              }
            |}
        );
      ]
  in
  let configuration =
    match configuration with
    | Error _ -> assert_failure "Failed to parse configuration"
    | Ok configuration -> configuration
  in
  assert_equal configuration.sources [named "A"; named "B"];
  assert_equal configuration.sinks [named "C"; named "D"];
  assert_equal configuration.features ["E"; "F"];
  assert_equal (List.length configuration.rules) 1;
  assert_equal (List.hd_exn configuration.rules).code 2001;
  assert_equal
    ~cmp:(Option.equal Int.equal)
    configuration.analysis_model_constraints.maximum_overrides_to_analyze
    (Some 42)


let test_validate _ =
  let assert_parse_multiple_error ~error configurations =
    let result =
      configurations
      |> List.map ~f:(fun (path, content) ->
             PyrePath.create_absolute path, Yojson.Safe.from_string content)
      |> Taint.TaintConfiguration.parse
      |> Core.Result.map_error
           ~f:(List.map ~f:(fun { Error.path; kind } -> path >>| PyrePath.absolute, kind))
    in
    let printer = function
      | Ok _ -> "Ok _"
      | Error errors ->
          List.map ~f:[%show: string option * Error.kind] errors |> String.concat ~sep:"\n"
    in
    assert_equal ~printer (Error [error]) result
  in
  assert_parse_error
    ~errors:[Error.SourceDuplicate "UserControlled"]
    {|
    {
      sources: [
       { name: "UserControlled" },
       { name: "UserControlled" }
      ]
    }
    |};
  assert_parse_error
    ~errors:[Error.SinkDuplicate "Test"]
    {|
    {
      sources: [
      ],
      sinks: [
       { name: "Test" },
       { name: "Test" }
      ]
    }
    |};
  assert_parse_error
    ~errors:[Error.TransformDuplicate "Test"]
    {|
    {
      sources: [
      ],
      sinks: [
      ],
      transforms: [
        { name: "Test" },
        { name: "Test" }
      ]
    }
    |};
  assert_parse_error
    ~errors:[Error.FeatureDuplicate "concat"]
    {|
    {
      sources: [
       { name: "UserControlled" }
      ],
      sinks: [
       { name: "Test" }
      ],
      features: [
       { name: "concat" },
       { name: "concat" }
      ]
    }
    |};
  assert_parse_error
    ~errors:
      [
        Error.SourceDuplicate "UserControlled";
        Error.SinkDuplicate "Test";
        Error.FeatureDuplicate "concat";
      ]
    {|
    {
      sources: [
       { name: "UserControlled" },
       { name: "UserControlled" }
      ],
      sinks: [
       { name: "Test" },
       { name: "Test" }
      ],
      features: [
       { name: "concat" },
       { name: "concat" }
      ]
    }
    |};
  assert_parse_error
    ~errors:[Error.SourceDuplicate "UserControlled"]
    {|
    {
      sources: [
       {
         name: "UserControlled",
         comment: "First copy of user controlled"
       },
       {
         name: "UserControlled",
         comment: "Another copy of user controlled"
       }
      ]
    }
    |};
  assert_parse_error
    ~errors:[Error.SinkDuplicate "Test"]
    {|
    {
      sinks: [
       {
         name: "Test"
       },
       {
         name: "Test",
         kind: "parametric"
       }
      ]
    }
    |};
  assert_parse_error
    ~errors:[Error.RuleCodeDuplicate 2001]
    {|
    {
      sources: [
        { name: "A" }
      ],
      sinks: [
       { name: "B" },
       { name: "C" }
      ],
      rules: [
        {
           name: "test rule",
           sources: ["A"],
           sinks: ["B"],
           code: 2001,
           message_format: "whatever"
        },
        {
           name: "test rule",
           sources: ["A"],
           sinks: ["C"],
           code: 2001,
           message_format: "format"
        }
      ]
    }
    |};
  assert_parse_error
    ~errors:[Error.RuleCodeDuplicate 2002]
    {|
    {
      sources: [
        { name: "A" },
        { name: "B" }
      ],
      sinks: [
        { name: "C" },
        { name: "D" }
      ],
      rules: [
        {
           name: "test rule",
           sources: ["A"],
           sinks: ["D"],
           code: 2002,
           message_format: "whatever"
        }
      ],
      combined_source_rules: [
        {
           name: "test combined rule",
           sources: {"a": "A", "b": "B"},
           sinks: ["C"],
           code: 2002,
           message_format: "some form"
        }
      ]
    }
    |};
  assert_parse_multiple_error
    ~error:(None, Error.OptionDuplicate "maximum_overrides_to_analyze")
    [
      ( "/a.config",
        {|
          {
            sources: [],
            sinks: [],
            features: [],
            rules: [],
            options: {
              maximum_overrides_to_analyze: 50
            }
          }
        |}
      );
      ( "/b.config",
        {|
          {
            sources: [],
            sinks: [],
            features: [],
            rules: [],
            options: {
              maximum_overrides_to_analyze: 60
            }
          }
        |}
      );
    ];
  assert_parse_multiple_error
    ~error:(None, Error.OptionDuplicate "maximum_trace_length")
    [
      ( "/a.config",
        {|
          {
            sources: [],
            sinks: [],
            features: [],
            rules: [],
            options: {
              maximum_trace_length: 10
            }
          }
        |}
      );
      ( "/b.config",
        {|
          {
            sources: [],
            sinks: [],
            features: [],
            rules: [],
            options: {
              maximum_trace_length: 20
            }
          }
        |}
      );
    ];
  assert_parse_multiple_error
    ~error:(None, Error.OptionDuplicate "maximum_tito_depth")
    [
      ( "/a.config",
        {|
          {
            sources: [],
            sinks: [],
            features: [],
            rules: [],
            options: {
              maximum_tito_depth: 10
            }
          }
        |}
      );
      ( "/b.config",
        {|
          {
            sources: [],
            sinks: [],
            features: [],
            rules: [],
            options: {
              maximum_tito_depth: 20
            }
          }
        |}
      );
    ];
  assert_parse_error
    ~errors:[Error.UnsupportedSource "MisspelledStringDigit"]
    {|
        {
          sources: [{ name: "StringDigit" }],
          sinks: [],
          features: [],
          rules: [],
          implicit_sources: {
            literal_strings: [
              {
                "regexp": "^\\d+$",
                "kind": "MisspelledStringDigit"
              }
            ]
          }
        }
    |};
  assert_parse_error
    ~errors:[Error.UnsupportedSink "Misspelled"]
    {|
          {
            sources: [],
            sinks: [{ name: "Test" }],
            features: [],
            rules: [],
            implicit_sinks: {
              literal_strings: [
                {
                  "regexp": "^\\d+$",
                  "kind": "Misspelled"
                }
              ]
            }
          }
    |}


let test_implicit_sources _ =
  let configuration =
    assert_parse
      {|
          { sources: [{ name: "StringDigit" }],
            sinks: [],
            features: [],
            rules: [],
            implicit_sources: {
              literal_strings: [
                {
                  "regexp": "^\\d+$",
                  "kind": "StringDigit"
                }
              ]
            }
           }
       |}
  in
  assert_equal configuration.sources [named "StringDigit"];
  match configuration.implicit_sources with
  | { TaintConfiguration.literal_strings = [{ pattern; source_kind }] } ->
      assert_equal ~cmp:Sources.equal source_kind (Sources.NamedSource "StringDigit");
      assert_equal
        ~cmp:(fun left right -> Re2.compare left right = 0)
        pattern
        (Re2.create_exn "^\\d+$")
  | _ -> Test.assert_unreached ()


let test_implicit_sinks _ =
  let configuration =
    assert_parse
      {|
          { sources: [],
            sinks: [{ name: "HTMLContainer" }],
            features: [],
            rules: [],
            implicit_sinks: {
              literal_strings: [
                {
                  "regexp": "<.*>",
                  "kind": "HTMLContainer"
                }
              ]
            }
           }
     |}
  in
  assert_equal configuration.sinks [named "HTMLContainer"];
  match configuration.implicit_sinks with
  | { TaintConfiguration.literal_string_sinks = [{ pattern; sink_kind }]; _ } ->
      assert_equal ~cmp:Sinks.equal sink_kind (Sinks.NamedSink "HTMLContainer");
      assert_equal
        ~cmp:(fun left right -> Re2.compare left right = 0)
        pattern
        (Re2.create_exn "<.*>")
  | _ -> Test.assert_unreached ()


let test_matching_kinds _ =
  let assert_matching ~configuration ~matching_sources ~matching_sinks ~possible_tito_transforms =
    let configuration = assert_parse configuration in
    let matching_sources_printer matching =
      matching
      |> Sources.Map.to_alist
      |> List.map ~f:(fun (source, sinks) ->
             Format.asprintf "%a -> %a" Sources.pp source Sinks.Set.pp sinks)
      |> String.concat ~sep:", "
      |> Format.asprintf "{%s}"
    in
    assert_equal
      ~printer:matching_sources_printer
      ~cmp:(Sources.Map.equal Sinks.Set.equal)
      (Sources.Map.of_alist_exn matching_sinks)
      configuration.matching_sinks;
    let matching_sinks_printer matching =
      matching
      |> Sinks.Map.to_alist
      |> List.map ~f:(fun (sink, sources) ->
             Format.asprintf "%a -> %a" Sinks.pp sink Sources.Set.pp sources)
      |> String.concat ~sep:", "
      |> Format.asprintf "{%s}"
    in
    assert_equal
      ~printer:matching_sinks_printer
      ~cmp:(Sinks.Map.equal Sources.Set.equal)
      (Sinks.Map.of_alist_exn matching_sources)
      configuration.matching_sources;
    let possible_tito_transforms_printer possible =
      possible
      |> TaintTransforms.Set.elements
      |> List.map ~f:TaintTransforms.show_transforms
      |> String.concat ~sep:", "
      |> Format.asprintf "{%s}"
    in
    assert_equal
      ~printer:possible_tito_transforms_printer
      ~cmp:TaintTransforms.Set.equal
      (TaintTransforms.Set.of_list possible_tito_transforms)
      configuration.possible_tito_transforms
  in
  assert_matching
    ~configuration:
      {|
        { sources: [
            { name: "A" },
            { name: "B" }
          ],
          sinks: [
            { name: "C" },
            { name: "D" }
          ],
          rules: [
            {
               name: "test rule",
               sources: ["A"],
               sinks: ["C", "D"],
               code: 1,
               message_format: ""
            }
          ]
        }
      |}
    ~matching_sinks:
      [Sources.NamedSource "A", Sinks.Set.of_list [Sinks.NamedSink "C"; Sinks.NamedSink "D"]]
    ~matching_sources:
      [
        Sinks.NamedSink "C", Sources.Set.of_list [Sources.NamedSource "A"];
        Sinks.NamedSink "D", Sources.Set.of_list [Sources.NamedSource "A"];
      ]
    ~possible_tito_transforms:[TaintTransforms.empty];
  assert_matching
    ~configuration:
      {|
        { sources: [
            { name: "A" },
            { name: "B" }
          ],
          sinks: [
            { name: "C" },
            { name: "D" }
          ],
          rules: [
            {
               name: "test rule",
               sources: ["A", "B"],
               sinks: ["D"],
               code: 1,
               message_format: ""
            }
          ]
        }
      |}
    ~matching_sinks:
      [
        Sources.NamedSource "A", Sinks.Set.of_list [Sinks.NamedSink "D"];
        Sources.NamedSource "B", Sinks.Set.of_list [Sinks.NamedSink "D"];
      ]
    ~matching_sources:
      [Sinks.NamedSink "D", Sources.Set.of_list [Sources.NamedSource "A"; Sources.NamedSource "B"]]
    ~possible_tito_transforms:[TaintTransforms.empty];
  assert_matching
    ~configuration:
      {|
        { sources: [
            { name: "A" },
            { name: "B" }
          ],
          sinks: [
            { name: "C" },
            { name: "D" }
          ],
          rules: [
            {
               name: "test rule",
               sources: ["A"],
               sinks: ["C"],
               code: 1,
               message_format: ""
            },
            {
               name: "test rule 2",
               sources: ["A"],
               sinks: ["D"],
               code: 2,
               message_format: ""
            }
          ]
        }
      |}
    ~matching_sinks:
      [Sources.NamedSource "A", Sinks.Set.of_list [Sinks.NamedSink "C"; Sinks.NamedSink "D"]]
    ~matching_sources:
      [
        Sinks.NamedSink "C", Sources.Set.of_list [Sources.NamedSource "A"];
        Sinks.NamedSink "D", Sources.Set.of_list [Sources.NamedSource "A"];
      ]
    ~possible_tito_transforms:[TaintTransforms.empty];
  let transformed_source transform_names source_name =
    Sources.Transform
      {
        base = Sources.NamedSource source_name;
        global =
          List.map transform_names ~f:(fun name -> TaintTransform.Named name)
          |> TaintTransforms.of_named_transforms;
        local = TaintTransforms.empty;
      }
  in
  let transformed_sink transform_names sink_name =
    Sinks.Transform
      {
        base = Sinks.NamedSink sink_name;
        global =
          List.map transform_names ~f:(fun name -> TaintTransform.Named name)
          |> TaintTransforms.of_named_transforms;
        local = TaintTransforms.empty;
      }
  in
  assert_matching
    ~configuration:
      {|
        { sources: [
            { name: "A" },
            { name: "B" }
          ],
          sinks: [
            { name: "C" },
            { name: "D" }
          ],
          transforms: [
            { name: "X" },
            { name: "Y" }
          ],
          rules: [
            {
               name: "test rule",
               sources: ["A"],
               transforms: ["X", "Y"],
               sinks: ["C", "D"],
               code: 1,
               message_format: ""
            }
          ]
        }
      |}
    ~matching_sinks:
      [
        ( Sources.NamedSource "A",
          Sinks.Set.of_list [transformed_sink ["X"; "Y"] "C"; transformed_sink ["X"; "Y"] "D"] );
        ( transformed_source ["X"] "A",
          Sinks.Set.of_list [transformed_sink ["Y"] "C"; transformed_sink ["Y"] "D"] );
        ( transformed_source ["Y"; "X"] "A",
          Sinks.Set.of_list [Sinks.NamedSink "C"; Sinks.NamedSink "D"] );
      ]
    ~matching_sources:
      [
        Sinks.NamedSink "C", Sources.Set.of_list [transformed_source ["Y"; "X"] "A"];
        Sinks.NamedSink "D", Sources.Set.of_list [transformed_source ["Y"; "X"] "A"];
        transformed_sink ["Y"] "C", Sources.Set.of_list [transformed_source ["X"] "A"];
        transformed_sink ["Y"] "D", Sources.Set.of_list [transformed_source ["X"] "A"];
        transformed_sink ["X"; "Y"] "C", Sources.Set.of_list [Sources.NamedSource "A"];
        transformed_sink ["X"; "Y"] "D", Sources.Set.of_list [Sources.NamedSource "A"];
      ]
    ~possible_tito_transforms:
      [
        TaintTransforms.empty;
        TaintTransforms.of_named_transforms [TaintTransform.Named "X"];
        TaintTransforms.of_named_transforms [TaintTransform.Named "X"; TaintTransform.Named "Y"];
        TaintTransforms.of_named_transforms [TaintTransform.Named "Y"];
      ];
  ()


let () =
  "configuration"
  >::: [
         "combined_source_rules" >:: test_combined_source_rules;
         "implicit_sources" >:: test_implicit_sources;
         "implicit_sinks" >:: test_implicit_sinks;
         "invalid_sink" >:: test_invalid_sink;
         "invalid_source" >:: test_invalid_source;
         "invalid_transform" >:: test_invalid_transform;
         "lineage_analysis" >:: test_lineage_analysis;
         "multiple_configurations" >:: test_multiple_configurations;
         "partial_sink_converter" >:: test_partial_sink_converter;
         "simple" >:: test_simple;
         "transform" >:: test_transform;
         "transform_splits" >:: test_transform_splits;
         "validate" >:: test_validate;
         "matching_kinds" >:: test_matching_kinds;
       ]
  |> Test.run
