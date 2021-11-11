(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Taint
open Pyre

let parse configuration = TaintConfiguration.parse [Yojson.Safe.from_string configuration]

let named name = { AnnotationParser.name; kind = Named }

let test_simple _ =
  let configuration =
    parse
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


let test_invalid_source _ =
  let parse () =
    parse
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
  in
  assert_raises (Failure "Unsupported taint source `C`") parse


let test_invalid_sink _ =
  let parse () =
    parse
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
  in
  assert_raises (Failure "Unsupported taint sink `B`") parse


let test_combined_source_rules _ =
  let assert_fails configuration ~expected =
    assert_raises expected (fun () -> parse configuration |> ignore)
  in
  assert_fails
    ~expected:(Yojson.Safe.Util.Type_error ("Expected string, got null", `Null))
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
    parse
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
        code = 2001;
        message_format = "some form";
        name = "test combined rule";
      };
      {
        Taint.TaintConfiguration.Rule.sources = [Sources.NamedSource "B"];
        sinks = [Sinks.TriggeredPartialSink { kind = "C"; label = "b" }];
        code = 2001;
        message_format = "some form";
        name = "test combined rule";
      };
    ];
  assert_equal (List.hd_exn configuration.rules).code 2001;
  assert_equal (String.Map.Tree.to_alist configuration.partial_sink_labels) ["C", ["a"; "b"]];
  let configuration =
    parse
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
        code = 2001;
        message_format = "some form";
        name = "test combined rule";
      };
      {
        Taint.TaintConfiguration.Rule.sources = [Sources.NamedSource "B"; Sources.NamedSource "C"];
        sinks = [Sinks.TriggeredPartialSink { kind = "CombinedSink"; label = "b" }];
        code = 2001;
        message_format = "some form";
        name = "test combined rule";
      };
    ];
  assert_equal (List.hd_exn configuration.rules).code 2001;
  assert_fails
    ~expected:(Failure "Partial sinks must be unique - an entry for `C` already exists.")
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


let test_empty _ =
  assert_raises (Yojson.Json_error "Blank input data") (fun () ->
      let _ = parse {| |} in
      ())


let test_lineage_analysis _ =
  let configuration =
    TaintConfiguration.parse
      [
        Yojson.Safe.from_string
          {|
          { sources:[],
            sinks: [],
            rules: []
           }
           |};
      ]
  in
  assert_equal configuration.lineage_analysis false;
  let configuration =
    TaintConfiguration.parse
      [
        Yojson.Safe.from_string
          {|
          { sources:[],
            sinks: [],
            rules: [],
            lineage_analysis: true
           }
           |};
      ]
  in
  assert_equal configuration.lineage_analysis true


let test_partial_sink_converter _ =
  let assert_triggered_sinks configuration ~partial_sink ~source ~expected_sink =
    parse configuration |> Taint.TaintConfiguration.register;
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
           |};
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
          |};
      ]
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
  let assert_validation_error_with_multiple_configurations ~error configurations =
    assert_raises (Failure error) (fun () ->
        Taint.TaintConfiguration.parse (List.map configurations ~f:Yojson.Safe.from_string)
        |> Taint.TaintConfiguration.validate)
  in
  let assert_validation_error ~error configuration =
    assert_validation_error_with_multiple_configurations ~error [configuration]
  in
  assert_validation_error
    ~error:"Duplicate entry for source: `UserControlled`"
    {|
    {
      sources: [
       { name: "UserControlled" },
       { name: "UserControlled" }
      ]
    }
    |};
  assert_validation_error
    ~error:"Duplicate entry for sink: `Test`"
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
  assert_validation_error
    ~error:"Duplicate entry for feature: `concat`"
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
  (* We only surface one error. *)
  assert_validation_error
    ~error:"Duplicate entry for source: `UserControlled`"
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
  assert_validation_error
    ~error:"Duplicate entry for source: `UserControlled`"
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
  assert_validation_error
    ~error:"Duplicate entry for sink: `Test`"
    {|
    {
      sinks: [
       {
         name: "Test"
       },
       {
         name: "Test",
         multi_sink_labels: ["a", "b"]
       }
      ]
    }
    |};
  assert_validation_error
    ~error:"Multiple rules share the same code `2001`."
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
  assert_validation_error
    ~error:"Multiple rules share the same code `2002`."
    {|
    {
      sources: [
        { name: "A" },
        { name: "B" }
      ],
      sinks: [
        { name: "C", multi_sink_labels: ["a", "b"] },
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
  assert_validation_error_with_multiple_configurations
    ~error:"Multiple values were passed in for `maximum_overrides_to_analyze`."
    [
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
      |};
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
      |};
    ];
  assert_validation_error_with_multiple_configurations
    ~error:"Multiple values were passed in for `maximum_trace_length`."
    [
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
      |};
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
      |};
    ];
  assert_validation_error_with_multiple_configurations
    ~error:"Multiple values were passed in for `maximum_tito_depth`."
    [
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
      |};
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
      |};
    ];
  assert_validation_error
    ~error:"Unsupported taint source `MisspelledStringDigit`"
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
  assert_validation_error
    ~error:"Unsupported taint sink `Misspelled`"
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
    TaintConfiguration.parse
      [
        Yojson.Safe.from_string
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
           |};
      ]
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
    TaintConfiguration.parse
      [
        Yojson.Safe.from_string
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
           |};
      ]
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
  let assert_matching ~configuration ~matching_sources ~matching_sinks =
    let configuration = parse configuration in
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
      configuration.matching_sources
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
      ];
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
      [Sinks.NamedSink "D", Sources.Set.of_list [Sources.NamedSource "A"; Sources.NamedSource "B"]];
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
      ];
  ()


let () =
  "configuration"
  >::: [
         "combined_source_rules" >:: test_combined_source_rules;
         "empty" >:: test_empty;
         "implicit_sources" >:: test_implicit_sources;
         "implicit_sinks" >:: test_implicit_sinks;
         "invalid_sink" >:: test_invalid_sink;
         "invalid_source" >:: test_invalid_source;
         "lineage_analysis" >:: test_lineage_analysis;
         "multiple_configurations" >:: test_multiple_configurations;
         "partial_sink_converter" >:: test_partial_sink_converter;
         "simple" >:: test_simple;
         "validate" >:: test_validate;
         "matching_kinds" >:: test_matching_kinds;
       ]
  |> Test.run
