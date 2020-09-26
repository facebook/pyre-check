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
  assert_equal configuration.sources ["A"; "B"];
  assert_equal configuration.sinks ["D"; "C"];
  assert_equal configuration.features ["E"; "F"];
  assert_equal (List.length configuration.rules) 1;
  assert_equal (List.hd_exn configuration.rules).code 2001;
  assert_equal
    ~cmp:(Option.equal Int.equal)
    configuration.analysis_model_constraints.maximum_overrides_to_analyze
    (Some 50)


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
  let configuration =
    parse
      {|
    { sources: [
        { name: "A" },
        { name: "B" }
      ],
      sinks: [
        { name: "C", multi_sink_labels: ["a", "b"] }
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
  |}
  in
  assert_equal configuration.sources ["A"; "B"];
  assert_equal configuration.sinks ["C"];
  assert_equal
    ~printer:(List.to_string ~f:Taint.TaintConfiguration.Rule.show)
    ~cmp:(List.equal Taint.TaintConfiguration.Rule.equal)
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
  assert_equal configuration.sources ["A"; "B"];
  assert_equal configuration.sinks [];
  assert_equal
    ~printer:(List.to_string ~f:Taint.TaintConfiguration.Rule.show)
    ~cmp:(List.equal Taint.TaintConfiguration.Rule.equal)
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
      sinks: [
        { name: "CombinedSink", multi_sink_labels: ["a", "b"] }
      ],
      combined_source_rules: [
        {
           name: "test combined rule",
           sources: {"a": "A", "b": ["B", "C"]},
           sinks: ["CombinedSink"],
           code: 2001,
           message_format: "some form"
        }
      ]
    }
  |}
  in
  assert_equal configuration.sources ["A"; "B"; "C"];
  assert_equal configuration.sinks ["CombinedSink"];
  assert_equal
    ~printer:(List.to_string ~f:Taint.TaintConfiguration.Rule.show)
    ~cmp:(List.equal Taint.TaintConfiguration.Rule.equal)
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
  let assert_fails configuration ~expected =
    assert_raises expected (fun () -> parse configuration |> ignore)
  in
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


let test_combined_source_parse_errors _ =
  let assert_fails configuration ~expected =
    assert_raises expected (fun () -> parse configuration |> ignore)
  in
  assert_fails
    {|
    {
      sources: [
        {  name: "A" }
      ],
      sinks: [
        {
          name: "B",
          multi_sink_labels: ["a", "b"]
        }
      ],
      rules: [
        {
           name: "test rule",
           sources: ["A"],
           sinks: ["B"],
           code: 2001,
           message_format: "whatever"
        }
      ]
    }
  |}
    ~expected:(Failure "Multi sink `B` can't be used for a regular rule.");
  assert_fails
    {|
    {
      sources: [
        { name: "A" },
        { name: "B" }
      ],
      sinks: [
        {
          name: "C"
        }
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
  |}
    ~expected:(Failure "Error when parsing configuration: `C` is not a multi sink.");
  assert_fails
    {|
    {
      sources: [
        { name: "A" },
        { name: "B" }
      ],
      sinks: [
        {
          name: "C",
          multi_sink_labels: ["a", "b"]
        }
      ],
      combined_source_rules: [
        {
           name: "test combined rule",
           sources: {"a": "A", "invalid": "B"},
           sinks: ["C"],
           code: 2001,
           message_format: "some form"
        }
      ]
    }
  |}
    ~expected:
      (Failure
         "Error when parsing configuration: `invalid` is an invalid label For multi sink `C` \
          (choices: `a, b`)")


let test_empty _ =
  assert_raises (Yojson.Json_error "Blank input data") (fun () ->
      let _ = parse {| |} in
      ())


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
      sinks: [{ name: "C", multi_sink_labels: ["ca", "cb"] }],
      combined_source_rules: [
        {
           name: "c rule",
           sources: {"ca": "A", "cb": "B"},
           sinks: ["C"],
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
      sinks: [{ name: "C", multi_sink_labels: ["ca", "cb"] }],
      combined_source_rules: [
        {
           name: "c rule",
           sources: {"ca": "A", "cb": "B"},
           sinks: ["C"],
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
      sinks: [{ name: "C", multi_sink_labels: ["ca", "cb"] }],
      combined_source_rules: [
        {
           name: "c rule",
           sources: {"ca": "A", "cb": "B"},
           sinks: ["C"],
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
      sinks: [{ name: "C", multi_sink_labels: ["ca", "cb"] }],
      combined_source_rules: [
        {
           name: "c rule",
           sources: {"ca": "A", "cb": "B"},
           sinks: ["C"],
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
  assert_equal configuration.sources ["A"; "B"];
  assert_equal configuration.sinks ["D"; "C"];
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
    ~error:"Multiple values were passed in for overrides to analyze."
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
    ]


let () =
  "configuration"
  >::: [
         "combined_source_rules" >:: test_combined_source_rules;
         "combined_source_parse_errors" >:: test_combined_source_parse_errors;
         "empty" >:: test_empty;
         "invalid_sink" >:: test_invalid_sink;
         "invalid_source" >:: test_invalid_source;
         "simple" >:: test_simple;
         "partial_sink_converter" >:: test_partial_sink_converter;
         "multiple_configurations" >:: test_multiple_configurations;
         "validate" >:: test_validate;
       ]
  |> Test.run
