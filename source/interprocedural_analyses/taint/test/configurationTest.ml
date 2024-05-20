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
module Result = Core.Result

let parse ?rule_filter ?source_filter ?sink_filter ?transform_filter configuration =
  let open Result in
  let configuration =
    TaintConfiguration.from_json_list
      [
        ( PyrePath.create_absolute "/taint.config",
          JsonParsing.JsonAst.Json.from_string_exn configuration );
      ]
    >>= TaintConfiguration.with_command_line_options
          ~rule_filter
          ~source_filter
          ~sink_filter
          ~transform_filter
          ~find_missing_flows:None
          ~dump_model_query_results_path:None
          ~maximum_model_source_tree_width:None
          ~maximum_model_sink_tree_width:None
          ~maximum_model_tito_tree_width:None
          ~maximum_tree_depth_after_widening:None
          ~maximum_return_access_path_width:None
          ~maximum_return_access_path_depth_after_widening:None
          ~maximum_tito_collapse_depth:None
          ~maximum_tito_positions:None
          ~maximum_overrides_to_analyze:None
          ~maximum_trace_length:None
          ~maximum_tito_depth:None
    >>= TaintConfiguration.validate
  in
  (* Test that the configuration can be written in shared memory. *)
  let (_ : (TaintConfiguration.SharedMemory.t, TaintConfiguration.Error.t list) Result.t) =
    configuration >>| TaintConfiguration.SharedMemory.from_heap
  in
  configuration


let assert_parse ?rule_filter ?source_filter ?sink_filter ?transform_filter configuration =
  match parse ?rule_filter ?source_filter ?sink_filter ?transform_filter configuration with
  | Error errors ->
      let errors = List.map ~f:TaintConfiguration.Error.show errors |> String.concat ~sep:"\n" in
      Format.sprintf "Unexpected error when parsing configuration: %s" errors |> assert_failure
  | Ok configuration -> configuration


let assert_parse_error ~errors configuration =
  let configuration =
    parse configuration
    |> Core.Result.map_error ~f:(List.map ~f:(fun { TaintConfiguration.Error.kind; _ } -> kind))
  in
  let printer = function
    | Ok _ -> "Ok _"
    | Error errors ->
        List.map ~f:TaintConfiguration.Error.show_kind errors |> String.concat ~sep:"\n"
  in
  assert_equal ~printer (Error errors) configuration


let assert_rules_equal ~actual ~expected =
  assert_equal
    ~printer:(List.to_string ~f:Rule.show)
    ~cmp:(List.equal [%compare.equal: Rule.t])
    actual
    expected


let without_locations sources_or_sinks =
  List.map
    ~f:(fun source_or_sink -> { source_or_sink with AnnotationParser.location = None })
    sources_or_sinks


let named name = { AnnotationParser.name; kind = Named; location = None }

let named_transform name = TaintTransform.Named { name; location = None }

let test_simple _ =
  let configuration =
    assert_parse
      {|
    { "sources": [
        { "name": "A" },
        { "name": "B" }
      ],
      "sinks": [
        { "name": "C" },
        { "name": "D" }
      ],
      "features": [
        { "name": "E" },
        { "name": "F" }
      ],
      "rules": [
        {
           "name": "test rule",
           "sources": ["A"],
           "sinks": ["D"],
           "code": 2001,
           "message_format": "whatever"
        }
      ],
      "options": {
        "maximum_overrides_to_analyze": 50
      }
    }
  |}
  in
  assert_equal (without_locations configuration.sources) [named "A"; named "B"];
  assert_equal (without_locations configuration.sinks) [named "C"; named "D"];
  assert_equal configuration.features ["E"; "F"];
  assert_equal (List.length configuration.rules) 1;
  assert_equal (List.hd_exn configuration.rules).code 2001;
  assert_equal
    ~cmp:(Option.equal Int.equal)
    configuration.analysis_model_constraints.maximum_overrides_to_analyze
    (Some 50);
  assert_equal
    (Sources.Map.of_alist_exn [Sources.NamedSource "A", Sinks.Set.of_list [Sinks.NamedSink "D"]])
    (SourceSinkFilter.matching_sinks configuration.source_sink_filter)


let test_transform _ =
  let configuration =
    assert_parse
      {|
    { "sources": [],
      "sinks": [],
      "rules": [
        {
           "name": "test rule",
           "sources": [],
           "sinks": [],
           "code": 2001,
           "message_format": "transforms are optional"
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
    { "sources": [],
      "sinks": [],
      "transforms": [
        {"name": "A"}
      ],
      "rules": [
        {
           "name": "test rule",
           "sources": [],
           "sinks": [],
           "transforms": ["A", "A"],
           "code": 2001,
           "message_format": "transforms can repeat in rules"
        }
      ]
    }
  |}
  in
  assert_equal
    (List.hd_exn configuration.rules).transforms
    [
      TaintTransform.Named { name = "A"; location = None };
      TaintTransform.Named { name = "A"; location = None };
    ];
  ()


let test_transform_splits _ =
  assert_equal [[], []] (Rule.transform_splits []);
  assert_equal [["T1"], []; [], ["T1"]] (Rule.transform_splits ["T1"]);
  assert_equal
    [["T2"; "T1"], []; ["T1"], ["T2"]; [], ["T1"; "T2"]]
    (Rule.transform_splits ["T1"; "T2"]);
  assert_equal
    [["T3"; "T2"; "T1"], []; ["T2"; "T1"], ["T3"]; ["T1"], ["T2"; "T3"]; [], ["T1"; "T2"; "T3"]]
    (Rule.transform_splits ["T1"; "T2"; "T3"]);
  ()


let test_invalid_source _ =
  assert_parse_error
    ~errors:[TaintConfiguration.Error.UnsupportedSource "C"]
    {|
    { "sources": [
        { "name": "A" },
        { "name": "B" }
      ],
      "sinks": [
      ],
      "features": [
      ],
      "rules": [
        {
           "name": "test rule",
           "sources": ["C"],
           "sinks": [],
           "code": 2001,
           "message_format": "whatever"
        }
      ]
    }
    |}


let test_invalid_sink _ =
  assert_parse_error
    ~errors:[TaintConfiguration.Error.UnsupportedSink "B"]
    {|
    { "sources": [
      ],
      "sinks": [
        { "name": "A" }
      ],
      "features": [
      ],
      "rules": [
        {
           "name": "test rule",
           "sources": [],
           "sinks": ["B"],
           "code": 2001,
           "message_format": "whatever"
        }
      ]
    }
    |}


let test_invalid_transform _ =
  assert_parse_error
    ~errors:[TaintConfiguration.Error.UnsupportedTransform "B"]
    {|
    { "sources": [
      ],
      "sinks": [
        { "name": "A" }
      ],
      "transforms": [
        { "name": "A" }
      ],
      "features": [
      ],
      "rules": [
        {
           "name": "test rule",
           "sources": [],
           "sinks": [],
           "transforms": ["B"],
           "code": 2001,
           "message_format": "whatever"
        }
      ]
    }
    |}


let test_combined_source_rules _ =
  assert_parse_error
    ~errors:
      [
        TaintConfiguration.Error.UnexpectedJsonType
          {
            json =
              {
                JsonParsing.JsonAst.Node.value = `Null;
                location = JsonParsing.JsonAst.Location.null_location;
              };
            message = "Expected string, got null";
            section = Some "partial_sink";
          };
      ]
    {|
      { "sources": [
          { "name": "A" },
          { "name": "B" }
        ],
        "combined_source_rules": [
          {
             "name": "test combined rule",
             "sources": {"a": "A", "b": "B"},
             "sinks": ["C"],
             "code": 2001,
             "message_format": "some form"
          }
        ]
      }
    |};
  assert_parse_error
    ~errors:
      [
        TaintConfiguration.Error.UnexpectedJsonType
          {
            json =
              {
                JsonParsing.JsonAst.Node.value = `Float 123.;
                location =
                  {
                    JsonParsing.JsonAst.Location.start = { line = 9; column = 31 };
                    stop = { line = 9; column = 33 };
                  };
              };
            message = "Expected a string or an array of strings";
            section = Some "a";
          };
      ]
    {|
      { "sources": [
          { "name": "A" },
          { "name": "B" }
        ],
        "combined_source_rules": [
          {
             "name": "test combined rule",
             "sources": {"a": 123, "b": "B"},
             "sinks": ["C"],
             "partial_sink": "CombinedSink",
             "code": 2001,
             "message_format": "some form"
          }
        ]
      }
    |};
  let configuration =
    assert_parse
      {|
    { "sources": [
        { "name": "A" },
        { "name": "B" }
      ],
      "combined_source_rules": [
        {
           "name": "test combined rule",
           "sources": {"a": "A", "b": "B"},
           "partial_sink": "C",
           "main_trace_source": "b",
           "code": 2001,
           "message_format": "some form"
        }
      ]
    }
  |}
  in
  assert_equal (without_locations configuration.sources) [named "A"; named "B"];
  assert_equal configuration.sinks [];
  assert_rules_equal
    ~actual:configuration.rules
    ~expected:
      [
        {
          Rule.sources = [Sources.NamedSource "A"];
          sinks = [Sinks.TriggeredPartialSink { kind = "C"; label = "a" }];
          transforms = [];
          code = 2001;
          message_format = "some form";
          filters = None;
          location =
            Some
              {
                JsonParsing.JsonAst.LocationWithPath.path = PyrePath.create_absolute "/taint.config";
                location =
                  {
                    JsonParsing.JsonAst.Location.start = { line = 12; column = 20 };
                    stop = { line = 12; column = 23 };
                  };
              };
          name = "test combined rule";
        };
        {
          Rule.sources = [Sources.NamedSource "B"];
          sinks = [Sinks.TriggeredPartialSink { kind = "C"; label = "b" }];
          transforms = [];
          code = 2001;
          message_format = "some form";
          filters = None;
          location =
            Some
              {
                JsonParsing.JsonAst.LocationWithPath.path = PyrePath.create_absolute "/taint.config";
                location =
                  {
                    JsonParsing.JsonAst.Location.start = { line = 12; column = 20 };
                    stop = { line = 12; column = 23 };
                  };
              };
          name = "test combined rule";
        };
      ];
  assert_equal (List.hd_exn configuration.rules).code 2001;
  assert_equal
    ["C", { TaintConfiguration.PartialSinkLabelsMap.main = "b"; secondary = "a" }]
    (TaintConfiguration.PartialSinkLabelsMap.to_alist configuration.partial_sink_labels);
  let configuration =
    assert_parse
      {|
    { "sources": [
        { "name": "A" },
        { "name": "B" },
        { "name": "C" }
      ],
      "sinks": [],
      "combined_source_rules": [
        {
           "name": "test combined rule",
           "sources": {"a": "A", "b": ["B", "C"]},
           "partial_sink": "CombinedSink",
           "code": 2001,
           "message_format": "some form"
        }
      ]
    }
  |}
  in
  assert_equal (without_locations configuration.sources) [named "A"; named "B"; named "C"];
  assert_equal configuration.sinks [];
  assert_equal
    (TaintConfiguration.PartialSinkLabelsMap.to_alist configuration.partial_sink_labels)
    ["CombinedSink", { TaintConfiguration.PartialSinkLabelsMap.main = "a"; secondary = "b" }];
  assert_rules_equal
    ~actual:configuration.rules
    ~expected:
      [
        {
          Rule.sources = [Sources.NamedSource "B"; Sources.NamedSource "C"];
          sinks = [Sinks.TriggeredPartialSink { kind = "CombinedSink"; label = "b" }];
          transforms = [];
          code = 2001;
          message_format = "some form";
          filters = None;
          location =
            Some
              {
                JsonParsing.JsonAst.LocationWithPath.path = PyrePath.create_absolute "/taint.config";
                location =
                  {
                    JsonParsing.JsonAst.Location.start = { line = 13; column = 20 };
                    stop = { line = 13; column = 23 };
                  };
              };
          name = "test combined rule";
        };
        {
          Rule.sources = [Sources.NamedSource "A"];
          sinks = [Sinks.TriggeredPartialSink { kind = "CombinedSink"; label = "a" }];
          transforms = [];
          code = 2001;
          message_format = "some form";
          filters = None;
          location =
            Some
              {
                JsonParsing.JsonAst.LocationWithPath.path = PyrePath.create_absolute "/taint.config";
                location =
                  {
                    JsonParsing.JsonAst.Location.start = { line = 13; column = 20 };
                    stop = { line = 13; column = 23 };
                  };
              };
          name = "test combined rule";
        };
      ];
  assert_equal (List.hd_exn configuration.rules).code 2001;
  assert_parse_error
    ~errors:[TaintConfiguration.Error.PartialSinkDuplicate "C"]
    {|
      { "sources": [
          { "name": "A" },
          { "name": "B" }
        ],
        "combined_source_rules": [
          {
             "name": "test combined rule",
             "sources": {"a": "A", "b": "B"},
             "partial_sink": "C",
             "code": 2001,
             "message_format": "some form"
          },
          {
             "name": "test combined rule",
             "sources": {"a": "A", "b": "B"},
             "partial_sink": "C",
             "code": 2002,
             "message_format": "other form"
          }
        ]
      }
    |}


let test_string_combine_rules _ =
  (* `main_sources` must exist *)
  assert_parse_error
    ~errors:
      [
        TaintConfiguration.Error.UnexpectedJsonType
          {
            json =
              {
                JsonParsing.JsonAst.Node.value = `Null;
                location = JsonParsing.JsonAst.Location.null_location;
              };
            message = "Expected a string or an array of strings";
            section = Some "main_sources";
          };
      ]
    {|
      { "sources": [],
        "string_combine_rules": [
          {
             "name": "",
             "code": 2001,
             "message_format": ""
          }
        ]
      }
    |};
  (* `secondary_sources` must exist *)
  assert_parse_error
    ~errors:
      [
        TaintConfiguration.Error.UnexpectedJsonType
          {
            json =
              {
                JsonParsing.JsonAst.Node.value = `Null;
                location = JsonParsing.JsonAst.Location.null_location;
              };
            message = "Expected a string or an array of strings";
            section = Some "secondary_sources";
          };
      ]
    {|
      { "sources": [
          { "name": "A" }
        ],
        "string_combine_rules": [
          {
             "name": "",
             "main_sources": "A",
             "code": 2001,
             "message_format": ""
          }
        ]
      }
    |};
  let configuration =
    assert_parse
      {|
    { "sources": [
        { "name": "A" },
        { "name":"B" },
        { "name":"C" }
      ],
      "sinks": [],
      "string_combine_rules": [
        {
           "name": "rule name",
           "main_sources": "A",
           "secondary_sources": ["B", "C"],
           "partial_sink": "UserDefinedPartialSink",
           "code": 2001,
           "message_format": "rule message"
        }
      ]
    }
  |}
  in
  assert_equal (without_locations configuration.sources) [named "A"; named "B"; named "C"];
  assert_equal
    [
      ( "UserDefinedPartialSink",
        { TaintConfiguration.PartialSinkLabelsMap.main = "main"; secondary = "secondary" } );
    ]
    (TaintConfiguration.PartialSinkLabelsMap.to_alist configuration.partial_sink_labels);
  assert_rules_equal
    ~actual:configuration.rules
    ~expected:
      [
        {
          Rule.sources = [Sources.NamedSource "A"];
          sinks = [Sinks.TriggeredPartialSink { kind = "UserDefinedPartialSink"; label = "main" }];
          transforms = [];
          code = 2001;
          message_format = "rule message";
          filters = None;
          location =
            Some
              {
                JsonParsing.JsonAst.LocationWithPath.path = PyrePath.create_absolute "/taint.config";
                location =
                  {
                    JsonParsing.JsonAst.Location.start = { line = 14; column = 20 };
                    stop = { line = 14; column = 23 };
                  };
              };
          name = "rule name";
        };
        {
          Rule.sources = [Sources.NamedSource "B"; Sources.NamedSource "C"];
          sinks =
            [Sinks.TriggeredPartialSink { kind = "UserDefinedPartialSink"; label = "secondary" }];
          transforms = [];
          code = 2001;
          message_format = "rule message";
          filters = None;
          location =
            Some
              {
                JsonParsing.JsonAst.LocationWithPath.path = PyrePath.create_absolute "/taint.config";
                location =
                  {
                    JsonParsing.JsonAst.Location.start = { line = 14; column = 20 };
                    stop = { line = 14; column = 23 };
                  };
              };
          name = "rule name";
        };
      ];
  assert_equal
    ~cmp:TaintConfiguration.StringOperationPartialSinks.equal
    configuration.string_combine_partial_sinks
    (TaintConfiguration.StringOperationPartialSinks.singleton "UserDefinedPartialSink")


let test_partial_sink_converter _ =
  let assert_triggered_sinks configuration ~partial_sink ~source ~expected_sink =
    let configuration = assert_parse configuration in
    TaintConfiguration.get_triggered_sink configuration ~partial_sink ~source
    |> assert_equal
         ~cmp:(Option.equal Sinks.equal)
         ~printer:(fun value -> value >>| Sinks.show |> Option.value ~default:"None")
         expected_sink
  in
  let configuration =
    {|
    {
      "sources": [{ "name": "A" }, { "name": "B" }],
      "sinks": [],
      "combined_source_rules": [
        {
           "name": "c rule",
           "sources": {"ca": "A", "cb": "B"},
           "partial_sink": "C",
           "code": 2001,
           "message_format": "some form"
        }
      ]
    }
  |}
  in
  assert_triggered_sinks
    configuration
    ~partial_sink:{ Sinks.kind = "C"; label = "ca" }
    ~source:(Sources.NamedSource "A")
    ~expected_sink:(Some (Sinks.TriggeredPartialSink { Sinks.kind = "C"; label = "cb" }));
  assert_triggered_sinks
    configuration
    ~partial_sink:{ Sinks.kind = "C"; label = "cb" }
    ~source:(Sources.NamedSource "B")
    ~expected_sink:(Some (Sinks.TriggeredPartialSink { Sinks.kind = "C"; label = "ca" }));
  assert_triggered_sinks
    configuration
    ~partial_sink:{ Sinks.kind = "C"; label = "ca" }
    ~source:(Sources.NamedSource "B")
    ~expected_sink:None;
  assert_triggered_sinks
    configuration
    ~partial_sink:{ Sinks.kind = "C"; label = "cb" }
    ~source:(Sources.NamedSource "A")
    ~expected_sink:None


let test_multiple_configurations _ =
  let configuration =
    TaintConfiguration.from_json_list
      [
        ( PyrePath.create_absolute "/a.config",
          JsonParsing.JsonAst.Json.from_string_exn
            {|
              { "sources": [
                  { "name": "A" },
                  { "name": "B" }
                ],
                "sinks": [
                  { "name": "C" },
                  { "name": "D" }
                ],
                "features": [
                  { "name": "E" },
                  { "name": "F" }
                ],
                "rules": [],
                "options": {
                  "maximum_overrides_to_analyze": 42
                }
               }
           |}
        );
        ( PyrePath.create_absolute "/b.config",
          JsonParsing.JsonAst.Json.from_string_exn
            {|
              {
                "sources": [],
                "sinks": [],
                "features": [],
                "rules": [
                  {
                     "name": "test rule",
                     "sources": ["A"],
                     "sinks": ["D"],
                     "code": 2001,
                     "message_format": "whatever"
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
  assert_equal (without_locations configuration.sources) [named "A"; named "B"];
  assert_equal (without_locations configuration.sinks) [named "C"; named "D"];
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
             PyrePath.create_absolute path, JsonParsing.JsonAst.Json.from_string_exn content)
      |> TaintConfiguration.from_json_list
      |> Core.Result.map_error
           ~f:
             (List.map ~f:(fun { TaintConfiguration.Error.path; kind; _ } ->
                  path >>| PyrePath.absolute, kind))
    in
    let printer = function
      | Ok _ -> "Ok _"
      | Error errors ->
          List.map ~f:[%show: string option * TaintConfiguration.Error.kind] errors
          |> String.concat ~sep:"\n"
    in
    assert_equal ~printer (Error [error]) result
  in
  assert_parse_error
    ~errors:
      [
        TaintConfiguration.Error.SourceDuplicate
          {
            name = "UserControlled";
            previous_location =
              Some
                {
                  JsonParsing.JsonAst.LocationWithPath.path =
                    PyrePath.create_absolute "/taint.config";
                  location =
                    {
                      JsonParsing.JsonAst.Location.start = { line = 4; column = 18 };
                      stop = { line = 4; column = 33 };
                    };
                };
          };
      ]
    {|
    {
      "sources": [
       { "name": "UserControlled" },
       { "name": "UserControlled" }
      ]
    }
    |};
  assert_parse_error
    ~errors:
      [
        TaintConfiguration.Error.SinkDuplicate
          {
            name = "Test";
            previous_location =
              Some
                {
                  JsonParsing.JsonAst.LocationWithPath.path =
                    PyrePath.create_absolute "/taint.config";
                  location =
                    {
                      JsonParsing.JsonAst.Location.start = { line = 6; column = 18 };
                      stop = { line = 6; column = 23 };
                    };
                };
          };
      ]
    {|
    {
      "sources": [
      ],
      "sinks": [
       { "name": "Test" },
       { "name": "Test" }
      ]
    }
    |};
  assert_parse_error
    ~errors:
      [
        TaintConfiguration.Error.TransformDuplicate
          {
            name = "Test";
            previous_location =
              Some
                {
                  JsonParsing.JsonAst.LocationWithPath.path =
                    PyrePath.create_absolute "/taint.config";
                  location =
                    {
                      JsonParsing.JsonAst.Location.start = { line = 8; column = 19 };
                      stop = { line = 8; column = 24 };
                    };
                };
          };
      ]
    {|
    {
      "sources": [
      ],
      "sinks": [
      ],
      "transforms": [
        { "name": "Test" },
        { "name": "Test" }
      ]
    }
    |};
  assert_parse_error
    ~errors:[TaintConfiguration.Error.FeatureDuplicate "concat"]
    {|
    {
      "sources": [
       { "name": "UserControlled" }
      ],
      "sinks": [
       { "name": "Test" }
      ],
      "features": [
       { "name": "concat" },
       { "name": "concat" }
      ]
    }
    |};
  assert_parse_error
    ~errors:
      [
        TaintConfiguration.Error.SourceDuplicate
          {
            name = "UserControlled";
            previous_location =
              Some
                {
                  JsonParsing.JsonAst.LocationWithPath.path =
                    PyrePath.create_absolute "/taint.config";
                  location =
                    {
                      JsonParsing.JsonAst.Location.start = { line = 4; column = 18 };
                      stop = { line = 4; column = 33 };
                    };
                };
          };
        TaintConfiguration.Error.SinkDuplicate
          {
            name = "Test";
            previous_location =
              Some
                {
                  JsonParsing.JsonAst.LocationWithPath.path =
                    PyrePath.create_absolute "/taint.config";
                  location =
                    {
                      JsonParsing.JsonAst.Location.start = { line = 8; column = 18 };
                      stop = { line = 8; column = 23 };
                    };
                };
          };
        TaintConfiguration.Error.FeatureDuplicate "concat";
      ]
    {|
    {
      "sources": [
       { "name": "UserControlled" },
       { "name": "UserControlled" }
      ],
      "sinks": [
       { "name": "Test" },
       { "name": "Test" }
      ],
      "features": [
       { "name": "concat" },
       { "name": "concat" }
      ]
    }
    |};
  assert_parse_error
    ~errors:
      [
        TaintConfiguration.Error.SourceDuplicate
          {
            name = "UserControlled";
            previous_location =
              Some
                {
                  JsonParsing.JsonAst.LocationWithPath.path =
                    PyrePath.create_absolute "/taint.config";
                  location =
                    {
                      JsonParsing.JsonAst.Location.start = { line = 5; column = 18 };
                      stop = { line = 5; column = 33 };
                    };
                };
          };
      ]
    {|
    {
      "sources": [
       {
         "name": "UserControlled",
         "comment": "First copy of user controlled"
       },
       {
         "name": "UserControlled",
         "comment": "Another copy of user controlled"
       }
      ]
    }
    |};
  assert_parse_error
    ~errors:
      [
        TaintConfiguration.Error.SinkDuplicate
          {
            name = "Test";
            previous_location =
              Some
                {
                  JsonParsing.JsonAst.LocationWithPath.path =
                    PyrePath.create_absolute "/taint.config";
                  location =
                    {
                      JsonParsing.JsonAst.Location.start = { line = 5; column = 18 };
                      stop = { line = 5; column = 23 };
                    };
                };
          };
      ]
    {|
    {
      "sinks": [
       {
         "name": "Test"
       },
       {
         "name": "Test",
         "kind": "parametric"
       }
      ]
    }
    |};
  assert_parse_error
    ~errors:
      [
        TaintConfiguration.Error.RuleCodeDuplicate
          {
            code = 2001;
            previous_location =
              Some
                {
                  JsonParsing.JsonAst.LocationWithPath.path =
                    PyrePath.create_absolute "/taint.config";
                  location =
                    {
                      JsonParsing.JsonAst.Location.start = { line = 22; column = 20 };
                      stop = { line = 22; column = 23 };
                    };
                };
          };
      ]
    {|
    {
      "sources": [
        { "name": "A" }
      ],
      "sinks": [
       { "name": "B" },
       { "name": "C" }
      ],
      "rules": [
        {
           "name": "test rule",
           "sources": ["A"],
           "sinks": ["B"],
           "code": 2001,
           "message_format": "whatever"
        },
        {
           "name": "test rule",
           "sources": ["A"],
           "sinks": ["C"],
           "code": 2001,
           "message_format": "format"
        }
      ]
    }
    |};
  assert_parse_error
    ~errors:
      [
        TaintConfiguration.Error.RuleCodeDuplicate
          {
            code = 2002;
            previous_location =
              Some
                {
                  JsonParsing.JsonAst.LocationWithPath.path =
                    PyrePath.create_absolute "/taint.config";
                  location =
                    {
                      JsonParsing.JsonAst.Location.start = { line = 16; column = 20 };
                      stop = { line = 16; column = 23 };
                    };
                };
          };
      ]
    {|
    {
      "sources": [
        { "name": "A" },
        { "name": "B" }
      ],
      "sinks": [
        { "name": "C" },
        { "name": "D" }
      ],
      "rules": [
        {
           "name": "test rule",
           "sources": ["A"],
           "sinks": ["D"],
           "code": 2002,
           "message_format": "whatever"
        }
      ],
      "combined_source_rules": [
        {
           "name": "test combined rule",
           "sources": {"a": "A", "b": "B"},
           "partial_sink": "D",
           "code": 2002,
           "message_format": "some form"
        }
      ]
    }
    |};
  assert_parse_multiple_error
    ~error:(None, TaintConfiguration.Error.OptionDuplicate "maximum_overrides_to_analyze")
    [
      ( "/a.config",
        {|
          {
            "sources": [],
            "sinks": [],
            "features": [],
            "rules": [],
            "options": {
              "maximum_overrides_to_analyze": 50
            }
          }
        |}
      );
      ( "/b.config",
        {|
          {
            "sources": [],
            "sinks": [],
            "features": [],
            "rules": [],
            "options": {
              "maximum_overrides_to_analyze": 60
            }
          }
        |}
      );
    ];
  assert_parse_multiple_error
    ~error:(None, TaintConfiguration.Error.OptionDuplicate "maximum_trace_length")
    [
      ( "/a.config",
        {|
          {
            "sources": [],
            "sinks": [],
            "features": [],
            "rules": [],
            "options": {
              "maximum_trace_length": 10
            }
          }
        |}
      );
      ( "/b.config",
        {|
          {
            "sources": [],
            "sinks": [],
            "features": [],
            "rules": [],
            "options": {
              "maximum_trace_length": 20
            }
          }
        |}
      );
    ];
  assert_parse_multiple_error
    ~error:(None, TaintConfiguration.Error.OptionDuplicate "maximum_tito_depth")
    [
      ( "/a.config",
        {|
          {
            "sources": [],
            "sinks": [],
            "features": [],
            "rules": [],
            "options": {
              "maximum_tito_depth": 10
            }
          }
        |}
      );
      ( "/b.config",
        {|
          {
            "sources": [],
            "sinks": [],
            "features": [],
            "rules": [],
            "options": {
              "maximum_tito_depth": 20
            }
          }
        |}
      );
    ];
  assert_parse_error
    ~errors:[TaintConfiguration.Error.UnsupportedSource "MisspelledStringDigit"]
    {|
        {
          "sources": [{ "name": "StringDigit" }],
          "sinks": [],
          "features": [],
          "rules": [],
          "implicit_sources": {
            "literal_strings": [
              {
                "regexp": "^\\d+$",
                "kind": "MisspelledStringDigit"
              }
            ]
          }
        }
    |};
  assert_parse_error
    ~errors:[TaintConfiguration.Error.UnsupportedSink "Misspelled"]
    {|
          {
            "sources": [],
            "sinks": [{ "name": "Test" }],
            "features": [],
            "rules": [],
            "implicit_sinks": {
              "literal_strings": [
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
          { "sources": [{ "name": "StringDigit" }],
            "sinks": [{ "name": "RCE" }],
            "features": [],
            "rules": [
              {
                 "name": "test rule",
                 "sources": ["StringDigit"],
                 "sinks": ["RCE"],
                 "code": 1,
                 "message_format": ""
              }
            ],
            "implicit_sources": {
              "literal_strings": [
                {
                  "regexp": "^\\d+$",
                  "kind": "StringDigit"
                }
              ]
            }
           }
       |}
  in
  assert_equal (without_locations configuration.sources) [named "StringDigit"];
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
          { "sources": [{ "name": "UserControlled" }],
            "sinks": [{ "name": "HTMLContainer" }],
            "features": [],
            "rules": [
              {
                 "name": "test rule",
                 "sources": ["UserControlled"],
                 "sinks": ["HTMLContainer"],
                 "code": 1,
                 "message_format": ""
              }
            ],
            "implicit_sinks": {
              "literal_strings": [
                {
                  "regexp": "<.*>",
                  "kind": "HTMLContainer"
                }
              ]
            }
           }
     |}
  in
  assert_equal (without_locations configuration.sinks) [named "HTMLContainer"];
  match configuration.implicit_sinks with
  | { TaintConfiguration.literal_string_sinks = [{ pattern; sink_kind }]; _ } ->
      assert_equal ~cmp:Sinks.equal sink_kind (Sinks.NamedSink "HTMLContainer");
      assert_equal
        ~cmp:(fun left right -> Re2.compare left right = 0)
        pattern
        (Re2.create_exn "<.*>")
  | _ -> Test.assert_unreached ()


let test_matching_kinds _ =
  let assert_matching
      ?rule_filter
      ?source_filter
      ?sink_filter
      ?transform_filter
      ~configuration
      ~matching_sources
      ~matching_sinks
      ~possible_tito_transforms
      ()
    =
    let configuration =
      assert_parse ?rule_filter ?source_filter ?sink_filter ?transform_filter configuration
    in
    let matching_sources_printer matching =
      matching
      |> Sources.Map.to_alist
      |> List.map ~f:(fun (source, sinks) ->
             Format.asprintf "%a -> %a" Sources.pp source Sinks.Set.pp sinks)
      |> String.concat ~sep:", "
      |> Format.asprintf "{%s}"
    in
    let source_sink_filter = configuration.source_sink_filter in
    assert_equal
      ~printer:matching_sources_printer
      ~cmp:(Sources.Map.equal Sinks.Set.equal)
      (Sources.Map.of_alist_exn matching_sinks)
      (SourceSinkFilter.matching_sinks source_sink_filter);
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
      (SourceSinkFilter.matching_sources source_sink_filter);
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
      (SourceSinkFilter.possible_tito_transforms source_sink_filter)
  in
  assert_matching
    ~configuration:
      {|
        { "sources": [
            { "name": "A" },
            { "name": "B" }
          ],
          "sinks": [
            { "name": "C" },
            { "name": "D" }
          ],
          "rules": [
            {
               "name": "test rule",
               "sources": ["A"],
               "sinks": ["C", "D"],
               "code": 1,
               "message_format": ""
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
    ~possible_tito_transforms:[TaintTransforms.empty]
    ();
  assert_matching
    ~configuration:
      {|
        { "sources": [
            { "name": "A" },
            { "name": "B" }
          ],
          "sinks": [
            { "name": "C" },
            { "name": "D" }
          ],
          "rules": [
            {
               "name": "test rule",
               "sources": ["A", "B"],
               "sinks": ["D"],
               "code": 1,
               "message_format": ""
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
    ~possible_tito_transforms:[TaintTransforms.empty]
    ();
  assert_matching
    ~configuration:
      {|
        { "sources": [
            { "name": "A" },
            { "name": "B" }
          ],
          "sinks": [
            { "name": "C" },
            { "name": "D" }
          ],
          "rules": [
            {
               "name": "test rule",
               "sources": ["A"],
               "sinks": ["C"],
               "code": 1,
               "message_format": ""
            },
            {
               "name": "test rule 2",
               "sources": ["A"],
               "sinks": ["D"],
               "code": 2,
               "message_format": ""
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
    ~possible_tito_transforms:[TaintTransforms.empty]
    ();
  let transformed_source transform_names source_name =
    Sources.Transform
      {
        base = Sources.NamedSource source_name;
        global =
          List.map transform_names ~f:(fun name -> named_transform name)
          |> TaintTransforms.of_named_transforms;
        local = TaintTransforms.empty;
      }
  in
  let transformed_sink transform_names sink_name =
    Sinks.Transform
      {
        base = Sinks.NamedSink sink_name;
        global =
          List.map transform_names ~f:(fun name -> named_transform name)
          |> TaintTransforms.of_named_transforms;
        local = TaintTransforms.empty;
      }
  in
  assert_matching
    ~configuration:
      {|
        { "sources": [
            { "name": "A" },
            { "name": "B" }
          ],
          "sinks": [
            { "name": "C" },
            { "name": "D" }
          ],
          "transforms": [
            { "name": "X" },
            { "name": "Y" }
          ],
          "rules": [
            {
               "name": "test rule",
               "sources": ["A"],
               "transforms": ["X", "Y"],
               "sinks": ["C", "D"],
               "code": 1,
               "message_format": ""
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
        TaintTransforms.of_named_transforms [named_transform "X"];
        TaintTransforms.of_named_transforms [named_transform "X"; named_transform "Y"];
        TaintTransforms.of_named_transforms [named_transform "Y"];
      ]
    ();
  assert_matching
    ~rule_filter:[1]
    ~configuration:
      {|
        { "sources": [
            { "name": "A" },
            { "name": "B" }
          ],
          "sinks": [
            { "name": "C" },
            { "name": "D" }
          ],
          "rules": [
            {
               "name": "test rule",
               "sources": ["A"],
               "sinks": ["C"],
               "code": 1,
               "message_format": ""
            },
            {
               "name": "test rule 2",
               "sources": ["A"],
               "sinks": ["D"],
               "code": 2,
               "message_format": ""
            }
          ]
        }
      |}
    ~matching_sinks:[Sources.NamedSource "A", Sinks.Set.of_list [Sinks.NamedSink "C"]]
    ~matching_sources:[Sinks.NamedSink "C", Sources.Set.of_list [Sources.NamedSource "A"]]
    ~possible_tito_transforms:[TaintTransforms.empty]
    ();
  assert_matching
    ~source_filter:["A"; "B"]
    ~configuration:
      {|
        { "sources": [
            { "name": "A" },
            { "name": "B", "kind": "parametric" },
            { "name": "C" },
            { "name": "D" }
          ],
          "sinks": [
            { "name": "X" },
            { "name": "Y" },
            { "name": "Z" }
          ],
          "rules": [
            {
               "name": "test rule",
               "sources": ["A"],
               "sinks": ["X"],
               "code": 1,
               "message_format": ""
            },
            {
               "name": "test rule 2",
               "sources": ["B", "C"],
               "sinks": ["Y"],
               "code": 2,
               "message_format": ""
            },
            {
               "name": "test rule 2",
               "sources": ["C", "D"],
               "sinks": ["Z"],
               "code": 3,
               "message_format": ""
            }
          ]
        }
      |}
    ~matching_sinks:
      [
        Sources.NamedSource "A", Sinks.Set.of_list [Sinks.NamedSink "X"];
        Sources.NamedSource "B", Sinks.Set.of_list [Sinks.NamedSink "Y"];
      ]
    ~matching_sources:
      [
        Sinks.NamedSink "X", Sources.Set.of_list [Sources.NamedSource "A"];
        Sinks.NamedSink "Y", Sources.Set.of_list [Sources.NamedSource "B"];
      ]
    ~possible_tito_transforms:[TaintTransforms.empty]
    ();
  assert_matching
    ~source_filter:["A"; "B"]
    ~configuration:
      {|
        { "sources": [
            { "name": "A" },
            { "name": "B" },
            { "name": "C" },
            { "name": "D" }
          ],
          "sinks": [
            { "name": "X" },
            { "name": "Y" },
            { "name": "Z" }
          ],
          "combined_source_rules": [
            {
              "name": "test combined rule",
              "sources": { "a": "A", "b": "B" },
              "partial_sink": "PartialSink1",
              "code": 1,
              "message_format": ""
            },
            {
              "name": "test combined rule 2",
              "sources": { "c": "C", "d": "D" },
              "partial_sink": "PartialSink2",
              "code": 2,
              "message_format": ""
            },
            {
              "name": "test combined rule 3",
              "sources": { "a": "A", "d": "D" },
              "partial_sink": "PartialSink3",
              "code": 3,
              "message_format": ""
            }
          ]
        }
      |}
    ~matching_sinks:
      [
        ( Sources.NamedSource "A",
          Sinks.Set.of_list
            [
              Sinks.TriggeredPartialSink { kind = "PartialSink1"; label = "a" };
              Sinks.TriggeredPartialSink { kind = "PartialSink3"; label = "a" };
            ] );
        ( Sources.NamedSource "B",
          Sinks.Set.of_list [Sinks.TriggeredPartialSink { kind = "PartialSink1"; label = "b" }] );
      ]
    ~matching_sources:
      [
        ( Sinks.TriggeredPartialSink { kind = "PartialSink1"; label = "a" },
          Sources.Set.of_list [Sources.NamedSource "A"] );
        ( Sinks.TriggeredPartialSink { kind = "PartialSink1"; label = "b" },
          Sources.Set.of_list [Sources.NamedSource "B"] );
        ( Sinks.TriggeredPartialSink { kind = "PartialSink3"; label = "a" },
          Sources.Set.of_list [Sources.NamedSource "A"] );
      ]
    ~possible_tito_transforms:[TaintTransforms.empty]
    ();
  assert_matching
    ~sink_filter:["X"; "Y"]
    ~configuration:
      {|
        { "sources": [
            { "name": "A" },
            { "name": "B", "kind": "parametric" },
            { "name": "C" },
            { "name": "D" }
          ],
          "sinks": [
            { "name": "X" },
            { "name": "Y", "kind": "parametric" },
            { "name": "Z" }
          ],
          "rules": [
            {
               "name": "test rule",
               "sources": ["A"],
               "sinks": ["X"],
               "code": 1,
               "message_format": ""
            },
            {
               "name": "test rule 2",
               "sources": ["B"],
               "sinks": ["Y", "Z"],
               "code": 2,
               "message_format": ""
            },
            {
               "name": "test rule 2",
               "sources": ["C"],
               "sinks": ["Z"],
               "code": 3,
               "message_format": ""
            }
          ]
        }
      |}
    ~matching_sinks:
      [
        Sources.NamedSource "A", Sinks.Set.of_list [Sinks.NamedSink "X"];
        Sources.NamedSource "B", Sinks.Set.of_list [Sinks.NamedSink "Y"];
      ]
    ~matching_sources:
      [
        Sinks.NamedSink "X", Sources.Set.of_list [Sources.NamedSource "A"];
        Sinks.NamedSink "Y", Sources.Set.of_list [Sources.NamedSource "B"];
      ]
    ~possible_tito_transforms:[TaintTransforms.empty]
    ();
  assert_matching
    ~transform_filter:["TU"]
    ~configuration:
      {|
        { "sources": [
            { "name": "A" },
            { "name": "B", "kind": "parametric" },
            { "name": "C" },
            { "name": "D" }
          ],
          "sinks": [
            { "name": "X" },
            { "name": "Y", "kind": "parametric" },
            { "name": "Z" }
          ],
          "transforms": [
            { "name": "TU" },
            { "name": "TV" },
            { "name": "TW" }
          ],
          "rules": [
            {
               "name": "test rule",
               "sources": ["A"],
               "sinks": ["X"],
               "code": 1,
               "message_format": ""
            },
            {
               "name": "test rule 2",
               "sources": ["B"],
               "transforms": ["TU"],
               "sinks": ["Y"],
               "code": 2,
               "message_format": ""
            },
            {
               "name": "test rule 3",
               "sources": ["C"],
               "transforms": ["TV"],
               "sinks": ["Z"],
               "code": 3,
               "message_format": ""
            },
            {
               "name": "test rule 4",
               "sources": ["C"],
               "transforms": ["TU", "TW"],
               "sinks": ["Z"],
               "code": 4,
               "message_format": ""
            }
          ]
        }
      |}
    ~matching_sinks:
      [
        Sources.NamedSource "A", Sinks.Set.of_list [Sinks.NamedSink "X"];
        ( Sources.NamedSource "B",
          Sinks.Set.of_list
            [
              Sinks.Transform
                {
                  local = TaintTransforms.empty;
                  global = TaintTransforms.of_named_transforms [named_transform "TU"];
                  base = Sinks.NamedSink "Y";
                };
            ] );
        ( Sources.Transform
            {
              local = TaintTransforms.empty;
              global = TaintTransforms.of_named_transforms [named_transform "TU"];
              base = Sources.NamedSource "B";
            },
          Sinks.Set.of_list [Sinks.NamedSink "Y"] );
      ]
    ~matching_sources:
      [
        Sinks.NamedSink "X", Sources.Set.of_list [Sources.NamedSource "A"];
        ( Sinks.NamedSink "Y",
          Sources.Set.of_list
            [
              Sources.Transform
                {
                  local = TaintTransforms.empty;
                  global = TaintTransforms.of_named_transforms [named_transform "TU"];
                  base = Sources.NamedSource "B";
                };
            ] );
        ( Sinks.Transform
            {
              local = TaintTransforms.empty;
              global = TaintTransforms.of_named_transforms [named_transform "TU"];
              base = Sinks.NamedSink "Y";
            },
          Sources.Set.of_list [Sources.NamedSource "B"] );
      ]
    ~possible_tito_transforms:
      [TaintTransforms.empty; TaintTransforms.of_named_transforms [named_transform "TU"]]
    ();
  assert_matching
    ~rule_filter:[1; 2; 4]
    ~source_filter:["A"; "B"; "C"]
    ~sink_filter:["X"; "Z"]
    ~transform_filter:["TU"; "TW"]
    ~configuration:
      {|
        { "sources": [
            { "name": "A" },
            { "name": "B", "kind": "parametric" },
            { "name": "C" },
            { "name": "D" }
          ],
          "sinks": [
            { "name": "X" },
            { "name": "Y", "kind": "parametric" },
            { "name": "Z" }
          ],
          "transforms": [
            { "name": "TU" },
            { "name": "TV" },
            { "name": "TW" }
          ],
          "rules": [
            {
               "name": "test rule",
               "sources": ["A", "D"],
               "sinks": ["X", "Y"],
               "code": 1,
               "message_format": ""
            },
            {
               "name": "test rule 2",
               "sources": ["B"],
               "transforms": ["TU"],
               "sinks": ["Y"],
               "code": 2,
               "message_format": ""
            },
            {
               "name": "test rule 3",
               "sources": ["C"],
               "transforms": ["TV"],
               "sinks": ["Z"],
               "code": 3,
               "message_format": ""
            },
            {
               "name": "test rule 4",
               "sources": ["C"],
               "transforms": ["TU", "TW"],
               "sinks": ["Z"],
               "code": 4,
               "message_format": ""
            }
          ]
        }
      |}
    ~matching_sinks:
      [
        Sources.NamedSource "A", Sinks.Set.of_list [Sinks.NamedSink "X"];
        ( Sources.NamedSource "C",
          Sinks.Set.of_list
            [
              Sinks.Transform
                {
                  local = TaintTransforms.empty;
                  global =
                    TaintTransforms.of_named_transforms [named_transform "TU"; named_transform "TW"];
                  base = Sinks.NamedSink "Z";
                };
            ] );
        ( Sources.Transform
            {
              local = TaintTransforms.empty;
              global = TaintTransforms.of_named_transforms [named_transform "TU"];
              base = Sources.NamedSource "C";
            },
          Sinks.Set.of_list
            [
              Sinks.Transform
                {
                  local = TaintTransforms.empty;
                  global = TaintTransforms.of_named_transforms [named_transform "TW"];
                  base = Sinks.NamedSink "Z";
                };
            ] );
        ( Sources.Transform
            {
              local = TaintTransforms.empty;
              global =
                TaintTransforms.of_named_transforms [named_transform "TW"; named_transform "TU"];
              base = Sources.NamedSource "C";
            },
          Sinks.Set.of_list [Sinks.NamedSink "Z"] );
      ]
    ~matching_sources:
      [
        Sinks.NamedSink "X", Sources.Set.of_list [Sources.NamedSource "A"];
        ( Sinks.NamedSink "Z",
          Sources.Set.of_list
            [
              Sources.Transform
                {
                  local = TaintTransforms.empty;
                  global =
                    TaintTransforms.of_named_transforms [named_transform "TW"; named_transform "TU"];
                  base = Sources.NamedSource "C";
                };
            ] );
        ( Sinks.Transform
            {
              local = TaintTransforms.empty;
              global = TaintTransforms.of_named_transforms [named_transform "TW"];
              base = Sinks.NamedSink "Z";
            },
          Sources.Set.of_list
            [
              Sources.Transform
                {
                  local = TaintTransforms.empty;
                  global = TaintTransforms.of_named_transforms [named_transform "TU"];
                  base = Sources.NamedSource "C";
                };
            ] );
        ( Sinks.Transform
            {
              local = TaintTransforms.empty;
              global =
                TaintTransforms.of_named_transforms [named_transform "TU"; named_transform "TW"];
              base = Sinks.NamedSink "Z";
            },
          Sources.Set.of_list [Sources.NamedSource "C"] );
      ]
    ~possible_tito_transforms:
      [
        TaintTransforms.empty;
        TaintTransforms.of_named_transforms [named_transform "TU"];
        TaintTransforms.of_named_transforms [named_transform "TW"];
        TaintTransforms.of_named_transforms [named_transform "TU"; named_transform "TW"];
      ]
    ();
  ()


let test_filters _ =
  let configuration =
    assert_parse
      {|
    { "sources": [
        { "name": "A" },
        { "name": "B" }
      ],
      "sinks": [
        { "name": "C" },
        { "name": "D" }
      ],
      "rules": [
        {
           "name": "test rule",
           "sources": ["A"],
           "sinks": ["D"],
           "code": 1001,
           "message_format": "whatever",
           "filters": {
             "maximum_source_distance": 2,
             "maximum_sink_distance": 3
           }
        }
      ]
    }
  |}
  in
  assert_equal (without_locations configuration.sources) [named "A"; named "B"];
  assert_equal (without_locations configuration.sinks) [named "C"; named "D"];
  assert_equal (List.length configuration.rules) 1;
  assert_equal (List.hd_exn configuration.rules).code 1001;
  assert_equal
    ~cmp:(Option.equal Rule.equal_filters)
    (List.hd_exn configuration.rules).filters
    (Some { Rule.maximum_source_distance = Some 2; maximum_sink_distance = Some 3 })


let () =
  "configuration"
  >::: [
         "combined_source_rules" >:: test_combined_source_rules;
         "string_combine_rules" >:: test_string_combine_rules;
         "implicit_sources" >:: test_implicit_sources;
         "implicit_sinks" >:: test_implicit_sinks;
         "invalid_sink" >:: test_invalid_sink;
         "invalid_source" >:: test_invalid_source;
         "invalid_transform" >:: test_invalid_transform;
         "multiple_configurations" >:: test_multiple_configurations;
         "partial_sink_converter" >:: test_partial_sink_converter;
         "simple" >:: test_simple;
         "transform" >:: test_transform;
         "transform_splits" >:: test_transform_splits;
         "validate" >:: test_validate;
         "matching_kinds" >:: test_matching_kinds;
         "filters" >:: test_filters;
       ]
  |> Test.run
