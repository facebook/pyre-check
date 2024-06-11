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
          ~infer_self_tito:false
          ~infer_argument_tito:false
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
    ~printer:(fun rules -> rules |> List.map ~f:Rule.show |> String.concat ~sep:"\n")
    ~cmp:(List.equal [%compare.equal: Rule.t])
    expected
    actual


let without_locations sources_or_sinks =
  List.map
    ~f:(fun source_or_sink ->
      { source_or_sink with AnnotationParser.KindDefinition.location = None })
    sources_or_sinks


let named name = { AnnotationParser.KindDefinition.name; kind = Named; location = None }

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
    [TaintTransform.Named "A"; TaintTransform.Named "A"];
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


let test_new_combined_source_rules _ =
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
            message = "Expected a string";
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
             "code": 2001,
             "message_format": "some form",
             "rule": [
               {
                 "sources": [ "A" ],
                 "sinks": [ "CA" ]
               },
               {
                 "sources": [ "B" ],
                 "sinks": [ "CB" ]
               }
             ]
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
                JsonParsing.JsonAst.Node.value =
                  `List
                    [
                      {
                        JsonParsing.JsonAst.Node.value = `Float 123.;
                        location =
                          {
                            JsonParsing.JsonAst.Location.start = { line = 13; column = 31 };
                            stop = { line = 13; column = 33 };
                          };
                      };
                    ];
                location =
                  {
                    JsonParsing.JsonAst.Location.start = { line = 13; column = 31 };
                    stop = { line = 13; column = 33 };
                  };
              };
            message = "Expected a string or an array of strings";
            section = Some "sources";
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
             "code": 2001,
             "message_format": "some form",
             "rule": [
               {
                 "sources": [ 123 ],
                 "partial_sink": "CombinedSinkA"
               },
               {
                 "sources": [ "B" ],
                 "partial_sink": "CombinedSinkB"
               }
             ]
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
           "code": 2001,
           "message_format": "some form",
           "rule": [
             {
               "sources": [ "A" ],
               "partial_sink": "CA"
             },
             {
               "sources": [ "B" ],
               "partial_sink": "CB"
             }
           ]
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
          Rule.sources = [Sources.NamedSource "B"];
          sinks = [Sinks.TriggeredPartialSink { partial_sink = "CB"; triggering_source = "A" }];
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
                    JsonParsing.JsonAst.Location.start = { line = 9; column = 20 };
                    stop = { line = 9; column = 23 };
                  };
              };
          name = "test combined rule";
        };
        {
          Rule.sources = [Sources.NamedSource "A"];
          sinks = [Sinks.TriggeredPartialSink { partial_sink = "CA"; triggering_source = "B" }];
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
                    JsonParsing.JsonAst.Location.start = { line = 9; column = 20 };
                    stop = { line = 9; column = 23 };
                  };
              };
          name = "test combined rule";
        };
      ];
  assert_equal (List.hd_exn configuration.rules).code 2001;
  assert_equal
    ~printer:TaintConfiguration.RegisteredPartialSinks.show
    ~cmp:TaintConfiguration.RegisteredPartialSinks.equal
    (TaintConfiguration.RegisteredPartialSinks.of_alist_exn ["CA", ["CB"]; "CB", ["CA"]])
    configuration.registered_partial_sinks;
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
           "code": 2001,
           "message_format": "some form",
           "rule": [
             {
               "partial_sink": "CombinedSinkA",
               "sources": [ "A" ]
             },
             {
               "partial_sink": "CombinedSinkB",
               "sources": [ "B", "C" ]
             }
           ]
        }
      ]
    }
  |}
  in
  assert_equal (without_locations configuration.sources) [named "A"; named "B"; named "C"];
  assert_equal configuration.sinks [];
  assert_equal
    ~printer:TaintConfiguration.RegisteredPartialSinks.show
    ~cmp:TaintConfiguration.RegisteredPartialSinks.equal
    (TaintConfiguration.RegisteredPartialSinks.of_alist_exn
       ["CombinedSinkA", ["CombinedSinkB"]; "CombinedSinkB", ["CombinedSinkA"]])
    configuration.registered_partial_sinks;
  assert_rules_equal
    ~actual:configuration.rules
    ~expected:
      [
        {
          Rule.sources = [Sources.NamedSource "B"; Sources.NamedSource "C"];
          sinks =
            [Sinks.TriggeredPartialSink { partial_sink = "CombinedSinkB"; triggering_source = "A" }];
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
                    JsonParsing.JsonAst.Location.start = { line = 11; column = 20 };
                    stop = { line = 11; column = 23 };
                  };
              };
          name = "test combined rule";
        };
        {
          Rule.sources = [Sources.NamedSource "A"];
          sinks =
            [
              Sinks.TriggeredPartialSink { partial_sink = "CombinedSinkA"; triggering_source = "B" };
              Sinks.TriggeredPartialSink { partial_sink = "CombinedSinkA"; triggering_source = "C" };
            ];
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
                    JsonParsing.JsonAst.Location.start = { line = 11; column = 20 };
                    stop = { line = 11; column = 23 };
                  };
              };
          name = "test combined rule";
        };
      ];
  assert_equal (List.hd_exn configuration.rules).code 2001;
  (* Test using the same parital sink kind in multiple combined source rules *)
  let configuration =
    assert_parse
      {|
      { "sources": [
          { "name": "A" },
          { "name": "B" },
          { "name": "C" },
          { "name": "D" }
        ],
        "combined_source_rules": [
          {
             "name": "test combined rule 1",
             "code": 2001,
             "message_format": "some form",
             "rule": [
               {
                 "sources": [ "A" ],
                 "partial_sink": "TestSinkA"
               },
               {
                 "sources": [ "B" ],
                 "partial_sink": "TestSinkB"
               }
             ]
          },
          {
             "name": "test combined rule 2",
             "code": 2002,
             "message_format": "other form",
             "rule": [
               {
                 "sources": [ "A" ],
                 "partial_sink": "TestSinkA"
               },
               {
                 "sources": [ "D" ],
                 "partial_sink": "TestSinkB"
               }
             ]
          }
        ]
      }
    |}
  in
  assert_rules_equal
    ~actual:configuration.rules
    ~expected:
      [
        {
          Rule.sources = [Sources.NamedSource "D"];
          sinks =
            [Sinks.TriggeredPartialSink { partial_sink = "TestSinkB"; triggering_source = "A" }];
          transforms = [];
          code = 2002;
          message_format = "other form";
          filters = None;
          location =
            Some
              {
                JsonParsing.JsonAst.LocationWithPath.path = PyrePath.create_absolute "/taint.config";
                location =
                  {
                    JsonParsing.JsonAst.Location.start = { line = 26; column = 22 };
                    stop = { line = 26; column = 25 };
                  };
              };
          name = "test combined rule 2";
        };
        {
          Rule.sources = [Sources.NamedSource "A"];
          sinks =
            [Sinks.TriggeredPartialSink { partial_sink = "TestSinkA"; triggering_source = "D" }];
          transforms = [];
          code = 2002;
          message_format = "other form";
          filters = None;
          location =
            Some
              {
                JsonParsing.JsonAst.LocationWithPath.path = PyrePath.create_absolute "/taint.config";
                location =
                  {
                    JsonParsing.JsonAst.Location.start = { line = 26; column = 22 };
                    stop = { line = 26; column = 25 };
                  };
              };
          name = "test combined rule 2";
        };
        {
          Rule.sources = [Sources.NamedSource "B"];
          sinks =
            [Sinks.TriggeredPartialSink { partial_sink = "TestSinkB"; triggering_source = "A" }];
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
                    JsonParsing.JsonAst.Location.start = { line = 11; column = 22 };
                    stop = { line = 11; column = 25 };
                  };
              };
          name = "test combined rule 1";
        };
        {
          Rule.sources = [Sources.NamedSource "A"];
          sinks =
            [Sinks.TriggeredPartialSink { partial_sink = "TestSinkA"; triggering_source = "B" }];
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
                    JsonParsing.JsonAst.Location.start = { line = 11; column = 22 };
                    stop = { line = 11; column = 25 };
                  };
              };
          name = "test combined rule 1";
        };
      ]


let test_old_combined_source_rules _ =
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
    ~printer:TaintConfiguration.RegisteredPartialSinks.show
    ~cmp:TaintConfiguration.RegisteredPartialSinks.equal
    (TaintConfiguration.RegisteredPartialSinks.of_alist_exn
       ["CombinedSink[a]", ["CombinedSink[b]"]; "CombinedSink[b]", ["CombinedSink[a]"]])
    configuration.registered_partial_sinks;
  assert_rules_equal
    ~actual:configuration.rules
    ~expected:
      [
        {
          Rule.sources = [Sources.NamedSource "B"; Sources.NamedSource "C"];
          sinks =
            [
              Sinks.TriggeredPartialSink
                { partial_sink = "CombinedSink[b]"; triggering_source = "A" };
            ];
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
          sinks =
            [
              Sinks.TriggeredPartialSink
                { partial_sink = "CombinedSink[a]"; triggering_source = "B" };
              Sinks.TriggeredPartialSink
                { partial_sink = "CombinedSink[a]"; triggering_source = "C" };
            ];
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
  assert_equal (List.hd_exn configuration.rules).code 2001


let test_new_string_combine_rules _ =
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
  (* Section `rules` must be in the expected form. *)
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
      { "sources": [
          { "name": "A" }
        ],
        "string_combine_rules": [
          {
             "name": "",
             "code": 2001,
             "message_format": "",
             "rule": [
             {
               "sources": [ "A" ],
               "partial_sink": "UserDefinedPartialSinkA"
             }
           ]
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
           "code": 2001,
           "message_format": "rule message",
           "rule": [
             {
               "sources": [ "A" ],
               "partial_sink": "UserDefinedPartialSinkA"
             },
             {
               "sources": [ "B", "C" ],
               "partial_sink": "UserDefinedPartialSinkBC"
             }
           ]
        }
      ]
    }
  |}
  in
  assert_equal (without_locations configuration.sources) [named "A"; named "B"; named "C"];
  assert_equal
    ~printer:TaintConfiguration.RegisteredPartialSinks.show
    ~cmp:TaintConfiguration.RegisteredPartialSinks.equal
    (TaintConfiguration.RegisteredPartialSinks.of_alist_exn
       [
         "UserDefinedPartialSinkA", ["UserDefinedPartialSinkBC"];
         "UserDefinedPartialSinkBC", ["UserDefinedPartialSinkA"];
       ])
    configuration.registered_partial_sinks;
  assert_rules_equal
    ~actual:configuration.rules
    ~expected:
      [
        {
          Rule.sources = [Sources.NamedSource "A"];
          sinks =
            [
              Sinks.TriggeredPartialSink
                { partial_sink = "UserDefinedPartialSinkA"; triggering_source = "B" };
              Sinks.TriggeredPartialSink
                { partial_sink = "UserDefinedPartialSinkA"; triggering_source = "C" };
            ];
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
                    JsonParsing.JsonAst.Location.start = { line = 11; column = 20 };
                    stop = { line = 11; column = 23 };
                  };
              };
          name = "rule name";
        };
        {
          Rule.sources = [Sources.NamedSource "B"; Sources.NamedSource "C"];
          sinks =
            [
              Sinks.TriggeredPartialSink
                { partial_sink = "UserDefinedPartialSinkBC"; triggering_source = "A" };
            ];
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
                    JsonParsing.JsonAst.Location.start = { line = 11; column = 20 };
                    stop = { line = 11; column = 23 };
                  };
              };
          name = "rule name";
        };
      ];
  assert_equal
    ~printer:TaintConfiguration.StringOperationPartialSinks.show
    ~cmp:TaintConfiguration.StringOperationPartialSinks.equal
    (TaintConfiguration.StringOperationPartialSinks.of_list
       ["UserDefinedPartialSinkA"; "UserDefinedPartialSinkBC"])
    configuration.string_combine_partial_sinks;
  (* Test using the same partial sink kind in multiple string combine rules *)
  let configuration =
    assert_parse
      {|
      { "sources": [
          { "name": "A" },
          { "name": "B" },
          { "name": "C" },
          { "name": "D" }
        ],
        "string_combine_rules": [
          {
            "name": "rule name 1",
            "code": 2001,
            "message_format": "rule message 1",
            "rule": [
              {
                "sources": [ "A" ],
                "partial_sink": "UserDefinedPartialSink1"
              },
              {
                "sources": [ "B", "C" ],
                "partial_sink": "UserDefinedPartialSink2"
              }
            ]
          },
          {
            "name": "rule name 2",
            "code": 2002,
            "message_format": "rule message 2",
            "rule": [
              {
                "sources": [ "A" ],
                "partial_sink": "UserDefinedPartialSink1"
              },
              {
                "sources": [ "C", "D" ],
                "partial_sink": "UserDefinedPartialSink2"
              }
            ]
          }
        ]
      }
    |}
  in
  assert_rules_equal
    ~actual:configuration.rules
    ~expected:
      [
        {
          Rule.sources = [Sources.NamedSource "A"];
          sinks =
            [
              Sinks.TriggeredPartialSink
                { partial_sink = "UserDefinedPartialSink1"; triggering_source = "B" };
              Sinks.TriggeredPartialSink
                { partial_sink = "UserDefinedPartialSink1"; triggering_source = "C" };
            ];
          transforms = [];
          code = 2001;
          message_format = "rule message 1";
          filters = None;
          location =
            Some
              {
                JsonParsing.JsonAst.LocationWithPath.path = PyrePath.create_absolute "/taint.config";
                location =
                  {
                    JsonParsing.JsonAst.Location.start = { line = 11; column = 21 };
                    stop = { line = 11; column = 24 };
                  };
              };
          name = "rule name 1";
        };
        {
          Rule.sources = [Sources.NamedSource "B"; Sources.NamedSource "C"];
          sinks =
            [
              Sinks.TriggeredPartialSink
                { partial_sink = "UserDefinedPartialSink2"; triggering_source = "A" };
            ];
          transforms = [];
          code = 2001;
          message_format = "rule message 1";
          filters = None;
          location =
            Some
              {
                JsonParsing.JsonAst.LocationWithPath.path = PyrePath.create_absolute "/taint.config";
                location =
                  {
                    JsonParsing.JsonAst.Location.start = { line = 11; column = 21 };
                    stop = { line = 11; column = 24 };
                  };
              };
          name = "rule name 1";
        };
        {
          Rule.sources = [Sources.NamedSource "A"];
          sinks =
            [
              Sinks.TriggeredPartialSink
                { partial_sink = "UserDefinedPartialSink1"; triggering_source = "C" };
              Sinks.TriggeredPartialSink
                { partial_sink = "UserDefinedPartialSink1"; triggering_source = "D" };
            ];
          transforms = [];
          code = 2002;
          message_format = "rule message 2";
          filters = None;
          location =
            Some
              {
                JsonParsing.JsonAst.LocationWithPath.path = PyrePath.create_absolute "/taint.config";
                location =
                  {
                    JsonParsing.JsonAst.Location.start = { line = 26; column = 21 };
                    stop = { line = 26; column = 24 };
                  };
              };
          name = "rule name 2";
        };
        {
          Rule.sources = [Sources.NamedSource "C"; Sources.NamedSource "D"];
          sinks =
            [
              Sinks.TriggeredPartialSink
                { partial_sink = "UserDefinedPartialSink2"; triggering_source = "A" };
            ];
          transforms = [];
          code = 2002;
          message_format = "rule message 2";
          filters = None;
          location =
            Some
              {
                JsonParsing.JsonAst.LocationWithPath.path = PyrePath.create_absolute "/taint.config";
                location =
                  {
                    JsonParsing.JsonAst.Location.start = { line = 26; column = 21 };
                    stop = { line = 26; column = 24 };
                  };
              };
          name = "rule name 2";
        };
      ]


let test_old_string_combine_rules _ =
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
    ~printer:TaintConfiguration.RegisteredPartialSinks.show
    ~cmp:TaintConfiguration.RegisteredPartialSinks.equal
    (TaintConfiguration.RegisteredPartialSinks.of_alist_exn
       [
         "UserDefinedPartialSink[main]", ["UserDefinedPartialSink[secondary]"];
         "UserDefinedPartialSink[secondary]", ["UserDefinedPartialSink[main]"];
       ])
    configuration.registered_partial_sinks;
  assert_rules_equal
    ~actual:configuration.rules
    ~expected:
      [
        {
          Rule.sources = [Sources.NamedSource "A"];
          sinks =
            [
              Sinks.TriggeredPartialSink
                { partial_sink = "UserDefinedPartialSink[main]"; triggering_source = "B" };
              Sinks.TriggeredPartialSink
                { partial_sink = "UserDefinedPartialSink[main]"; triggering_source = "C" };
            ];
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
            [
              Sinks.TriggeredPartialSink
                { partial_sink = "UserDefinedPartialSink[secondary]"; triggering_source = "A" };
            ];
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
    ~printer:TaintConfiguration.StringOperationPartialSinks.show
    ~cmp:TaintConfiguration.StringOperationPartialSinks.equal
    configuration.string_combine_partial_sinks
    (TaintConfiguration.StringOperationPartialSinks.of_list
       ["UserDefinedPartialSink[main]"; "UserDefinedPartialSink[secondary]"])


let test_partial_sink_converter _ =
  let assert_triggered_sinks configuration ~partial_sink ~source ~expected_sink =
    let configuration = assert_parse configuration in
    TaintConfiguration.PartialSinkConverter.get_triggered_sinks_if_matched
      configuration.TaintConfiguration.Heap.partial_sink_converter
      ~partial_sink
      ~source
    |> assert_equal
         ~cmp:Sinks.PartialSink.Triggered.Set.equal
         ~printer:Sinks.PartialSink.Triggered.Set.show
         expected_sink
  in
  let configuration =
    {|
    {
      "sources": [{ "name": "A" }, { "name": "B" }, { "name": "D" }],
      "sinks": [],
      "combined_source_rules": [
        {
           "name": "c rule",
           "code": 2001,
           "message_format": "some form",
           "rule": [
             {
               "sources": [ "A" ],
               "partial_sink": "C[ca]"
             },
             {
               "sources": [ "B" ],
               "partial_sink": "C[cb]"
             }
           ]
        },
        {
           "name": "another rule",
           "code": 2002,
           "message_format": "some form",
           "rule": [
              {
                "sources": [ "A" ],
                "partial_sink": "C[ca]"
              },
              {
                "sources": [ "D" ],
                "partial_sink": "C[cd]"
              }
            ]
        }
      ]
    }
  |}
  in
  assert_triggered_sinks
    configuration
    ~partial_sink:"C[ca]"
    ~source:(Sources.NamedSource "A")
    ~expected_sink:
      ([
         { Sinks.PartialSink.Triggered.partial_sink = "C[cb]"; triggering_source = "A" };
         { Sinks.PartialSink.Triggered.partial_sink = "C[cd]"; triggering_source = "A" };
       ]
      |> Sinks.PartialSink.Triggered.Set.of_list);
  assert_triggered_sinks
    configuration
    ~partial_sink:"C[cb]"
    ~source:(Sources.NamedSource "B")
    ~expected_sink:
      (Sinks.PartialSink.Triggered.Set.singleton
         { Sinks.PartialSink.Triggered.partial_sink = "C[ca]"; triggering_source = "B" });
  assert_triggered_sinks
    configuration
    ~partial_sink:"C[ca]"
    ~source:(Sources.NamedSource "B")
    ~expected_sink:Sinks.PartialSink.Triggered.Set.empty;
  assert_triggered_sinks
    configuration
    ~partial_sink:"C[cb]"
    ~source:(Sources.NamedSource "A")
    ~expected_sink:Sinks.PartialSink.Triggered.Set.empty


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
    ~errors:[TaintConfiguration.Error.TransformDuplicate "Test"]
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
           "code": 2002,
           "message_format": "some form",
           "rule": [
            {
              "sources": [ "A" ],
              "partial_sink": "D[a]"
            },
            {
              "sources": [ "B" ],
              "partial_sink": "D[b]"
            }
          ]
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
        TaintTransforms.of_named_transforms [TaintTransform.Named "X"];
        TaintTransforms.of_named_transforms [TaintTransform.Named "X"; TaintTransform.Named "Y"];
        TaintTransforms.of_named_transforms [TaintTransform.Named "Y"];
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
              "code": 1,
              "message_format": "",
              "rule": [
                {
                  "sources": [ "A" ],
                  "partial_sink": "PartialSink1[a]"
                },
                {
                  "sources": [ "B" ],
                  "partial_sink": "PartialSink1[b]"
                }
              ]
            },
            {
              "name": "test combined rule 2",
              "code": 2,
              "message_format": "",
              "rule": [
                {
                  "sources": [ "C" ],
                  "partial_sink": "PartialSink2[c]"
                },
                {
                  "sources": [ "D" ],
                  "partial_sink": "PartialSink2[d]"
                }
              ]
            },
            {
              "name": "test combined rule 3",
              "code": 3,
              "message_format": "",
              "rule": [
                {
                  "sources": [ "A" ],
                  "partial_sink": "PartialSink3[a]"
                },
                {
                  "sources": [ "D" ],
                  "partial_sink": "PartialSink3[d]"
                }
              ]
            }
          ]
        }
      |}
    ~matching_sinks:
      [
        ( Sources.NamedSource "A",
          Sinks.Set.of_list
            [
              Sinks.TriggeredPartialSink
                { partial_sink = "PartialSink1[a]"; triggering_source = "B" };
              Sinks.TriggeredPartialSink
                { partial_sink = "PartialSink3[a]"; triggering_source = "D" };
            ] );
        ( Sources.NamedSource "B",
          Sinks.Set.of_list
            [
              Sinks.TriggeredPartialSink
                { partial_sink = "PartialSink1[b]"; triggering_source = "A" };
            ] );
      ]
    ~matching_sources:
      [
        ( Sinks.TriggeredPartialSink { partial_sink = "PartialSink1[a]"; triggering_source = "B" },
          Sources.Set.of_list [Sources.NamedSource "A"] );
        ( Sinks.TriggeredPartialSink { partial_sink = "PartialSink1[b]"; triggering_source = "A" },
          Sources.Set.of_list [Sources.NamedSource "B"] );
        ( Sinks.TriggeredPartialSink { partial_sink = "PartialSink3[a]"; triggering_source = "D" },
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
                  global = TaintTransforms.of_named_transforms [TaintTransform.Named "TU"];
                  base = Sinks.NamedSink "Y";
                };
            ] );
        ( Sources.Transform
            {
              local = TaintTransforms.empty;
              global = TaintTransforms.of_named_transforms [TaintTransform.Named "TU"];
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
                  global = TaintTransforms.of_named_transforms [TaintTransform.Named "TU"];
                  base = Sources.NamedSource "B";
                };
            ] );
        ( Sinks.Transform
            {
              local = TaintTransforms.empty;
              global = TaintTransforms.of_named_transforms [TaintTransform.Named "TU"];
              base = Sinks.NamedSink "Y";
            },
          Sources.Set.of_list [Sources.NamedSource "B"] );
      ]
    ~possible_tito_transforms:
      [TaintTransforms.empty; TaintTransforms.of_named_transforms [TaintTransform.Named "TU"]]
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
                    TaintTransforms.of_named_transforms
                      [TaintTransform.Named "TU"; TaintTransform.Named "TW"];
                  base = Sinks.NamedSink "Z";
                };
            ] );
        ( Sources.Transform
            {
              local = TaintTransforms.empty;
              global = TaintTransforms.of_named_transforms [TaintTransform.Named "TU"];
              base = Sources.NamedSource "C";
            },
          Sinks.Set.of_list
            [
              Sinks.Transform
                {
                  local = TaintTransforms.empty;
                  global = TaintTransforms.of_named_transforms [TaintTransform.Named "TW"];
                  base = Sinks.NamedSink "Z";
                };
            ] );
        ( Sources.Transform
            {
              local = TaintTransforms.empty;
              global =
                TaintTransforms.of_named_transforms
                  [TaintTransform.Named "TW"; TaintTransform.Named "TU"];
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
                    TaintTransforms.of_named_transforms
                      [TaintTransform.Named "TW"; TaintTransform.Named "TU"];
                  base = Sources.NamedSource "C";
                };
            ] );
        ( Sinks.Transform
            {
              local = TaintTransforms.empty;
              global = TaintTransforms.of_named_transforms [TaintTransform.Named "TW"];
              base = Sinks.NamedSink "Z";
            },
          Sources.Set.of_list
            [
              Sources.Transform
                {
                  local = TaintTransforms.empty;
                  global = TaintTransforms.of_named_transforms [TaintTransform.Named "TU"];
                  base = Sources.NamedSource "C";
                };
            ] );
        ( Sinks.Transform
            {
              local = TaintTransforms.empty;
              global =
                TaintTransforms.of_named_transforms
                  [TaintTransform.Named "TU"; TaintTransform.Named "TW"];
              base = Sinks.NamedSink "Z";
            },
          Sources.Set.of_list [Sources.NamedSource "C"] );
      ]
    ~possible_tito_transforms:
      [
        TaintTransforms.empty;
        TaintTransforms.of_named_transforms [TaintTransform.Named "TU"];
        TaintTransforms.of_named_transforms [TaintTransform.Named "TW"];
        TaintTransforms.of_named_transforms [TaintTransform.Named "TU"; TaintTransform.Named "TW"];
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
         "new_combined_source_rules" >:: test_new_combined_source_rules;
         "new_string_combine_rules" >:: test_new_string_combine_rules;
         "old_combined_source_rules" >:: test_old_combined_source_rules;
         "old_string_combine_rules" >:: test_old_string_combine_rules;
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
