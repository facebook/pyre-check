(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Taint

let test_simple _ =
  let configuration =
    TaintConfiguration.parse
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
      ]
    }
  |}
  in
  assert_equal configuration.sources ["A"; "B"];
  assert_equal configuration.sinks ["C"; "D"];
  assert_equal configuration.features ["E"; "F"];
  assert_equal (List.length configuration.rules) 1;
  assert_equal (List.hd_exn configuration.rules).code 2001


let test_invalid_source _ =
  let parse () =
    TaintConfiguration.parse
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
    TaintConfiguration.parse
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
    TaintConfiguration.parse
      {|
    { sources: [
        { name: "A" },
        { name: "B" }
      ],
      sinks: [
        { name: "C" }
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
        Taint.TaintConfiguration.Rule.sources = [Sources.NamedSource "B"];
        sinks = [Sinks.TriggeredPartialSink { kind = "C"; label = "b" }];
        code = 2001;
        message_format = "some form";
        name = "test combined rule";
      };
      {
        Taint.TaintConfiguration.Rule.sources = [Sources.NamedSource "A"];
        sinks = [Sinks.TriggeredPartialSink { kind = "C"; label = "a" }];
        code = 2001;
        message_format = "some form";
        name = "test combined rule";
      };
    ];
  assert_equal (List.hd_exn configuration.rules).code 2001


let test_empty _ =
  assert_raises (Yojson.Json_error "Blank input data") (fun () ->
      let _ = TaintConfiguration.parse {| |} in
      ())


let () =
  "configuration"
  >::: [
         "combined_source_rules" >:: test_combined_source_rules;
         "empty" >:: test_empty;
         "invalid_sink" >:: test_invalid_sink;
         "invalid_source" >:: test_invalid_source;
         "simple" >:: test_simple;
       ]
  |> Test.run
