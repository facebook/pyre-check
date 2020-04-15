(* Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
open Core
open OUnit2
open Taint
open Pyre

let test_triggered_sinks _ =
  let assert_triggered_sinks configuration ~partial_sink ~source ~expected_sink =
    TaintConfiguration.parse configuration
    |> PartialSinkConverter.create
    |> PartialSinkConverter.get_triggered_sink ~partial_sink ~source
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


let () = "partialSinkConverter" >::: ["triggered_sinks" >:: test_triggered_sinks] |> Test.run
