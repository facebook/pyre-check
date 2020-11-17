(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Core
open Taint

let named name = { AnnotationParser.name; kind = Named }

let parametric name = { AnnotationParser.name; kind = Parametric }

let test_parse_source _ =
  let assert_parsed ~allowed ?subkind ~expected source =
    let actual = AnnotationParser.parse_source ~allowed ?subkind source in
    assert_equal ~cmp:Sources.equal actual expected
  in
  assert_parsed ~allowed:[named "A"] ~expected:(Sources.NamedSource "A") "A";
  assert_parsed ~allowed:[parametric "A"] ~expected:(Sources.NamedSource "A") "A";
  assert_parsed
    ~allowed:[parametric "A"]
    ~expected:(Sources.ParametricSource { source_name = "A"; subkind = "Subkind" })
    ~subkind:"Subkind"
    "A";
  assert_raises (Failure "Unsupported taint source `A`") (fun () ->
      AnnotationParser.parse_source ~allowed:[named "A"] ~subkind:"Subkind" "A");
  ()


let test_parse_sink _ =
  let assert_parsed ~allowed ?subkind ~expected sink =
    let actual = AnnotationParser.parse_sink ~allowed ?subkind sink in
    assert_equal ~cmp:Sinks.equal actual expected
  in
  assert_parsed ~allowed:[named "A"] ~expected:(Sinks.NamedSink "A") "A";
  assert_parsed ~allowed:[parametric "A"] ~expected:(Sinks.NamedSink "A") "A";
  assert_parsed
    ~allowed:[parametric "A"]
    ~expected:(Sinks.ParametricSink { sink_name = "A"; subkind = "Subkind" })
    ~subkind:"Subkind"
    "A";
  assert_raises (Failure "Unsupported taint sink `A`") (fun () ->
      AnnotationParser.parse_sink ~allowed:[named "A"] ~subkind:"Subkind" "A");
  assert_parsed ~allowed:[] ~expected:Sinks.LocalReturn "LocalReturn";
  assert_parsed ~allowed:[] ~expected:(Sinks.ParameterUpdate 2) "ParameterUpdate2";
  ()


let () =
  "annotationParser"
  >::: ["parse_source" >:: test_parse_source; "parse_sink" >:: test_parse_sink]
  |> Test.run
