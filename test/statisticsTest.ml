(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2

open! Test  (* Suppresses logging. *)


let () =
  let configuration = Configuration.Analysis.create () in
  Configuration.Analysis.set_global configuration


let test_request_sample_format _ =
  let sample =
    Statistics.sample
      ~integers:["elapsed_time", 420]
      ~normals:["request kind", "TypeCheck"]
      ~metadata:false
      ()
  in
  let expected_sample =
    Yojson.Safe.to_string (
      `Assoc [
        "int",
        `Assoc [
          "elapsed_time", `Int 420;
        ];
        "normal",
        `Assoc [
          "request kind", `String "TypeCheck";
        ];
      ])
  in
  assert_equal sample expected_sample


let test_end_to_end_format _ =
  let sample =
    Statistics.sample
      ~integers:["elapsed_time", 36]
      ~normals:["root", "/path/to/source"]
      ~metadata:false
      ()
  in
  let expected_sample =
    Yojson.Safe.to_string (
      `Assoc [
        "int",
        `Assoc [
          "elapsed_time", `Int 36;
        ];
        "normal",
        `Assoc [
          "root", `String "/path/to/source";
        ];
      ])
  in
  assert_equal sample expected_sample


let test_coverage_sample_format _ =
  let sample =
    Statistics.sample
      ~integers:["strict_coverage", 7; "declare_coverage", 25]
      ~normals:["root", "/path/to/source"]
      ~metadata:false
      ()
  in
  let expected_sample =
    Yojson.Safe.to_string (
      `Assoc [
        "int",
        `Assoc [
          "strict_coverage", `Int 7;
          "declare_coverage", `Int 25
        ];
        "normal",
        `Assoc [
          "root", `String "/path/to/source";
        ];
      ])
  in
  assert_equal sample expected_sample


let () =
  "performanceLogger">:::[
    "request_sample_format">::test_request_sample_format;
    "end_to_end_format">::test_end_to_end_format;
    "coverage_sample_format">::test_coverage_sample_format;
  ]
  |> Test.run
