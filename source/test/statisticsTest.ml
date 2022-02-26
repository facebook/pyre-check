(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

let test_request_sample_format _ =
  let sample =
    Statistics.sample
      ~integers:["elapsed_time", 420]
      ~normals:["request kind", "TypeCheck"]
      ~metadata:false
      ()
  in
  let expected_sample =
    Yojson.Safe.to_string
      (`Assoc
        [
          "int", `Assoc ["elapsed_time", `Int 420];
          "normal", `Assoc ["request kind", `String "TypeCheck"];
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
    Yojson.Safe.to_string
      (`Assoc
        [
          "int", `Assoc ["elapsed_time", `Int 36];
          "normal", `Assoc ["root", `String "/path/to/source"];
        ])
  in
  assert_equal sample expected_sample


let () =
  "performanceLogger"
  >::: [
         "request_sample_format" >:: test_request_sample_format;
         "end_to_end_format" >:: test_end_to_end_format;
       ]
  |> Test.run
