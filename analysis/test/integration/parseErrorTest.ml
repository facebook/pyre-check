(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_basic context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors "def good_syntax() -> int: ..." [];
  assert_type_errors
    "def bad_syntax("
    ["Parsing failure [404]: Could not parse file at test.py:2:0-2:0"];
  assert_type_errors
    "def good_syntax() -> int: ..."
    ~update_environment_with:[{ Test.handle = "foo.py"; source = "def bad_syntax(" }]
    [];
  ()


let () = "parse_error" >::: ["basic" >:: test_basic] |> Test.run
