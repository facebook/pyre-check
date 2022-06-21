(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_basic context =
  (* We need to use assert_default_type_errors because we want to avoid debug mode, which prevents
     suppressing syntax errors. *)
  let assert_default_type_errors = assert_default_type_errors ~context in
  assert_default_type_errors "def good_syntax() -> int: ..." [];
  assert_default_type_errors "def bad_syntax(" ["Parsing failure [404]: '(' was never closed"];
  assert_default_type_errors
    "def good_syntax() -> int: ..."
    ~update_environment_with:[{ Test.handle = "foo.py"; source = "def bad_syntax(" }]
    [];
  assert_default_type_errors {|
      # pyre-ignore-all-errors
      def bad_syntax(
    |} [];
  assert_default_type_errors {|
      # pyre-placeholder-stub
      def bad_syntax(
    |} [];
  assert_default_type_errors
    {|
      # pyre-ignore-all-errors[10]
      def bad_syntax(
    |}
    ["Parsing failure [404]: '(' was never closed"];
  assert_default_type_errors {|
      # pyre-ignore-all-errors[404]
      def bad_syntax(
    |} [];
  assert_default_type_errors
    {|
      # pyre-ignore-all-errors[10, 404]
      def bad_syntax(
    |}
    [];
  assert_default_type_errors
    {|
      # pyre-unsafe
      def bad_syntax(
    |}
    ["Parsing failure [404]: '(' was never closed"];
  assert_default_type_errors
    {|
      # pyre-strict
      def bad_syntax(
    |}
    ["Parsing failure [404]: '(' was never closed"];
  ()


let () = "parse_error" >::: ["basic" >:: test_basic] |> Test.run
