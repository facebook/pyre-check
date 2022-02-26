(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_basic context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors "def good_syntax() -> int: ..." [];
  assert_type_errors "def bad_syntax(" ["Parsing failure [404]: '(' was never closed"];
  assert_type_errors
    "def good_syntax() -> int: ..."
    ~update_environment_with:[{ Test.handle = "foo.py"; source = "def bad_syntax(" }]
    [];
  assert_type_errors {|
      # pyre-ignore-all-errors
      def bad_syntax(
    |} [];
  assert_type_errors {|
      # pyre-placeholder-stub
      def bad_syntax(
    |} [];
  assert_type_errors
    {|
      # pyre-ignore-all-errors[10]
      def bad_syntax(
    |}
    ["Parsing failure [404]: '(' was never closed"];
  assert_type_errors {|
      # pyre-ignore-all-errors[404]
      def bad_syntax(
    |} [];
  assert_type_errors {|
      # pyre-ignore-all-errors[10, 404]
      def bad_syntax(
    |} [];
  assert_type_errors
    {|
      # pyre-unsafe
      def bad_syntax(
    |}
    ["Parsing failure [404]: '(' was never closed"];
  assert_type_errors
    {|
      # pyre-strict
      def bad_syntax(
    |}
    ["Parsing failure [404]: '(' was never closed"];
  ()


let () = "parse_error" >::: ["basic" >:: test_basic] |> Test.run
