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
  assert_default_type_errors "def good_syntax() -> int: ..." [] context;
  assert_default_type_errors
    "def bad_syntax("
    ["Parsing failure [404]: '(' was never closed"]
    context;
  assert_default_type_errors
    "def good_syntax() -> int: ..."
    ~other_sources:[{ Test.handle = "foo.py"; source = "def bad_syntax(" }]
    []
    context;
  assert_default_type_errors
    {|
      # pyre-ignore-all-errors
      def bad_syntax(
    |}
    []
    context;
  assert_default_type_errors
    {|
      # pyre-ignore-all-errors[10]
      def bad_syntax(
    |}
    ["Parsing failure [404]: '(' was never closed"]
    context;
  assert_default_type_errors
    {|
      # pyre-ignore-all-errors[404]
      def bad_syntax(
    |}
    []
    context;
  assert_default_type_errors
    {|
      # pyre-ignore-all-errors[10, 404]
      def bad_syntax(
    |}
    []
    context;
  assert_default_type_errors
    {|
      # pyre-unsafe
      def bad_syntax(
    |}
    ["Parsing failure [404]: '(' was never closed"]
    context;
  assert_default_type_errors
    {|
      # pyre-strict
      def bad_syntax(
    |}
    ["Parsing failure [404]: '(' was never closed"]
    context;
  ()


let () = "parse_error" >::: ["basic" >:: test_basic] |> Test.run
