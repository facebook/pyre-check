(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Test
open TypeCheck

let assert_errors ?(show_error_traces = false) ~context input_source expected_errors =
  let ast_environment, type_errors =
    let project = ScratchProject.setup ~context ["test.py", input_source] in
    let { ScratchProject.BuiltTypeEnvironment.type_environment; _ }, type_errors =
      ScratchProject.build_type_environment_and_postprocess project
    in
    TypeEnvironment.ReadOnly.ast_environment type_environment, type_errors
  in
  let descriptions =
    List.map type_errors ~f:(fun error ->
        Error.instantiate
          ~show_error_traces
          ~lookup:(AstEnvironment.ReadOnly.get_real_path_relative ast_environment)
          error
        |> Error.Instantiated.description)
  in
  let description_list_to_string descriptions =
    Format.asprintf "%a" Sexp.pp [%message (descriptions : string list)]
  in
  assert_equal
    ~cmp:(List.equal String.equal)
    ~printer:description_list_to_string
    ~pp_diff:
      (diff ~print:(fun format expected_errors ->
           Format.fprintf format "%s" (description_list_to_string expected_errors)))
    expected_errors
    descriptions;
  Memory.reset_shared_memory ()


let ignore_lines_test context =
  let assert_errors = assert_errors ~context in
  assert_errors {|
        def foo() -> int:
          return 1.0  # pyre-ignore
      |} [];
  assert_errors
    {|
        def foo() -> str:
          return 1.0  # pyre-ignore
        def bar() -> int:
          return 1.0  # pyre-ignore
      |}
    [];
  assert_errors
    {|
        class A:
          pass
        def foo() -> A:
          # pyre-ignore
          return 1
       |}
    [];
  assert_errors {|
        def foo() -> str:
          return 1.0  # pyre-ignore[7]
      |} [];
  assert_errors
    {|
        def foo() -> str:
          return 1.0  # pyre-ignore[7]
        def bar() -> int:
          return 1.0  # pyre-ignore[7]
      |}
    [];

  (* Test error on unused ignores *)
  assert_errors
    {|
        def foo(a: int) -> None:
          return a
      |}
    ["Incompatible return type [7]: Expected `None` but got `int`."];
  assert_errors {|
        def foo(a: int) -> None:
          return a  # pyre-ignore
      |} [];
  assert_errors {|
        def foo(a: int) -> None:
          return a  # pyre-fixme
      |} [];
  assert_errors {|
        def foo(a: int) -> None:
          return a  # type: ignore
      |} [];
  assert_errors
    {|
        def foo(a: int) -> int:
          return a  # pyre-ignore
      |}
    [
      "Unused ignore [0]: The `pyre-ignore` or `pyre-fixme` comment is not suppressing type \
       errors, please remove it.";
    ];
  assert_errors
    {|
        def foo(a: int) -> int:
          return a  # pyre-fixme
      |}
    [
      "Unused ignore [0]: The `pyre-ignore` or `pyre-fixme` comment is not suppressing type \
       errors, please remove it.";
    ];
  assert_errors {|
        def foo(a: int) -> int:
          return a  # type: ignore
      |} [];
  assert_errors
    {|
        def foo() -> str:
          return 1.0  # pyre-ignore[5]
      |}
    [
      "Incompatible return type [7]: Expected `str` but got `float`.";
      "Unused ignore [0]: The `pyre-ignore[5]` or `pyre-fixme[5]` comment is not suppressing type \
       errors, please remove it.";
    ];
  assert_errors
    {|
        def foo(a: int) -> int:
          return a  # pyre-ignore[7, 5]
      |}
    [
      "Unused ignore [0]: The `pyre-ignore[7, 5]` or `pyre-fixme[7, 5]` comment is not suppressing \
       type errors, please remove it.";
    ];
  assert_errors
    {|
        def foo(a: int) -> str:
          return a  # pyre-ignore[7, 5]
      |}
    [
      "Unused ignore [0]: The `pyre-ignore[5]` or `pyre-fixme[5]` comment is not suppressing type \
       errors, please remove it.";
    ];
  assert_errors
    {|
        def bar(x: int) -> int:
          return x
        def foo(a: int) -> str:
          return bar(a.undefined)  # pyre-ignore[7, 16]
      |}
    [];
  assert_errors
    {|
        def bar(x: int) -> int:
          return x
        def foo(a: int) -> str:
          return bar(a.undefined)  # pyre-ignore[7, 5, 16]
      |}
    [
      "Unused ignore [0]: The `pyre-ignore[5]` or `pyre-fixme[5]` comment is not suppressing type \
       errors, please remove it.";
    ];
  assert_errors
    {|
        # pyre-strict
        import typing
        # pyre-fixme[3]: Return annotation cannot be `Any`.
        # pyre-fixme[2]: Parameter annotation cannot be `Any`.
        # pyre-fixme[2]: Parameter annotation cannot be `Any`.
        def foo(x: typing.Any, y: typing.Any) -> typing.Any:
            return x
      |}
    [];

  assert_errors
    {|
        # pyre-ignore-all-errors
        # pyre-fixme[1]
        x: int = 42
      |}
    [];
  (* Ignores on source modes. *)
  assert_errors
    {|
        # pyre-strict
        # pyre-fixme[51]
        # pyre-ignore-all-errors
      |}
    [];
  ()


let bad_ignore_comments context =
  let assert_errors = assert_errors ~context in
  (* We use a hacky regex to get multiple error codes within square brackets. This barfed when there
     was a comma outside the brackets, when the ignore statement was `pyre-ignore-all-errors`. *)
  assert_errors
    {|
        # pyre-ignore-all-errors[2]: Yes, this single comma used to be a problem.
        def foo() -> int:
          return 1.0
      |}
    ["Incompatible return type [7]: Expected `int` but got `float`."];
  ()


let () =
  "ignore"
  >::: ["ignore_lines" >:: ignore_lines_test; "bad_ignore_comments" >:: bad_ignore_comments]
  |> Test.run
