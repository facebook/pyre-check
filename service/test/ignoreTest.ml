(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Analysis
open Test
open TypeCheck

let ignore_lines_test context =
  let assert_errors ?(show_error_traces = false) input_source expected_errors =
    let configuration, sources, environment =
      let project = ScratchProject.setup ~context ["test.py", input_source] in
      let sources, _, environment = ScratchProject.build_environment project in
      let configuration = ScratchProject.configuration_of project in
      configuration, sources, environment
    in
    let scheduler = Scheduler.mock () in
    let descriptions =
      let ast_environment = Environment.ast_environment environment in
      Service.Check.analyze_sources ~scheduler ~configuration ~environment sources
      |> List.map ~f:(fun error ->
             Error.instantiate ~lookup:(AstEnvironment.ReadOnly.get_relative ast_environment) error
             |> Error.Instantiated.description ~show_error_traces)
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
      descriptions
  in
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
    ["Unused ignore [0]: Pyre ignore is extraneous."];
  assert_errors
    {|
        def foo(a: int) -> int:
          return a  # pyre-fixme
      |}
    ["Unused ignore [0]: Pyre ignore is extraneous."];
  assert_errors {|
        def foo(a: int) -> int:
          return a  # type: ignore
      |} [];
  assert_errors
    {|
        def foo() -> str:
          return 1.0  # pyre-ignore[5]
      |}
    [ "Incompatible return type [7]: Expected `str` but got `float`.";
      "Unused ignore [0]: Pyre ignore [5] is extraneous." ];
  assert_errors
    {|
        def foo(a: int) -> int:
          return a  # pyre-ignore[7, 5]
      |}
    ["Unused ignore [0]: Pyre ignores [7, 5] are extraneous."];
  assert_errors
    {|
        def foo(a: int) -> str:
          return a  # pyre-ignore[7, 5]
      |}
    ["Unused ignore [0]: Pyre ignore [5] is extraneous."];
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
    ["Unused ignore [0]: Pyre ignore [5] is extraneous."];
  assert_errors
    {|
        # pyre-strict
        # pyre-fixme[3]: Return annotation cannot be `Any`.
        # pyre-fixme[2]: Parameter annotation cannot be `Any`.
        # pyre-fixme[2]: Parameter annotation cannot be `Any`.
        def foo(x: typing.Any, y: typing.Any) -> typing.Any:
            return x
      |}
    []


let () = "typeChecker" >::: ["ignore_lines" >:: ignore_lines_test] |> Test.run
