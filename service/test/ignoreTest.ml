(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Pyre
open Test
open TypeCheck

module Scheduler = Service.Scheduler


let create_files ~root content =
  let default_content =
    {|
      class object():
        def __sizeof__() -> int: pass
      class typing.Sized: ...
      class float():
        pass
      class int(float):
        pass
      class str(typing.Sized):
        pass
    |}
    |> trim_extra_indentation
  in
  let path, _ = Filename.open_temp_file ~in_dir:(Path.absolute root) "test" ".py" in
  [
    File.create
      ~content:(Some (default_content ^ "\n" ^ (content |> trim_extra_indentation)))
      (Path.create_relative ~root ~relative:path);
  ]


let assert_errors
    ?(show_error_traces = false)
    input_source
    expected_errors =
  let root = Path.current_working_directory () in
  let configuration =
    Configuration.Analysis.create ~local_root:root ~project_root:(Path.create_absolute "/") ()
  in
  let scheduler = Scheduler.mock () in
  let handles =
    Service.Parser.parse_sources
      ~configuration
      ~scheduler
      ~files:(create_files ~root input_source)
  in
  let ((module Handler: Environment.Handler) as environment) =
    Service.Environment.handler ~configuration ~stubs:[] ~sources:handles
  in
  add_defaults_to_environment environment;
  Service.Ignore.register ~configuration scheduler handles;
  let descriptions =
    Service.Check.analyze_sources
      scheduler
      configuration
      environment
      handles
    |> fun (errors, _) ->
    List.map ~f:(fun error -> Error.description error ~detailed:show_error_traces) errors
  in
  Handler.purge handles;
  let description_list_to_string descriptions =
    Format.asprintf "%a" Sexp.pp [%message (descriptions: string list)]
  in
  assert_equal
    ~cmp:(List.equal ~equal:String.equal)
    ~printer:description_list_to_string
    ~pp_diff:
      (diff
         ~print:(fun format expected_errors ->
             Format.fprintf format "%s" (description_list_to_string expected_errors)))
    expected_errors
    descriptions


let ignore_lines_test context =
  let check _ =
    assert_errors
      {|
        def foo() -> int:
          return 1.0  # pyre-ignore
      |}
      [];
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
    assert_errors
      {|
        def foo() -> str:
          return 1.0  # pyre-ignore[7]
      |}
      [];
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
    assert_errors
      {|
        def foo(a: int) -> None:
          return a  # pyre-ignore
      |}
      [];
    assert_errors
      {|
        def foo(a: int) -> None:
          return a  # pyre-fixme
      |}
      [];
    assert_errors
      {|
        def foo(a: int) -> None:
          return a  # type: ignore
      |}
      [];
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
    assert_errors
      {|
        def foo(a: int) -> int:
          return a  # type: ignore
      |}
      [];
    assert_errors
      {|
        def foo() -> str:
          return 1.0  # pyre-ignore[5]
      |}
      [
        "Unused ignore [0]: Pyre ignore [5] is extraneous.";
        "Incompatible return type [7]: Expected `str` but got `float`."
      ];
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
      ["Unused ignore [0]: Pyre ignore [5] is extraneous."]
  in
  with_bracket_chdir context (bracket_tmpdir context) check


let () =
  "typeChecker">:::[
    "ignore_lines">::ignore_lines_test;
  ]
  |> Test.run
