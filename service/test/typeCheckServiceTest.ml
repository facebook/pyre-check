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
      class typing.Sized: ...
      class float():
        pass
      class int(float):
        pass
      class str(typing.Sized):
        pass
    |}
  in
  let path, _ = Filename.open_temp_file ~in_dir:(Path.absolute root) "test" ".py" in
  [
    File.create
      ~content:(Some (default_content ^ "\n" ^ content |> trim_extra_indentation))
      (Path.create_relative ~root ~relative:path);
  ]


let type_check_sources_list_test context =
  let check context =
    let root = Path.current_working_directory () in
    let files =
      {|
        def foo() -> str:
            return 1
      |}
      |> create_files ~root
    in
    let configuration = Configuration.create () in
    let scheduler = Scheduler.mock () in
    let stubs = Service.Parser.parse_stubs scheduler ~configuration in
    let sources, _ =
      Service.Parser.parse_sources_list
        scheduler
        files
        ~configuration
    in
    let environment =
      Service.Environment.in_process_handler scheduler ~configuration ~stubs ~sources
    in

    let assert_errors_with_root project_root expected_error_count =
      let configuration =
        Configuration.create
          ~source_root:root
          ~project_root:(Path.create_absolute project_root)
          ()
      in
      let errors, _, _ =
        Service.TypeCheck.analyze_sources
          scheduler
          configuration
          environment
          sources
      in
      assert_equal (List.length errors) expected_error_count
    in
    assert_errors_with_root "/" 1;
    assert_errors_with_root (bracket_tmpdir context) 0;
  in
  with_bracket_chdir context (bracket_tmpdir context) check


let ignore_lines_test _ =
  let root = Path.current_working_directory () in
  let configuration = Configuration.create () in
  let scheduler = Scheduler.mock () in

  let assert_errors
      ?(show_error_traces = false)
      input_source
      expected_errors =
    let stubs = Service.Parser.parse_stubs scheduler ~configuration in
    let (sources, _) =
      Service.Parser.parse_sources_list scheduler (create_files ~root input_source) ~configuration
    in
    let environment =
      Service.Environment.in_process_handler scheduler ~configuration ~stubs ~sources
    in
    let descriptions =
      Service.TypeCheck.analyze_sources scheduler configuration environment sources
      |> fun (errors, _, _) ->
      List.map ~f:(fun error -> Error.description error ~detailed:show_error_traces) errors
    in
    let description_list_to_string descriptions =
      Format.asprintf "%a" Sexp.pp (sexp_of_list sexp_of_string descriptions)
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
  in

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
    ["Unused ignore [0]: Pyre ignore  is extraneous; there is no matching type error here."];
  assert_errors
    {|
      def foo(a: int) -> int:
        return a  # pyre-fixme
    |}
    ["Unused ignore [0]: Pyre ignore  is extraneous; there is no matching type error here."];
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
      "Unused ignore [0]: Pyre ignore [5] is extraneous; there is no matching type error here.";
      "Incompatible return type [7]: Expected `str` but got `float`."
    ];
  assert_errors
    {|
      def foo(a: int) -> int:
        return a  # pyre-ignore[7][5]
    |}
    [
      "Unused ignore [0]: Pyre ignores [7], [5] are extraneous; " ^
      "there is no matching type error here."
    ];
  assert_errors
    {|
      def foo(a: int) -> str:
        return a  # pyre-ignore[7][5]
    |}
    ["Unused ignore [0]: Pyre ignore [5] is extraneous; there is no matching type error here."];
  assert_errors
    {|
      def bar(x: int) -> int:
        return x
      def foo(a: int) -> str:
        return bar(a.undefined)  # pyre-ignore[7][16]
    |}
    [];
  assert_errors
    {|
      def bar(x: int) -> int:
        return x
      def foo(a: int) -> str:
        return bar(a.undefined)  # pyre-ignore[7][5][16]
    |}
    ["Unused ignore [0]: Pyre ignore [5] is extraneous; there is no matching type error here."]


let () =
  "typeChecker">:::[
    "type_check_sources_list">::type_check_sources_list_test;
    "ignore_lines">::ignore_lines_test
  ]
  |> run_test_tt_main
