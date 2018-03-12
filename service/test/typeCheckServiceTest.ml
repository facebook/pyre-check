(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Pyre
open Test

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
    let sources, _ =
      Service.Parser.parse_sources_list
        scheduler
        files
        ~configuration
    in
    let environment =
      Service.Environment.in_process_handler scheduler ~configuration ~stubs:[] ~sources
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


let () =
  "typeChecker">:::[
    "type_check_sources_list">::type_check_sources_list_test;
  ]
  |> run_test_tt_main
