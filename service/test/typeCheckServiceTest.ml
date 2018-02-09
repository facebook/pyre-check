(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Pyre
open Test

module Scheduler = Service.Scheduler


let type_check_sources_list_test context =
  let check context =
    let root = Path.current_working_directory () in
    let path, _ = Filename.open_temp_file ~in_dir:(Path.absolute root) "test" ".py" in
    let content =
      {|
        def foo() -> str:
            return 1
      |}
      |> trim_extra_indentation
    in
    let files =
      [
        File.create ~content:(Some content) (Path.create_relative ~root ~relative:path);
      ]
    in
    let configuration = Configuration.create () in
    let environment =
      Environment.handler ~configuration (Environment.Builder.create ~configuration ())
    in
    Environment.populate
      ~configuration
      environment
      [
        parse {|
          class int(float): pass
          class str: pass
        |}
      ];

    let assert_errors_with_root project_root expected_error_count =
      let configuration =
        Configuration.create
          ~source_root:root
          ~project_root:(Path.create_absolute project_root)
          ()
      in
      let sources, _ =
        Service.Parser.parse_sources_list
          (Scheduler.mock ())
          files
          ~configuration
      in
      let errors, _, _ =
        Service.TypeCheck.analyze_sources
          (Scheduler.mock ())
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
