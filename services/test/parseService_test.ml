(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Test

open Ast

open Pyre


let test_parse_stubs_list _ =
  let root = Path.current_working_directory () in
  let create_with_relative relative =
    let content = Some "def f()->int: ...\n" in
    File.create ~content (Path.create_relative ~root ~relative)
  in
  let files =
    [
      create_with_relative "a.pyi";
      create_with_relative "dir/b.pyi";
      create_with_relative "2/c.pyi";
      create_with_relative "2and3/d.pyi";
    ]
  in
  let handles =
    ParseService.parse_stubs_list
      (Service.mock ())
      files
  in
  assert_equal (List.length files) (List.length handles);
  let get_handle_at position =
    File.handle (List.nth_exn files position)
    |> Option.value ~default:(File.Handle.create "")
  in
  let assert_stub_matches_name handle define_name =
    let source = AstSharedMemory.get_source handle in
    assert_is_some source;
    let { Source.statements; _ } = Option.value_exn source in
    (match statements with
     | [{
         Node.value = Statement.Stub (Statement.Stub.Define { Statement.Define.name; _ });
         _;
       }] ->
         assert_equal name (Expression.Access.create_from_identifiers define_name)
     | _ -> assert_unreached ())
  in
  assert_stub_matches_name (get_handle_at 0) [~~"a"; ~~"f"];
  assert_stub_matches_name (get_handle_at 1) [~~"dir"; ~~"b"; ~~"f"];
  assert_stub_matches_name (get_handle_at 2) [~~"c"; ~~"f"];
  assert_stub_matches_name (get_handle_at 3) [~~"d"; ~~"f"]


let test_parse_sources_list _ =
  let file =
    File.create
      ~content:(Some "def foo()->int:\n    return 1\n")
      (Path.create_relative ~root:(Path.current_working_directory ()) ~relative:"a.py")
  in
  let (handles, _) =
    ParseService.parse_sources_list
      (Service.mock ()) [file]
      ~root:(Path.current_working_directory ())
  in
  let handle = Option.value_exn (File.handle file) in
  assert_equal handles [handle];

  let source = AstSharedMemory.get_source handle in
  assert_equal (Option.is_some source) true;

  let { Source.path; statements; _ } = Option.value_exn source in
  assert_equal path "a.py";
  (match statements with
   | [{ Node.value = Statement.Define { Statement.Define.name; _ }; _ }] ->
       assert_equal name (Expression.Access.create_from_identifiers [~~"a"; ~~"foo"])
   | _ -> assert_unreached ())


let test_parse_sources_coverage _ =
  let (_, (strict_coverage, declare_coverage)) =
    ParseService.parse_sources_list
      (Service.mock ())
      [
        File.create
          ~content:(Some "#pyre-strict\ndef foo()->int:\n    return 1\n")
          (Path.create_relative ~root:(Path.current_working_directory ()) ~relative:"a.py");
        File.create
          ~content:(Some "#pyre-strict\ndef foo()->int:\n    return 1\n")
          (Path.create_relative ~root:(Path.current_working_directory ()) ~relative:"b.py");
        File.create
          ~content:(Some "#pyre-declare-but-dont-check\ndef foo()->int:\n    return 1\n")
          (Path.create_relative ~root:(Path.current_working_directory ()) ~relative:"c.py");
      ]
      ~root:(Path.current_working_directory ())
  in
  assert_equal strict_coverage 2;

  assert_equal declare_coverage 1

let () =
  Log.initialize_for_tests ();
  "parser">:::[
    "parse_stubs_list">::test_parse_stubs_list;
    "parse_sources_list">::test_parse_sources_list;
    "parse_sources_coverage">::test_parse_sources_coverage;
  ]
  |> run_test_tt_main
