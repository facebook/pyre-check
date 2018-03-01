(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Service
open Test

open Ast

open Pyre


let test_parse_stubs_modules_list _ =
  let root = Path.current_working_directory () in
  let create_stub_with_relative relative =
    let content = Some "def f()->int: ...\n" in
    File.create ~content (Path.create_relative ~root ~relative)
  in
  let create_module_with_relative relative =
    let content = Some "def f()->int:\n    return 1\n" in
    File.create ~content (Path.create_relative ~root ~relative)
  in
  let files =
    [
      create_stub_with_relative "a.pyi";
      create_stub_with_relative "dir/b.pyi";
      create_stub_with_relative "2/c.pyi";
      create_stub_with_relative "2and3/d.pyi";
      create_module_with_relative "moda.py";
      create_module_with_relative "dir/modb.py";
      create_module_with_relative "2/modc.py";
      create_module_with_relative "2and3/modd.py";
    ]
  in
  let handles =
    Service.Parser.parse_stubs_list
      (Scheduler.mock ())
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
    begin
      match statements with
      | [{
          Node.value = Statement.Stub (Statement.Stub.Define { Statement.Define.name; _ });
          _;
        }] ->
          assert_equal name (Expression.Access.create_from_identifiers define_name)
      | _ -> assert_unreached ()
    end
  in
  let assert_module_matches_name handle define_name =
    let source = AstSharedMemory.get_source handle in
    assert_is_some source;
    let { Source.statements; _ } = Option.value_exn source in
    begin
      match statements with
      | [{
          Node.value = Statement.Define { Statement.Define.name; _ };
          _;
        }] ->
          assert_equal name (Expression.Access.create_from_identifiers define_name)
      | _ -> assert_unreached ()
    end
  in
  assert_stub_matches_name (get_handle_at 0) [~~"a"; ~~"f"];
  assert_stub_matches_name (get_handle_at 1) [~~"dir"; ~~"b"; ~~"f"];
  assert_stub_matches_name (get_handle_at 2) [~~"c"; ~~"f"];
  assert_stub_matches_name (get_handle_at 3) [~~"d"; ~~"f"];
  assert_module_matches_name (get_handle_at 4) [~~"moda"; ~~"f"];
  assert_module_matches_name (get_handle_at 5) [~~"dir"; ~~"modb"; ~~"f"];
  assert_module_matches_name (get_handle_at 6) [~~"modc"; ~~"f"];
  assert_module_matches_name (get_handle_at 7) [~~"modd"; ~~"f"]


let test_parse_stubs context =
  let write_stub root relative =
    let content = Some "def f()->int: ...\n" in
    let file = File.create ~content (Path.create_relative ~root ~relative) in
    File.write file;
    file
  in
  let write_module root relative =
    let content = Some "def f()->int:\n    return 1\n" in
    let file = File.create ~content (Path.create_relative ~root ~relative) in
    File.write file;
    file
  in

  (* top-level directories *)
  let source_root = Path.create_absolute (bracket_tmpdir context) in
  let stub_root = Path.create_absolute (bracket_tmpdir context) in

  (* stubs match in all directories, while modules will match in stub roots only *)
  let matching =
    [
      write_stub source_root "a.pyi";
      write_stub stub_root "b.pyi";
      write_module stub_root "c.py";
    ]
  in
  (* modules should not match in source_root *)
  let _not_matching =
    [
      write_module source_root "d.py";
    ]
  in

  let handles =
    Service.Parser.parse_stubs
      (Scheduler.mock ())
      ~configuration:
        (Configuration.create
           ~source_root
           ~stub_roots:[stub_root]
           ()
        )
  in

  assert_equal (List.length matching) (List.length handles)


let test_parse_sources_list _ =
  let file =
    File.create
      ~content:(Some "def foo()->int:\n    return 1\n")
      (Path.create_relative ~root:(Path.current_working_directory ()) ~relative:"a.py")
  in
  let (handles, _) =
    Service.Parser.parse_sources_list
      (Scheduler.mock ())
      [file]
      ~configuration:(Configuration.create ~source_root:(Path.current_working_directory ()) ())
  in
  let handle = Option.value_exn (File.handle file) in
  assert_equal handles [handle];

  let source = AstSharedMemory.get_source handle in
  assert_equal (Option.is_some source) true;

  let { Source.path; statements; metadata = { Source.Metadata.number_of_lines; _ }; _ } =
    Option.value_exn source
  in
  assert_equal path "a.py";
  assert_equal number_of_lines 3;
  begin
    match statements with
    | [{ Node.value = Statement.Define { Statement.Define.name; _ }; _ }] ->
        assert_equal name (Expression.Access.create_from_identifiers [~~"a"; ~~"foo"])
    | _ -> assert_unreached ()
  end


let test_parse_sources_coverage _ =
  let (_, (strict_coverage, declare_coverage)) =
    Service.Parser.parse_sources_list
      (Scheduler.mock ())
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
      ~configuration:(Configuration.create ~source_root:(Path.current_working_directory ()) ())
  in
  assert_equal strict_coverage 2;

  assert_equal declare_coverage 1

let () =
  "parser">:::[
    "parse_stubs_modules_list">::test_parse_stubs_modules_list;
    "parse_stubs">::test_parse_stubs;
    "parse_sources_list">::test_parse_sources_list;
    "parse_sources_coverage">::test_parse_sources_coverage;
  ]
  |> run_test_tt_main
