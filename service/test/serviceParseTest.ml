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
    Service.Parser.parse_sources_list
      ~configuration:(Configuration.create ())
      ~scheduler:(Scheduler.mock ())
      ~files
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
  assert_module_matches_name (get_handle_at 6) [~~"2"; ~~"modc"; ~~"f"];
  assert_module_matches_name (get_handle_at 7) [~~"2and3"; ~~"modd"; ~~"f"]


let test_parse_stubs context =
  let handles =
    let source_root = Path.create_absolute (bracket_tmpdir context) in
    let stub_root = Path.create_absolute (bracket_tmpdir context) in

    let write_file root relative =
      File.create ~content:(Some "def foo() -> int: ...") (Path.create_relative ~root ~relative)
      |> File.write
    in
    write_file source_root "a.pyi";
    write_file source_root "d.py";
    write_file stub_root "b.pyi";
    write_file stub_root "c.py";
    write_file source_root "ttypes.py";
    write_file source_root "ttypes.pyi";

    Service.Parser.parse_stubs
      (Scheduler.mock ())
      ~configuration:(Configuration.create ~source_root ~stub_roots:[stub_root] ())
    |> List.map ~f:File.Handle.show
    |> List.sort ~compare:String.compare
  in
  assert_equal
    ~cmp:(List.equal ~equal:String.equal)
    ~printer:(String.concat ~sep:", ")
    ["a.pyi"; "b.pyi"; "c.py"; "ttypes.pyi"]
    handles


let test_parse_sources_list _ =
  let file =
    File.create
      ~content:(Some "def foo()->int:\n    return 1\n")
      (Path.create_relative ~root:(Path.current_working_directory ()) ~relative:"a.py")
  in
  let handles =
    Service.Parser.parse_sources_list
      ~configuration:(Configuration.create ~source_root:(Path.current_working_directory ()) ())
      ~scheduler:(Scheduler.mock ())
      ~files:[file]
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


let test_parse_sources context =
  let stub_handles, source_handles =
    let source_root = Path.create_absolute (bracket_tmpdir context) in
    let stub_root = Path.create_absolute (bracket_tmpdir context) in

    let write_file root relative =
      File.create ~content:(Some "def foo() -> int: ...") (Path.create_relative ~root ~relative)
      |> File.write
    in
    write_file source_root "a.pyi";
    write_file source_root "a.py";

    write_file stub_root "b.pyi";
    write_file source_root "b.py";

    write_file source_root "c.py";

    let configuration = Configuration.create ~source_root ~stub_roots:[stub_root] () in
    let scheduler = Scheduler.mock () in
    let stub_handles =
      Service.Parser.parse_stubs scheduler ~configuration
      |> List.map ~f:File.Handle.show
      |> List.sort ~compare:String.compare
    in
    let open Expression in
    let filter path =
      match Path.get_relative_to_root ~root:source_root ~path:(Path.create_absolute path) with
      | Some path ->
          Set.mem
            (Access.Set.of_list [Access.create "a"; Access.create "b"])
            (Source.qualifier ~path)
          |> not
      | _ ->
          true
    in
    let source_handles =
      Service.Parser.parse_sources scheduler ~filter ~configuration
      |> List.map ~f:File.Handle.show
      |> List.sort ~compare:String.compare
    in
    stub_handles, source_handles
  in
  assert_equal
    ~cmp:(List.equal ~equal:String.equal)
    ~printer:(String.concat ~sep:", ")
    ["a.pyi"; "b.pyi"]
    stub_handles;
  assert_equal
    ~cmp:(List.equal ~equal:String.equal)
    ~printer:(String.concat ~sep:", ")
    ["c.py"]
    source_handles


let () =
  "parser">:::[
    "parse_stubs_modules_list">::test_parse_stubs_modules_list;
    "parse_stubs">::test_parse_stubs;
    "parse_sources">::test_parse_sources;
    "parse_sources_list">::test_parse_sources_list;
  ]
  |> run_test_tt_main
