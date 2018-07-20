(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Service
open Test

open Ast
open Expression

open Pyre


let test_parse_stubs_modules_list _ =
  let files =
    let root = Path.current_working_directory () in
    let create_stub_with_relative relative =
      let content = Some "def f()->int: ...\n" in
      File.create ~content (Path.create_relative ~root ~relative)
    in
    let create_module_with_relative relative =
      let content = Some "def f()->int:\n    return 1\n" in
      File.create ~content (Path.create_relative ~root ~relative)
    in
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
    Service.Parser.parse_sources
      ~configuration:(Configuration.create ())
      ~scheduler:(Scheduler.mock ())
      ~files
  in
  assert_equal (List.length files) (List.length handles);
  let get_handle_at position =
    File.handle (List.nth_exn files position)
    |> Option.value ~default:(File.Handle.create "")
  in
  let assert_stub_matches_name ~handle define =
    let name =
      match AstSharedMemory.get_source (get_handle_at handle) with
      | Some {
          Source.statements = [
            {
              Node.value = Statement.Define ({ Statement.Define.name; _ } as define);
              _;
            };
          ];
          _;
        } when Statement.Define.is_stub define -> name
      | _ -> failwith "Could not get source."
    in
    assert_equal ~cmp:Access.equal ~printer:Access.show (Access.create define) name
  in
  let assert_module_matches_name ~handle define =
    let name =
      match AstSharedMemory.get_source (get_handle_at handle) with
      | Some {
          Source.statements = [
            {
              Node.value = Statement.Define { Statement.Define.name; _ };
              _;
            };
          ];
          _;
        } -> name
      | _ -> failwith "Could not get source."
    in
    assert_equal ~cmp:Access.equal ~printer:Access.show (Access.create define) name
  in
  assert_stub_matches_name ~handle:0 "a.f";
  assert_stub_matches_name ~handle:1 "dir.b.f";
  assert_stub_matches_name ~handle:2 "c.f";
  assert_stub_matches_name ~handle:3 "d.f";
  assert_module_matches_name ~handle:4 "moda.f";
  assert_module_matches_name ~handle:5 "dir.modb.f";
  assert_module_matches_name ~handle:6 "2.modc.f";
  assert_module_matches_name ~handle:7 "2and3.modd.f"


let test_parse_stubs context =
  let handles =
    let source_root = Path.create_absolute (bracket_tmpdir context) in
    let module_root = Path.create_absolute (bracket_tmpdir context) in

    let write_file root relative =
      File.create ~content:(Some "def foo() -> int: ...") (Path.create_relative ~root ~relative)
      |> File.write
    in
    write_file source_root "a.pyi";
    write_file source_root "d.py";
    write_file module_root "b.pyi";
    write_file module_root "c.py";
    write_file source_root "ttypes.py";
    write_file source_root "ttypes.pyi";

    Service.Parser.parse_stubs
      (Scheduler.mock ())
      ~configuration:(Configuration.create ~source_root ~search_path:[module_root] ())
    |> List.map ~f:File.Handle.show
    |> List.sort ~compare:String.compare
  in
  assert_equal
    ~cmp:(List.equal ~equal:String.equal)
    ~printer:(String.concat ~sep:", ")
    ["a.pyi"; "b.pyi"; "c.py"; "ttypes.pyi"]
    handles


let test_parse_sources _ =
  let file =
    File.create
      ~content:(Some "def foo()->int:\n    return 1\n")
      (Path.create_relative ~root:(Path.current_working_directory ()) ~relative:"a.py")
  in
  let handles =
    Service.Parser.parse_sources
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
        assert_equal
          ~cmp:Access.equal
          ~printer:Access.show
          name
          (Access.create_from_identifiers [~~"a"; ~~"foo"])
    | _ -> assert_unreached ()
  end


let test_parse_sources context =
  let stub_handles, source_handles =
    let source_root = Path.create_absolute (bracket_tmpdir context) in
    let module_root = Path.create_absolute (bracket_tmpdir context) in
    let link_root = Path.create_absolute (bracket_tmpdir context) in

    let write_file root relative =
      File.create ~content:(Some "def foo() -> int: ...") (Path.create_relative ~root ~relative)
      |> File.write
    in
    write_file source_root "a.pyi";
    write_file source_root "a.py";

    write_file module_root "b.pyi";
    write_file source_root "b.py";

    write_file source_root "c.py";

    write_file link_root "link.py";
    write_file link_root "seemingly_unrelated.pyi";

    Unix.symlink
      ~src:((Path.absolute link_root) ^/ "link.py")
      ~dst:((Path.absolute source_root) ^/ "d.py");
    Unix.symlink
      ~src:((Path.absolute link_root) ^/ "seemingly_unrelated.pyi")
      ~dst:((Path.absolute source_root) ^/ "d.pyi");

    let configuration = Configuration.create ~source_root ~search_path:[module_root] () in
    let scheduler = Scheduler.mock () in
    let { Service.Parser.stubs; sources } = Service.Parser.parse_all scheduler ~configuration in
    let stubs =
      stubs
      |> List.map ~f:File.Handle.show
      |> List.sort ~compare:String.compare
    in
    let sources =
      sources
      |> List.map ~f:File.Handle.show
      |> List.sort ~compare:String.compare
    in
    stubs, sources
  in
  assert_equal
    ~cmp:(List.equal ~equal:String.equal)
    ~printer:(String.concat ~sep:", ")
    ["a.pyi"; "b.pyi"; "d.pyi"]
    stub_handles;
  assert_equal
    ~cmp:(List.equal ~equal:String.equal)
    ~printer:(String.concat ~sep:", ")
    ["c.py"]
    source_handles


let test_register_modules _ =
  let assert_module_exports raw_source expected_exports =
    let get_qualifier file =
      File.handle file
      >>= AstSharedMemory.get_source
      >>| (fun { Source.qualifier; _ } -> qualifier)
    in
    let file =
      File.create
        ~content:(Some (trim_extra_indentation raw_source))
        (Path.create_relative ~root:(Path.current_working_directory ()) ~relative:"a.py")
    in

    (* Build environment *)
    AstSharedMemory.remove_modules (List.filter_map ~f:get_qualifier [file]);
    AstSharedMemory.remove_paths (List.filter_map ~f:(fun file -> File.handle file) [file]);
    let configuration = Configuration.create ~source_root:(Path.current_working_directory ()) () in
    let sources =
      Service.Parser.parse_sources
        ~configuration
        ~scheduler:(Scheduler.mock ())
        ~files:[file]
    in
    (* Check specific testing file *)
    let qualifier = Option.value_exn (get_qualifier file) in
    (* The modules get removed after preprocessing. *)
    assert_is_none (AstSharedMemory.get_module qualifier);

    Service.Environment.shared_memory_handler ~configuration ~stubs:[] ~sources |> ignore;
    assert_is_some (AstSharedMemory.get_module qualifier);

    assert_equal
      ~cmp:(List.equal ~equal:Access.equal)
      ~printer:(fun expression_list ->
          List.map ~f:(Access.show) expression_list
          |> String.concat ~sep:", ")
      (List.map ~f:Access.create expected_exports)
      (Option.value_exn (AstSharedMemory.get_module_exports qualifier))
  in
  assert_module_exports
    {|
      def foo() -> int:
        return 1
    |}
    ["foo"];
  assert_module_exports
    {|
      from x import y as z
    |}
    ["z"];
  assert_module_exports
    {|
      from a import b
      from x import y as z
      def fuzz() -> int: pass
    |}
    ["b"; "z"; "fuzz"];
  assert_module_exports
    {|
      from a import b
      from x import y as z
      def _private_fuzz() -> int: pass
    |}
    ["b"; "z"];
  assert_module_exports
    {|
      __all__ = ["b"]
      from a import b
      from x import y as z
      def fuzz() -> int: pass
    |}
    ["b"]


let () =
  "parser">:::[
    "parse_stubs_modules_list">::test_parse_stubs_modules_list;
    "parse_stubs">::test_parse_stubs;
    "parse_sources">::test_parse_sources;
    "parse_sources">::test_parse_sources;
    "register_modules">::test_register_modules;
  ]
  |> run_test_tt_main
