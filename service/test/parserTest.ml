(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Test

open Ast
open Expression

open Pyre
open Path.AppendOperator


let test_parse_stubs_modules_list _ =
  let root = Path.current_working_directory () in
  let configuration = Configuration.Analysis.create ~local_root:root () in
  let files =
    let create_stub_with_relative relative =
      File.create ~content:"def f()->int: ...\n" (Path.create_relative ~root ~relative)
    in
    let create_module_with_relative relative =
      File.create ~content:"def f()->int:\n    return 1\n" (Path.create_relative ~root ~relative)
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
  let { Service.Parser.parsed = handles; _ } =
    Service.Parser.parse_sources
      ~configuration:(Configuration.Analysis.create ())
      ~scheduler:(Scheduler.mock ())
      ~files
  in
  assert_equal (List.length files) (List.length handles);
  let get_handle_at position =
    File.handle ~configuration (List.nth_exn files position)
  in
  let assert_stub_matches_name ~handle define =
    let name =
      match Ast.SharedMemory.Sources.get (get_handle_at handle) with
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
      match Ast.SharedMemory.Sources.get (get_handle_at handle) with
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


let test_find_stubs context =
  let handles =
    let local_root = Path.create_absolute (bracket_tmpdir context) in
    let module_root = Path.create_absolute (bracket_tmpdir context) in
    let virtual_root = Path.create_absolute (bracket_tmpdir context) in
    Sys_utils.mkdir_no_fail (Path.absolute virtual_root ^ "/package");
    let write_file root relative =
      File.create ~content:"def foo() -> int: ..." (Path.create_relative ~root ~relative)
      |> File.write
    in
    write_file local_root "a.pyi";
    write_file local_root "d.py";
    write_file module_root "a.pyi";
    write_file module_root "b.pyi";
    write_file module_root "c.py";

    write_file local_root "ttypes.py";
    write_file local_root "ttypes.pyi";
    write_file local_root "dir/legit.pyi";
    write_file local_root "excluded.pyi";
    write_file local_root "nested/excluded.pyi";

    write_file (virtual_root ^| "package") "virtual.py";
    write_file (virtual_root ^| "external") "other.py";

    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~excludes:["this/matches/nothing"; ".*/dir"; ".*/excluded.pyi"]
        ~search_path:[
          Path.SearchPath.Root module_root;
          Path.SearchPath.Subdirectory { root = virtual_root; subdirectory = "package" };
        ]
        ()
    in
    Service.Parser.find_stubs
      ~configuration
    |> List.map ~f:File.create
    |> List.map ~f:(File.handle ~configuration)
    |> List.map ~f:File.Handle.show
    |> List.sort ~compare:String.compare
  in
  assert_equal
    ~cmp:(List.equal ~equal:String.equal)
    ~printer:(String.concat ~sep:", ")
    ["a.pyi"; "b.pyi"; "c.py"; "package/virtual.py"; "ttypes.pyi"]
    handles


let test_parse_typeshed context =
  let typeshed_root = Path.create_absolute (bracket_tmpdir context) in
  let handles =
    let local_root = Path.create_absolute (bracket_tmpdir context) in
    let write_file root relative =
      File.create ~content:"def foo() -> int: ..." (Path.create_relative ~root ~relative)
      |> File.write
    in
    write_file typeshed_root "stdlib/a.pyi";
    write_file typeshed_root "stdlib/b.pyi";
    write_file typeshed_root "third_party/c.pyi";
    write_file typeshed_root "tests/d.pyi";
    write_file typeshed_root ".skipme/e.pyi";

    Service.Parser.find_stubs
      ~configuration:(Configuration.Analysis.create ~local_root ~typeshed:typeshed_root ())
    |> List.filter_map ~f:Path.relative
    |> List.sort ~compare:String.compare
  in
  assert_equal
    ~cmp:(List.equal ~equal:String.equal)
    ~printer:(String.concat ~sep:", ")
    ["a.pyi"; "b.pyi"; "c.pyi"]
    handles


let test_parse_source _ =
  let root = Path.current_working_directory () in
  let configuration = Configuration.Analysis.create ~local_root:root () in
  let file =
    File.create
      ~content:"def foo()->int:\n    return 1\n"
      (Path.create_relative ~root ~relative:"a.py")
  in
  let { Service.Parser.parsed = handles; _ } =
    Service.Parser.parse_sources
      ~configuration:(
        Configuration.Analysis.create ~local_root:(Path.current_working_directory ()) ())
      ~scheduler:(Scheduler.mock ())
      ~files:[file]
  in
  let handle = File.handle ~configuration file in
  assert_equal handles [handle];

  let source = Ast.SharedMemory.Sources.get handle in
  assert_equal (Option.is_some source) true;

  let { Source.handle; statements; metadata = { Source.Metadata.number_of_lines; _ }; _ } =
    Option.value_exn source
  in
  assert_equal handle (File.Handle.create "a.py");
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
  let scheduler = Scheduler.mock () in
  let content = "def foo() -> int: ..." in
  let stub_handles, source_handles =
    let local_root = Path.create_absolute (bracket_tmpdir context) in
    let module_root = Path.create_absolute (bracket_tmpdir context) in
    let link_root = Path.create_absolute (bracket_tmpdir context) in

    let write_file root relative =
      File.create ~content (Path.create_relative ~root ~relative)
      |> File.write
    in
    write_file local_root "a.pyi";
    write_file local_root "a.py";

    write_file module_root "b.pyi";
    write_file local_root "b.py";

    write_file local_root "c.py";
    write_file local_root ".pyre/resource_cache/typeshed/ignoreme.py";

    write_file link_root "link.py";
    write_file link_root "seemingly_unrelated.pyi";

    Unix.symlink
      ~src:((Path.absolute link_root) ^/ "link.py")
      ~dst:((Path.absolute local_root) ^/ "d.py");
    Unix.symlink
      ~src:((Path.absolute link_root) ^/ "seemingly_unrelated.pyi")
      ~dst:((Path.absolute local_root) ^/ "d.pyi");

    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~search_path:[Path.SearchPath.Root module_root]
        ()
    in
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
    source_handles;
  let local_root = Path.create_absolute (bracket_tmpdir context) in
  let stub_root = Path.create_relative ~root:local_root ~relative:"stubs" in
  let stub_handles, source_handles =
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~search_path:[Path.SearchPath.Root stub_root]
        ()
    in
    let write_file root relative =
      File.create ~content:"def foo() -> int: ..." (Path.create_relative ~root ~relative)
      |> File.write
    in
    write_file local_root "a.py";
    write_file stub_root "stub.pyi";
    Ast.SharedMemory.Sources.remove
      ~handles:[File.Handle.create "a.py"; File.Handle.create "stub.pyi"];
    let { Service.Parser.stubs; sources } = Service.Parser.parse_all scheduler ~configuration in
    stubs, sources
  in
  (* Note that the stub gets parsed twice due to appearing both in the local root and stubs, but
     consistently gets mapped to the correct handle. *)
  assert_equal
    ~printer:(List.to_string ~f:File.Handle.show)
    stub_handles
    [File.Handle.create "stub.pyi"];
  assert_equal source_handles [File.Handle.create "a.py"];
  begin
    match Ast.SharedMemory.Sources.get (File.Handle.create "c.py") with
    | Some { Source.hash; _ } ->
        assert_equal hash ([%hash: string list] (String.split ~on:'\n' content))
    | None ->
        assert_unreached ()
  end


let test_register_modules _ =
  let configuration =
    Configuration.Analysis.create ~local_root:(Path.current_working_directory ()) ()
  in
  let assert_module_exports raw_source expected_exports =
    let get_qualifier file =
      File.handle ~configuration file
      |> Ast.SharedMemory.Sources.get
      >>| (fun { Source.qualifier; _ } -> qualifier)
    in
    let file =
      File.create
        ~content:(trim_extra_indentation raw_source)
        (Path.create_relative ~root:(Path.current_working_directory ()) ~relative:"a.py")
    in
    (* Use a "canary" to inspect the exports of a.py between its
       module parsing stage and source parsing stage. *)
    let import_non_toplevel =
      File.create
        ~content:"from .a import *"
        (Path.create_relative ~root:(Path.current_working_directory ()) ~relative:"canary.py")
    in
    let files = [file; import_non_toplevel] in

    (* Build environment. *)
    Ast.SharedMemory.Modules.remove ~qualifiers:(List.filter_map ~f:get_qualifier files);
    Ast.SharedMemory.Sources.remove ~handles:(List.map ~f:(File.handle ~configuration) files);
    let configuration =
      Configuration.Analysis.create ~local_root:(Path.current_working_directory ()) ()
    in
    let { Service.Parser.parsed = sources; _ } =
      Service.Parser.parse_sources
        ~configuration
        ~scheduler:(Scheduler.mock ())
        ~files
    in
    (* Check specific file. *)
    let qualifier = Option.value_exn (get_qualifier file) in
    (* The modules get removed after preprocessing. *)
    assert_is_none (Ast.SharedMemory.Modules.get ~qualifier);
    Service.Environment.populate_shared_memory ~configuration ~stubs:[] ~sources;
    assert_is_some (Ast.SharedMemory.Modules.get ~qualifier);

    let assert_exports ~qualifier =
      assert_equal
        ~cmp:(List.equal ~equal:Access.equal)
        ~printer:(fun expression_list ->
            List.map ~f:(Access.show) expression_list
            |> String.concat ~sep:", ")
        (List.map ~f:Access.create expected_exports)
        (Option.value_exn (Ast.SharedMemory.Modules.get_exports ~qualifier))
    in

    assert_exports ~qualifier;
    assert_exports ~qualifier:(Option.value_exn (get_qualifier import_non_toplevel))
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
    ["b"];
  assert_module_exports
    {|
      if sys.version_info >= (3, 7):
        def foo() -> int: pass
      if sys.platform != "win32":
        def fooz() -> int: pass
    |}
    ["foo"; "fooz"]


let () =
  "parser">:::[
    "parse_stubs_modules_list">::test_parse_stubs_modules_list;
    "find_stubs">::test_find_stubs;
    "parse_typeshed">::test_parse_typeshed;
    "parse_source">::test_parse_source;
    "parse_sources">::test_parse_sources;
    "register_modules">::test_register_modules;
  ]
  |> Test.run
