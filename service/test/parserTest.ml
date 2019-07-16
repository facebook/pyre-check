(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Test
open Ast
open Pyre
open Path.AppendOperator

let test_parse_stubs_modules_list context =
  let _ =
    let stub_content = "def f()->int: ...\n" in
    let source_content = "def f()->int:\n    return 1\n" in
    ScratchProject.setup
      ~context
      [ "a.pyi", stub_content;
        "dir/b.pyi", stub_content;
        "2/c.pyi", stub_content;
        "2and3/d.pyi", stub_content;
        "moda.py", source_content;
        "dir/modb.py", source_content;
        "2/modc.py", source_content;
        "2and3/modd.py", source_content ]
    |> ScratchProject.parse_sources
  in
  let assert_function_matches_name ~qualifier ?(is_stub = false) define =
    let name =
      match Ast.SharedMemory.Sources.get qualifier with
      | Some
          { Source.statements =
              [ { Node.value =
                    Statement.Define ({ Statement.Define.signature = { name; _ }; _ } as define);
                  _
                } ];
            _
          }
        when Statement.Define.is_stub define = is_stub ->
          name
      | _ -> failwith "Could not get source."
    in
    assert_equal ~cmp:Reference.equal ~printer:Reference.show (Reference.create define) name
  in
  assert_function_matches_name ~qualifier:!&"a" ~is_stub:true "a.f";
  assert_function_matches_name ~qualifier:!&"dir.b" ~is_stub:true "dir.b.f";
  assert_function_matches_name ~qualifier:!&"2.c" ~is_stub:true "2.c.f";
  assert_function_matches_name ~qualifier:!&"2and3.d" ~is_stub:true "2and3.d.f";
  assert_function_matches_name ~qualifier:!&"moda" "moda.f";
  assert_function_matches_name ~qualifier:!&"dir.modb" "dir.modb.f";
  assert_function_matches_name ~qualifier:!&"2.modc" "2.modc.f";
  assert_function_matches_name ~qualifier:!&"2and3.modd" "2and3.modd.f"


let test_find_stubs_and_sources context =
  let assert_sources configuration expected =
    let handles =
      Service.Parser.find_stubs_and_sources configuration
      |> List.map ~f:File.create
      |> List.map ~f:(File.handle ~configuration)
      |> List.map ~f:File.Handle.show
      |> List.sort ~compare:String.compare
    in
    assert_equal
      ~cmp:(List.equal ~equal:String.equal)
      ~printer:(String.concat ~sep:", ")
      (List.sort ~compare:String.compare expected)
      handles
  in
  let local_root = Path.create_absolute (bracket_tmpdir context) in
  let local_subdirectory_root = Path.create_relative ~root:local_root ~relative:"subdirectory" in
  Sys_utils.mkdir_no_fail (Path.absolute local_subdirectory_root);
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
  write_file local_subdirectory_root "stub.pyi";
  write_file local_root "ttypes.py";
  write_file local_root "ttypes.pyi";
  write_file local_root "dir/legit.pyi";
  write_file local_root "excluded.pyi";
  write_file local_root "nested/excluded.pyi";
  write_file (virtual_root ^| "package") "virtual.py";
  write_file (virtual_root ^| "package") "virtual.pyi";
  write_file (virtual_root ^| "external") "other.py";
  let configuration =
    Configuration.Analysis.create
      ~local_root
      ~excludes:["this/matches/nothing"; ".*/dir"; ".*/excluded.pyi"]
      ~search_path:
        [ SearchPath.Root local_subdirectory_root;
          SearchPath.Root module_root;
          SearchPath.Subdirectory { root = virtual_root; subdirectory = "package" } ]
      ()
  in
  assert_sources
    configuration
    ["a.pyi"; "b.pyi"; "c.py"; "d.py"; "package/virtual.pyi"; "stub.pyi"; "ttypes.pyi"];
  let configuration = { configuration with Configuration.Analysis.search_path = [] } in
  assert_sources configuration ["a.pyi"; "d.py"; "subdirectory/stub.pyi"; "ttypes.pyi"]


let test_find_sources context =
  let handles =
    let local_root = Path.create_absolute (bracket_tmpdir context) in
    let module_root = Path.create_absolute (bracket_tmpdir context) in
    let virtual_root = Path.create_absolute (bracket_tmpdir context) in
    Sys_utils.mkdir_no_fail (Path.absolute virtual_root ^ "/package");
    let write_file root relative =
      File.create ~content:"def foo() -> int: ..." (Path.create_relative ~root ~relative)
      |> File.write
    in
    write_file local_root "a.py";
    write_file local_root "b.py";
    write_file local_root "b.pyi";
    write_file module_root "a.pyi";
    write_file local_root "ttypes.py";
    write_file local_root "dir/legit.py";
    write_file local_root "excluded.py";
    write_file local_root "nested/excluded.py";
    write_file local_root "c.cconf";
    write_file local_root "nested/a.cconf";
    write_file local_root "a.jpg";
    write_file local_root "nested/a.jpg";
    write_file local_root "extensionless";
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~excludes:["this/matches/nothing"; ".*/dir"; ".*/excluded.py"]
        ~extensions:[".cconf"; ""]
        ~search_path:[SearchPath.Root module_root]
        ()
    in
    Service.Parser.find_stubs_and_sources configuration
    |> List.map ~f:File.create
    |> List.map ~f:(File.handle ~configuration)
    |> List.map ~f:File.Handle.show
    |> List.sort ~compare:String.compare
  in
  assert_equal
    ~cmp:(List.equal ~equal:String.equal)
    ~printer:(String.concat ~sep:", ")
    ["a.pyi"; "b.pyi"; "c.cconf"; "extensionless"; "nested/a.cconf"; "ttypes.py"]
    handles


let test_external_sources context =
  let local_root, external_root, handles =
    let root = Path.create_absolute (bracket_tmpdir context) in
    let create_path name =
      let path_name = Path.absolute root ^ name in
      Sys_utils.mkdir_no_fail path_name;
      Path.create_absolute path_name
    in
    let local_root = create_path "/local" in
    let external_root = create_path "/external" in
    let write_file root relative =
      File.create ~content:"def foo() -> int: ..." (Path.create_relative ~root ~relative)
      |> File.write
    in
    (* local a.py should be shadowed by external a.py *)
    write_file local_root "a.py";
    write_file local_root "b.py";
    write_file external_root "a.py";
    let configuration =
      Configuration.Analysis.create ~local_root ~search_path:[SearchPath.Root external_root] ()
    in
    ( local_root,
      external_root,
      Service.Parser.find_stubs_and_sources configuration |> List.sort ~compare:Path.compare )
  in
  assert_equal
    ~cmp:(List.equal ~equal:Path.equal)
    ~printer:(fun paths -> String.concat ~sep:", " (List.map paths ~f:Path.absolute))
    [ Path.create_relative ~root:external_root ~relative:"a.py";
      Path.create_relative ~root:local_root ~relative:"b.py" ]
    handles


let test_parse_source context =
  let sources =
    ScratchProject.setup ~context ["x.py", "def foo()->int:\n    return 1\n"]
    |> ScratchProject.parse_sources
  in
  let handles = List.map sources ~f:(fun { Source.handle; _ } -> handle) in
  assert_equal handles [File.Handle.create_for_testing "x.py"];
  let source = Ast.SharedMemory.Sources.get !&"x" in
  assert_equal (Option.is_some source) true;
  let { Source.handle; statements; metadata = { Source.Metadata.number_of_lines; _ }; _ } =
    Option.value_exn source
  in
  assert_equal handle (File.Handle.create_for_testing "x.py");
  assert_equal number_of_lines 2;
  match statements with
  | [{ Node.value = Statement.Define { Statement.Define.signature = { name; _ }; _ }; _ }] ->
      assert_equal ~cmp:Reference.equal ~printer:Reference.show name (Reference.create "x.foo")
  | _ -> assert_unreached ()


let test_parse_sources context =
  let scheduler = Scheduler.mock () in
  let content = "def foo() -> int: ..." in
  let source_handles =
    let local_root = Path.create_absolute (bracket_tmpdir context) in
    let typeshed_root =
      Path.create_relative ~root:local_root ~relative:".pyre/resource_cache/typeshed"
    in
    Sys_utils.mkdir_p (Path.absolute typeshed_root);
    let module_root = Path.create_absolute (bracket_tmpdir context) in
    let link_root = Path.create_absolute (bracket_tmpdir context) in
    let write_file root relative =
      File.create ~content (Path.create_relative ~root ~relative) |> File.write
    in
    write_file local_root "a.pyi";
    write_file local_root "a.py";
    write_file module_root "b.pyi";
    write_file local_root "b.py";
    write_file local_root "c.py";
    write_file local_root ".pyre/resource_cache/typeshed/foo.pyi";
    write_file link_root "link.py";
    write_file link_root "seemingly_unrelated.pyi";
    Unix.symlink
      ~src:(Path.absolute link_root ^/ "link.py")
      ~dst:(Path.absolute local_root ^/ "d.py");
    Unix.symlink
      ~src:(Path.absolute link_root ^/ "seemingly_unrelated.pyi")
      ~dst:(Path.absolute local_root ^/ "d.pyi");
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~search_path:[SearchPath.Root module_root; SearchPath.Root typeshed_root]
        ~filter_directories:[local_root]
        ()
    in
    let module_tracker = Service.ModuleTracker.create configuration in
    Service.Parser.parse_all ~scheduler ~configuration module_tracker
    |> List.map ~f:File.Handle.show
    |> List.sort ~compare:String.compare
  in
  assert_equal
    ~cmp:(List.equal ~equal:String.equal)
    ~printer:(String.concat ~sep:", ")
    ["a.pyi"; "b.pyi"; "c.py"; "d.pyi"; "foo.pyi"]
    source_handles;
  let local_root = Path.create_absolute (bracket_tmpdir context) in
  let stub_root = Path.create_relative ~root:local_root ~relative:"stubs" in
  let source_handles =
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~search_path:[SearchPath.Root stub_root]
        ~filter_directories:[local_root]
        ()
    in
    let write_file root relative =
      File.create ~content:"def foo() -> int: ..." (Path.create_relative ~root ~relative)
      |> File.write
    in
    write_file local_root "a.py";
    write_file stub_root "stub.pyi";
    Ast.SharedMemory.Sources.remove [Reference.create "a"; Reference.create "stub"];
    let module_tracker = Service.ModuleTracker.create configuration in
    Service.Parser.parse_all ~scheduler ~configuration module_tracker
  in
  (* Note that the stub gets parsed twice due to appearing both in the local root and stubs, but
     consistently gets mapped to the correct handle. *)
  assert_equal
    ~printer:(List.to_string ~f:File.Handle.show)
    ~cmp:(fun left_handles right_handles ->
      let left_handles = List.sort ~compare:File.Handle.compare left_handles in
      let right_handles = List.sort ~compare:File.Handle.compare right_handles in
      List.equal ~equal:File.Handle.equal left_handles right_handles)
    source_handles
    [File.Handle.create_for_testing "stub.pyi"; File.Handle.create_for_testing "a.py"];
  match Ast.SharedMemory.Sources.get (Reference.create "c") with
  | Some { Source.hash; _ } ->
      assert_equal hash ([%hash: string list] (String.split ~on:'\n' content))
  | None -> assert_unreached ()


let test_register_modules context =
  let assert_module_exports raw_source expected_exports =
    let configuration, sources =
      let project =
        ScratchProject.setup
          ~context
          ["testing.py", raw_source; "canary.py", "from .testing import *"]
      in
      let _ = ScratchProject.parse_sources project in
      ScratchProject.configuration_of project, ScratchProject.handles_of project
    in
    (* The modules get removed after preprocessing. *)
    assert_is_none (Ast.SharedMemory.Modules.get ~qualifier:!&"testing");
    Test.populate_shared_memory ~configuration ~sources;
    let assert_exports ~qualifier =
      let module_definition =
        Option.value_exn (Service.EnvironmentSharedMemory.Modules.get qualifier)
      in
      assert_equal
        ~cmp:(List.equal ~equal:Reference.equal)
        ~printer:(fun expression_list ->
          List.map ~f:Reference.show expression_list |> String.concat ~sep:", ")
        (List.map ~f:Reference.create expected_exports)
        (Module.wildcard_exports module_definition)
    in
    assert_exports ~qualifier:!&"testing";
    assert_exports ~qualifier:!&"canary"
  in
  assert_module_exports {|
      def foo() -> int:
        return 1
    |} ["foo"];
  assert_module_exports {|
      from x import y as z
    |} ["z"];
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


let test_parse_repository context =
  let assert_repository_parses_to repository ~expected =
    let actual =
      ScratchProject.setup ~context repository
      |> ScratchProject.parse_sources
      |> List.map ~f:(fun ({ Source.handle; _ } as source) -> handle, source)
      |> List.sort ~compare:(fun (left_handle, _) (right_handle, _) ->
             File.Handle.compare left_handle right_handle)
    in
    let equal (expected_handle, expected_source) (handle, { Ast.Source.statements; _ }) =
      File.Handle.equal expected_handle handle
      && Ast.Source.equal expected_source { expected_source with Ast.Source.statements }
    in
    let printer (handle, source) =
      Format.sprintf "%s: %s" (File.Handle.show handle) (Ast.Source.show source)
    in
    let expected =
      List.map expected ~f:(fun (handle, parsed_source) ->
          File.Handle.create_for_testing handle, Test.parse parsed_source)
    in
    assert_equal ~cmp:(List.equal ~equal) ~printer:(List.to_string ~f:printer) expected actual
  in
  assert_repository_parses_to
    ["a.py", "def foo() -> int: ..."]
    ~expected:["a.py", "def a.foo() -> int: ..."];
  assert_repository_parses_to
    ["a.py", "def foo() -> int: ..."; "b.pyi", "from a import *"]
    ~expected:["a.py", "def a.foo() -> int: ..."; "b.pyi", "from a import foo"];
  assert_repository_parses_to
    ["a.py", "def foo() -> int: ..."; "b.pyi", "from a import *"; "c.py", "from b import *"]
    ~expected:
      ["a.py", "def a.foo() -> int: ..."; "b.pyi", "from a import foo"; "c.py", "from b import foo"]


let () =
  "parser"
  >::: [ "parse_stubs_modules_list" >:: test_parse_stubs_modules_list;
         "find_stubs_and_sources" >:: test_find_stubs_and_sources;
         "find_sources" >:: test_find_sources;
         "external_sources" >:: test_external_sources;
         "parse_source" >:: test_parse_source;
         "parse_sources" >:: test_parse_sources;
         "register_modules" >:: test_register_modules;
         "parse_repository" >:: test_parse_repository ]
  |> Test.run
