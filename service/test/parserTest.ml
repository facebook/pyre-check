(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Test
open Ast
open Pyre

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
          {
            Source.statements =
              [ {
                  Node.value =
                    Statement.Define ({ Statement.Define.signature = { name; _ }; _ } as define);
                  _;
                } ];
            _;
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


let test_parse_source context =
  let sources, _ =
    ScratchProject.setup ~context ["x.py", "def foo()->int:\n    return 1\n"]
    |> ScratchProject.parse_sources
  in
  let handles = List.map sources ~f:(fun { Source.relative; _ } -> relative) in
  assert_equal handles ["x.py"];
  let source = Ast.SharedMemory.Sources.get !&"x" in
  assert_equal (Option.is_some source) true;
  let { Source.relative; statements; metadata = { Source.Metadata.number_of_lines; _ }; _ } =
    Option.value_exn source
  in
  assert_equal relative "x.py";
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
      ~target:(Path.absolute link_root ^/ "link.py")
      ~link_name:(Path.absolute local_root ^/ "d.py");
    Unix.symlink
      ~target:(Path.absolute link_root ^/ "seemingly_unrelated.pyi")
      ~link_name:(Path.absolute local_root ^/ "d.pyi");
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~search_path:[SearchPath.Root module_root; SearchPath.Root typeshed_root]
        ~filter_directories:[local_root]
        ()
    in
    let module_tracker = Analysis.ModuleTracker.create configuration in
    let source_paths, _ = Service.Parser.parse_all ~scheduler ~configuration module_tracker in
    List.map source_paths ~f:(fun { SourcePath.relative; _ } -> relative)
    |> List.sort ~compare:String.compare
  in
  assert_equal
    ~cmp:(List.equal String.equal)
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
    let module_tracker = Analysis.ModuleTracker.create configuration in
    let source_paths, _ = Service.Parser.parse_all ~scheduler ~configuration module_tracker in
    List.map source_paths ~f:(fun { SourcePath.relative; _ } -> relative)
  in
  (* Note that the stub gets parsed twice due to appearing both in the local root and stubs, but
     consistently gets mapped to the correct handle. *)
  assert_equal
    ~printer:(String.concat ~sep:", ")
    ~cmp:(fun left_handles right_handles ->
      let left_handles = List.sort ~compare:String.compare left_handles in
      let right_handles = List.sort ~compare:String.compare right_handles in
      List.equal String.equal left_handles right_handles)
    source_handles
    ["stub.pyi"; "a.py"];
  match Ast.SharedMemory.Sources.get (Reference.create "c") with
  | Some { Source.hash; _ } ->
      assert_equal hash ([%hash: string list] (String.split ~on:'\n' content))
  | None -> assert_unreached ()


let test_register_modules context =
  let assert_module_exports raw_source expected_exports =
    let configuration, qualifiers =
      let project =
        ScratchProject.setup
          ~context
          ["testing.py", raw_source; "canary.py", "from .testing import *"]
      in
      let _ = ScratchProject.parse_sources project in
      ScratchProject.configuration_of project, ScratchProject.qualifiers_of project
    in
    (* The modules get removed after preprocessing. *)
    assert_is_none (Ast.SharedMemory.Modules.get ~qualifier:!&"testing");
    Test.populate_shared_memory ~configuration qualifiers;
    let assert_exports ~qualifier =
      let module_definition =
        let module_get =
          let global_resolution =
            Analysis.Environment.resolution (Analysis.Environment.shared_memory_handler ()) ()
          in
          Analysis.GlobalResolution.module_definition global_resolution
        in
        Option.value_exn (module_get qualifier)
      in
      assert_equal
        ~cmp:(List.equal Reference.equal)
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
      |> fun (sources, _) ->
      List.map sources ~f:(fun ({ Source.relative; _ } as source) -> relative, source)
      |> List.sort ~compare:(fun (left_handle, _) (right_handle, _) ->
             String.compare left_handle right_handle)
    in
    let equal (expected_handle, expected_source) (handle, { Ast.Source.statements; _ }) =
      String.equal expected_handle handle
      && Ast.Source.equal expected_source { expected_source with Ast.Source.statements }
    in
    let printer (handle, source) = Format.sprintf "%s: %s" handle (Ast.Source.show source) in
    let expected =
      List.map expected ~f:(fun (handle, parsed_source) -> handle, Test.parse parsed_source)
    in
    assert_equal ~cmp:(List.equal equal) ~printer:(List.to_string ~f:printer) expected actual
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
         "parse_source" >:: test_parse_source;
         "parse_sources" >:: test_parse_sources;
         "register_modules" >:: test_register_modules;
         "parse_repository" >:: test_parse_repository ]
  |> Test.run
