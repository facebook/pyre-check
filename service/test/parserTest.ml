(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Test
open Ast
open Analysis
open Pyre

let test_parse_stubs_modules_list context =
  let _, ast_environment =
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
      match Analysis.AstEnvironment.get_source ast_environment qualifier with
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
  let sources, ast_environment =
    ScratchProject.setup ~context ["x.py", "def foo()->int:\n    return 1\n"]
    |> ScratchProject.parse_sources
  in
  let handles = List.map sources ~f:(fun { Source.relative; _ } -> relative) in
  assert_equal handles ["x.py"];
  let source = Analysis.AstEnvironment.get_source ast_environment !&"x" in
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
  let source_handles, ast_environment =
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
    let sources, ast_environment =
      Service.Parser.parse_all ~scheduler ~configuration module_tracker
    in
    let sorted_handles =
      List.map sources ~f:(fun { Source.relative; _ } -> relative)
      |> List.sort ~compare:String.compare
    in
    sorted_handles, ast_environment
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
    Analysis.AstEnvironment.remove_sources
      ast_environment
      [Reference.create "a"; Reference.create "stub"];
    let module_tracker = Analysis.ModuleTracker.create configuration in
    let sources, _ = Service.Parser.parse_all ~scheduler ~configuration module_tracker in
    List.map sources ~f:(fun { Source.relative; _ } -> relative)
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
  match Analysis.AstEnvironment.get_source ast_environment (Reference.create "c") with
  | Some { metadata = { Source.Metadata.raw_hash; _ }; _ } ->
      assert_equal raw_hash ([%hash: string list] (String.split ~on:'\n' content))
  | None -> assert_unreached ()


let test_ast_hash _ =
  let assert_hash_changed ~old_source ~new_source ~expected =
    let handle = "test.py" in
    let old_source = Test.parse ~handle old_source in
    let new_source = Test.parse ~handle new_source in
    let actual = not (Int.equal (Source.hash old_source) (Source.hash new_source)) in
    assert_bool "Test if source hash change is expected" (Bool.equal expected actual)
  in
  (* Metadata *)
  assert_hash_changed ~old_source:"# pyre-strict" ~new_source:"# pyre-strict" ~expected:false;
  assert_hash_changed ~old_source:"# pyre-strict" ~new_source:"" ~expected:true;
  assert_hash_changed
    ~old_source:"# pyre-strict"
    ~new_source:"# pyre-declare-but-dont-check"
    ~expected:true;

  (* Assignments. *)
  assert_hash_changed ~old_source:"a = 1" ~new_source:"a = 1" ~expected:false;
  assert_hash_changed ~old_source:"a: int = 1" ~new_source:"a: int = 1" ~expected:false;
  assert_hash_changed ~old_source:"a: str = 1" ~new_source:"a: int = 1" ~expected:true;
  assert_hash_changed ~old_source:"a = 2" ~new_source:"a = 1" ~expected:true;

  (* Defines *)
  assert_hash_changed
    ~old_source:"def foo() -> int: ..."
    ~new_source:"def foo() -> int: ..."
    ~expected:false;
  assert_hash_changed
    ~old_source:"def foo() -> int: ..."
    ~new_source:"def bar() -> int: ..."
    ~expected:true;
  assert_hash_changed
    ~old_source:"def foo(a: int): ..."
    ~new_source:"def foo(a: str): ..."
    ~expected:true;
  assert_hash_changed
    ~old_source:"def foo(a: int = 1): ..."
    ~new_source:"def foo(a: int = 2): ..."
    ~expected:true;
  assert_hash_changed
    ~old_source:"def foo() -> int: ..."
    ~new_source:"def foo() -> str: ..."
    ~expected:true;
  assert_hash_changed
    ~old_source:"def foo(): ..."
    ~new_source:"async def foo(): ..."
    ~expected:true;
  assert_hash_changed
    ~old_source:{|
        @decorator
        def foo(): ...
      |}
    ~new_source:{|
        @other_decorator
        def foo(): ...
      |}
    ~expected:true;
  assert_hash_changed
    ~old_source:{|
    def foo() -> int:
      return 42
    |}
    ~new_source:{|
    def foo() -> int:
        return 42
    |}
    ~expected:false;
  assert_hash_changed
    ~old_source:{|
    def foo() -> int:
      return 42
    |}
    ~new_source:{|
    def foo() -> int:
  
      return 42
    |}
    ~expected:false;
  assert_hash_changed
    ~old_source:{|
    def foo(a: int, b: int) -> int:
      return a + b
    |}
    ~new_source:{|
    def foo(a: int,  b:    int) ->    int:
        return a   +   b
    |}
    ~expected:false;
  assert_hash_changed
    ~old_source:{|
    def foo() -> int:
      return 42
    |}
    ~new_source:{|
    # pyre-ignore
    def foo() -> int:
      return 42
    |}
    ~expected:true;
  assert_hash_changed
    ~old_source:{|
    def foo() -> int:
      return 42
    |}
    ~new_source:{|
    # pyre-ignore-all-errors
    def foo() -> int:
      return 42
    |}
    ~expected:true;
  assert_hash_changed
    ~old_source:{|
    # pyre-ignore
    def foo() -> int:
      return 42
    |}
    ~new_source:{|
    def foo() -> int: # pyre-ignore
      return 42
    |}
    ~expected:true;
  assert_hash_changed
    ~old_source:{|
    # pyre-ignore[42]
    def foo() -> int:
      return 42
    |}
    ~new_source:{|
    # pyre-ignore[43]
    def foo() -> int:
      return 42
    |}
    ~expected:true;
  assert_hash_changed
    ~old_source:{|
    # pyre-fixme
    def foo() -> int:
      return 42
    |}
    ~new_source:{|
    # pyre-ignore
    def foo() -> int:
      return 42
    |}
    ~expected:false;

  (* Classes *)
  assert_hash_changed
    ~old_source:{|
       @decorator
       class B(A):
         attribute: int = 1
     |}
    ~new_source:{|
       @decorator
       class B(A):
         attribute: int = 1
     |}
    ~expected:false;
  assert_hash_changed ~old_source:"class A: ..." ~new_source:"class B: ..." ~expected:true;
  assert_hash_changed ~old_source:"class A(B): ..." ~new_source:"class A(C): ..." ~expected:true;
  assert_hash_changed
    ~old_source:{|
       class A:
         attribute: int = 1
     |}
    ~new_source:{|
       class A:
         attribute: str = 1
     |}
    ~expected:true;
  assert_hash_changed
    ~old_source:{|
       @decorator
       class A: ...
     |}
    ~new_source:{|
       @other_decorator
       class A: ...
     |}
    ~expected:true;

  (* If *)
  assert_hash_changed
    ~old_source:
      {|
        if test:
          attribute = 1
        else:
          attribute = 2
      |}
    ~new_source:
      {|
        if test:
          attribute = 1
        else:
          attribute = 2
      |}
    ~expected:false;
  assert_hash_changed
    ~old_source:{|
        if test:
          attribute = 1
      |}
    ~new_source:{|
        if other_test:
          attribute = 1
      |}
    ~expected:true;
  assert_hash_changed
    ~old_source:{|
        if test:
          attribute = 1
      |}
    ~new_source:{|
        if test:
          attribute = 2
      |}
    ~expected:true;
  assert_hash_changed
    ~old_source:
      {|
        if test:
          attribute = 1
        else:
          attribute = 2
      |}
    ~new_source:
      {|
        if test:
          attribute = 1
        else:
          attribute = 3
      |}
    ~expected:true;

  (* Imports *)
  assert_hash_changed ~old_source:"from a import b" ~new_source:"from a import b" ~expected:false;
  assert_hash_changed ~old_source:"import a" ~new_source:"import a" ~expected:false;
  assert_hash_changed ~old_source:"from a import b" ~new_source:"from a import c" ~expected:true;
  assert_hash_changed ~old_source:"import a" ~new_source:"import b" ~expected:true;

  (* With *)
  assert_hash_changed
    ~old_source:{|
        with resource:
          attribute = 1
      |}
    ~new_source:{|
        with resource:
          attribute = 1
      |}
    ~expected:false;
  assert_hash_changed
    ~old_source:{|
        with resource:
          attribute = 1
      |}
    ~new_source:{|
        with resource:
          attribute = 2
      |}
    ~expected:true;

  (* TODO: This should be false. It seems that docstrings are still counted as part of the function
     body *)
  assert_hash_changed
    ~old_source:
      {|
    def foo() -> int:
      '''Lets just wait it out. You know, we could be all poetic and lose our minds together.'''
      return 42
    |}
    ~new_source:
      {|
    def foo() -> int:
      '''I'm still waiting for my turn.'''
      return 42
    |}
    ~expected:true;
  ()


let test_register_modules context =
  let assert_module_exports raw_source expected_exports =
    let ast_environment =
      let _, ast_environment =
        ScratchProject.setup
          ~context
          ["testing.py", raw_source; "canary.py", "from .testing import *"]
        |> ScratchProject.parse_sources
      in
      Analysis.AstEnvironment.read_only ast_environment
    in
    let assert_exports ~qualifier =
      let actual_exports =
        let exports =
          Analysis.AstEnvironment.ReadOnly.get_wildcard_exports ast_environment qualifier
        in
        Option.value_exn exports
      in
      let expected_exports =
        List.map ~f:Reference.create expected_exports |> List.sort ~compare:Reference.compare
      in
      let actual_exports = List.sort ~compare:Reference.compare actual_exports in
      assert_equal
        ~cmp:(List.equal Reference.equal)
        ~printer:(List.to_string ~f:Reference.show)
        expected_exports
        actual_exports
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


module IncrementalTest = struct
  type t = {
    handle: string;
    old_source: string option;
    new_source: string option;
  }

  module Expectation = struct
    type t = {
      dependencies: Reference.t list;
      check_exports: (Reference.t * Reference.t list) list;
    }

    let create ?(check_exports = []) dependencies = { dependencies; check_exports }
  end

  let assert_parser_update
      ?(external_setups = [])
      ~context
      ~expected:{ Expectation.dependencies = expected_dependencies; check_exports }
      setups
    =
    let get_old_inputs setups =
      List.filter_map setups ~f:(fun { handle; old_source; _ } ->
          old_source >>| fun source -> handle, source)
    in
    let update_filesystem_state { Configuration.Analysis.local_root; search_path; _ } =
      let update_file ~root { handle; old_source; new_source } =
        let path = Path.create_relative ~root ~relative:handle in
        match old_source, new_source with
        | Some old_source, Some new_source
          when String.equal (trim_extra_indentation old_source) (trim_extra_indentation new_source)
          ->
            (* File content did not change *)
            None
        | _, Some source ->
            (* A file is added/updated *)
            File.create path ~content:(trim_extra_indentation source) |> File.write;
            Some path
        | Some _, None ->
            (* A file is removed *)
            Path.remove path;
            Some path
        | _, _ -> None
      in
      let paths = List.filter_map setups ~f:(update_file ~root:local_root) in
      let external_paths =
        let external_root = List.hd_exn search_path |> SearchPath.get_root in
        List.filter_map external_setups ~f:(update_file ~root:external_root)
      in
      List.append external_paths paths
    in
    (* Set up the initial project *)
    let old_external_sources = get_old_inputs external_setups in
    let old_sources = get_old_inputs setups in
    let configuration, module_tracker, ast_environment =
      let ({ ScratchProject.configuration; module_tracker; _ } as project) =
        ScratchProject.setup ~context ~external_sources:old_external_sources old_sources
      in
      let _, ast_environment = ScratchProject.parse_sources project in
      configuration, module_tracker, ast_environment
    in
    (* Update filesystem *)
    let paths = update_filesystem_state configuration in
    (* Compute the dependencies *)
    let module_tracker_updates = ModuleTracker.update ~configuration ~paths module_tracker in
    let updated_qualifiers =
      Service.Parser.update
        ~configuration
        ~scheduler:(Scheduler.mock ())
        ~ast_environment
        module_tracker_updates
    in
    (* Check dependency expectations *)
    let assert_parser_dependency expected actual =
      let expected_set = Reference.Set.of_list expected in
      let actual_set = Reference.Set.of_list actual in
      assert_bool
        "Check if the actual parser dependency overapproximates the expected one"
        (Set.is_subset expected_set ~of_:actual_set)
    in
    assert_parser_dependency expected_dependencies updated_qualifiers;

    (* Check export expectations *)
    let assert_new_exports ~expected qualifier =
      let expected = List.sort ~compare:Reference.compare expected in
      let actual =
        AstEnvironment.get_wildcard_exports ast_environment qualifier |> Option.value ~default:[]
      in
      assert_equal
        ~cmp:(List.equal Reference.equal)
        ~printer:(List.to_string ~f:Reference.show)
        expected
        actual
    in
    List.iter check_exports ~f:(fun (qualifier, expected) ->
        assert_new_exports ~expected qualifier);
    Memory.reset_shared_memory ()
end

let test_parser_update context =
  (* TODO (T47159596): Automatic shared memory reset for ScratchProject *)
  Memory.reset_shared_memory ();

  let open IncrementalTest in
  let assert_parser_update = assert_parser_update ~context in
  (* Single project file update *)
  assert_parser_update
    [{ handle = "test.py"; old_source = None; new_source = Some "def foo() -> None: ..." }]
    ~expected:(Expectation.create [!&"test"] ~check_exports:[!&"test", [!&"foo"]]);

  assert_parser_update
    [{ handle = "test.py"; old_source = Some "def foo() -> None: ..."; new_source = None }]
    ~expected:(Expectation.create [!&"test"] ~check_exports:[!&"test", []]);
  assert_parser_update
    [ {
        handle = "test.py";
        old_source = Some "def foo() -> None: ...";
        (* Intentionally invalid syntax *)
        new_source = Some "def foo() -> None";
      } ]
    ~expected:(Expectation.create [!&"test"] ~check_exports:[!&"test", []]);
  assert_parser_update
    [ {
        handle = "test.py";
        old_source = Some "def foo() -> None: ...";
        new_source = Some "def foo() -> None: ...";
      } ]
    ~expected:(Expectation.create [] ~check_exports:[!&"test", [!&"foo"]]);
  assert_parser_update
    [ {
        handle = "test.py";
        old_source = Some "def foo(x: int) -> None: ...";
        new_source = Some "def foo   (x  :    int)  ->    None: ...";
      } ]
    ~expected:(Expectation.create [] ~check_exports:[!&"test", [!&"foo"]]);
  assert_parser_update
    [ {
        handle = "test.py";
        old_source = Some "def foo() -> None: ...";
        new_source = Some "def foo(x: int) -> int: ...";
      } ]
    ~expected:(Expectation.create [!&"test"] ~check_exports:[!&"test", [!&"foo"]]);

  (* Single external file update *)
  assert_parser_update
    ~external_setups:
      [{ handle = "test.pyi"; old_source = None; new_source = Some "def foo() -> None: ..." }]
    []
    ~expected:(Expectation.create [!&"test"] ~check_exports:[!&"test", [!&"foo"]]);
  assert_parser_update
    ~external_setups:
      [{ handle = "test.pyi"; old_source = Some "def foo() -> None: ..."; new_source = None }]
    []
    ~expected:(Expectation.create [!&"test"] ~check_exports:[!&"test", []]);
  assert_parser_update
    ~external_setups:
      [ {
          handle = "test.pyi";
          old_source = Some "def foo() -> None: ...";
          new_source = Some "def foo() -> None: ...";
        } ]
    []
    ~expected:(Expectation.create [] ~check_exports:[!&"test", [!&"foo"]]);
  assert_parser_update
    ~external_setups:
      [ {
          handle = "test.pyi";
          old_source = Some "def foo() -> None: ...";
          new_source = Some "def foo(x: int) -> int: ...";
        } ]
    []
    ~expected:(Expectation.create [!&"test"] ~check_exports:[!&"test", [!&"foo"]]);

  (* Multi-file updates *)
  assert_parser_update
    [ { handle = "a.py"; old_source = None; new_source = Some "def foo() -> None: ..." };
      { handle = "b.py"; old_source = None; new_source = Some "def bar() -> None: ..." } ]
    ~expected:
      (Expectation.create [!&"a"; !&"b"] ~check_exports:[!&"a", [!&"foo"]; !&"b", [!&"bar"]]);
  assert_parser_update
    [ { handle = "a.py"; old_source = Some "def foo() -> None: ..."; new_source = None };
      { handle = "b.py"; old_source = Some "def bar() -> None: ..."; new_source = None } ]
    ~expected:(Expectation.create [!&"a"; !&"b"] ~check_exports:[!&"a", []; !&"b", []]);
  assert_parser_update
    [ { handle = "a.py"; old_source = Some "def foo() -> None: ..."; new_source = None };
      { handle = "b.py"; old_source = None; new_source = Some "def bar() -> None: ..." } ]
    ~expected:(Expectation.create [!&"a"; !&"b"] ~check_exports:[!&"a", []; !&"b", [!&"bar"]]);
  assert_parser_update
    [ { handle = "a.py"; old_source = None; new_source = Some "def foo() -> None: ..." };
      {
        handle = "b.py";
        old_source = Some "def bar() -> None: ...";
        new_source = Some "def bar() -> None: ...";
      } ]
    ~expected:(Expectation.create [!&"a"] ~check_exports:[!&"a", [!&"foo"]; !&"b", [!&"bar"]]);
  assert_parser_update
    [ {
        handle = "a.py";
        old_source = Some "def foo() -> None: ...";
        new_source = Some "def foo() -> None: ...";
      };
      { handle = "b.py"; old_source = Some "def bar() -> None: ..."; new_source = None } ]
    ~expected:(Expectation.create [!&"b"] ~check_exports:[!&"a", [!&"foo"]; !&"b", []]);
  assert_parser_update
    ~external_setups:
      [ {
          handle = "a.py";
          old_source = Some "def foo() -> None: ...";
          new_source = Some "def foo(x: int) -> int: ...";
        } ]
    [ {
        handle = "b.py";
        old_source = Some "def bar() -> None: ...";
        new_source = Some "def bar(x: str) -> str: ...";
      } ]
    ~expected:
      (Expectation.create [!&"a"; !&"b"] ~check_exports:[!&"a", [!&"foo"]; !&"b", [!&"bar"]]);

  (* Wildcard export tests *)
  assert_parser_update
    ~external_setups:[{ handle = "a.py"; old_source = Some "x = 1"; new_source = Some "x = 1" }]
    [{ handle = "b.py"; old_source = Some "from a import *"; new_source = Some "from a import *" }]
    ~expected:(Expectation.create [] ~check_exports:[!&"a", [!&"x"]; !&"b", [!&"x"]]);
  assert_parser_update
    ~external_setups:[{ handle = "a.py"; old_source = Some "x = 1"; new_source = Some "x = 2" }]
    [{ handle = "b.py"; old_source = Some "from a import *"; new_source = Some "from a import *" }]
    ~expected:(Expectation.create [!&"a"] ~check_exports:[!&"a", [!&"x"]; !&"b", [!&"x"]]);
  assert_parser_update
    ~external_setups:
      [{ handle = "a.py"; old_source = Some "x = 1"; new_source = Some "x = 1\ny = 2" }]
    [{ handle = "b.py"; old_source = Some "from a import *"; new_source = Some "from a import *" }]
    ~expected:
      (Expectation.create
         [!&"a"; !&"b"]
         ~check_exports:[!&"a", [!&"x"; !&"y"]; !&"b", [!&"x"; !&"y"]]);
  assert_parser_update
    ~external_setups:
      [{ handle = "a.py"; old_source = Some "x = 1\ny = 2\n"; new_source = Some "x = 1" }]
    [{ handle = "b.py"; old_source = Some "from a import *"; new_source = Some "from a import *" }]
    ~expected:(Expectation.create [!&"a"; !&"b"] ~check_exports:[!&"a", [!&"x"]; !&"b", [!&"x"]]);
  assert_parser_update
    ~external_setups:
      [{ handle = "a.py"; old_source = Some "x = 1"; new_source = Some "def foo() -> int: ..." }]
    [{ handle = "b.py"; old_source = Some "from a import *"; new_source = Some "from a import *" }]
    ~expected:
      (Expectation.create [!&"a"; !&"b"] ~check_exports:[!&"a", [!&"foo"]; !&"b", [!&"foo"]]);
  assert_parser_update
    [ { handle = "a.py"; old_source = Some "x = 1"; new_source = Some "x = 2" };
      { handle = "b.py"; old_source = Some "y = 1"; new_source = Some "y = 2" };
      {
        handle = "c.py";
        old_source = Some "from a import *\nfrom b import *";
        new_source = Some "from a import *\nfrom b import *";
      } ]
    ~expected:
      (Expectation.create
         [!&"a"; !&"b"]
         ~check_exports:[!&"a", [!&"x"]; !&"b", [!&"y"]; !&"c", [!&"x"; !&"y"]]);
  assert_parser_update
    [ { handle = "a.py"; old_source = Some "x = 1"; new_source = Some "y = 2" };
      { handle = "b.py"; old_source = Some "y = 1"; new_source = Some "y = 1" };
      {
        handle = "c.py";
        old_source = Some "from a import *\nfrom b import *";
        new_source = Some "from a import *\nfrom b import *";
      } ]
    ~expected:
      (Expectation.create
         [!&"a"; !&"c"]
         ~check_exports:[!&"a", [!&"y"]; !&"b", [!&"y"]; !&"c", [!&"y"]]);
  assert_parser_update
    [ { handle = "a.py"; old_source = Some "x = 1"; new_source = Some "x = 1" };
      { handle = "b.py"; old_source = Some "y = 1"; new_source = Some "x = 2" };
      {
        handle = "c.py";
        old_source = Some "from a import *\nfrom b import *";
        new_source = Some "from a import *\nfrom b import *";
      } ]
    ~expected:
      (Expectation.create
         [!&"b"; !&"c"]
         ~check_exports:[!&"a", [!&"x"]; !&"b", [!&"x"]; !&"c", [!&"x"]]);
  assert_parser_update
    [ { handle = "a.py"; old_source = Some "x = 1"; new_source = Some "x = 1" };
      { handle = "b.py"; old_source = Some "from a import *"; new_source = Some "from a import *" };
      { handle = "c.py"; old_source = Some "from b import *"; new_source = Some "from b import *" }
    ]
    ~expected:
      (Expectation.create [] ~check_exports:[!&"a", [!&"x"]; !&"b", [!&"x"]; !&"c", [!&"x"]]);
  assert_parser_update
    [ { handle = "a.py"; old_source = Some "x = 1"; new_source = Some "x = 2" };
      { handle = "b.py"; old_source = Some "from a import *"; new_source = Some "from a import *" };
      { handle = "c.py"; old_source = Some "from b import *"; new_source = Some "from b import *" }
    ]
    ~expected:
      (Expectation.create [!&"a"] ~check_exports:[!&"a", [!&"x"]; !&"b", [!&"x"]; !&"c", [!&"x"]]);
  assert_parser_update
    [ { handle = "a.py"; old_source = Some "x = 1"; new_source = Some "y = 1" };
      { handle = "b.py"; old_source = Some "from a import *"; new_source = Some "from a import *" };
      { handle = "c.py"; old_source = Some "from b import *"; new_source = Some "from b import *" }
    ]
    ~expected:
      (Expectation.create
         [!&"a"; !&"b"; !&"c"]
         ~check_exports:[!&"a", [!&"y"]; !&"b", [!&"y"]; !&"c", [!&"y"]]);

  (* This is expected -- the parser knows only names, not types *)
  assert_parser_update
    ~external_setups:
      [{ handle = "a.py"; old_source = Some "x = 1"; new_source = Some "def x() -> None: ..." }]
    [{ handle = "b.py"; old_source = Some "from a import *"; new_source = Some "from a import *" }]
    ~expected:(Expectation.create [!&"a"]);
  ()


let () =
  "parser"
  >::: [ "parse_stubs_modules_list" >:: test_parse_stubs_modules_list;
         "parse_source" >:: test_parse_source;
         "parse_sources" >:: test_parse_sources;
         "ast_hash" >:: test_ast_hash;
         "register_modules" >:: test_register_modules;
         "parse_repository" >:: test_parse_repository;
         "parser_update" >:: test_parser_update ]
  |> Test.run
