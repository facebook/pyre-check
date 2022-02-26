(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis
open Test
open OUnit2
open Pyre

let test_basic context =
  let handle_a = "a.py" in
  let source_a = trim_extra_indentation {|
    def foo(x: int) -> int:
      return x
    |} in
  let handle_b = "b.py" in
  let source_b =
    trim_extra_indentation {|
    def bar(y: str) -> str:
      return "hello" + y
    |}
  in
  let handle_c = "c.py" in
  let source_c = trim_extra_indentation {|
      def baz() -> int:
        return 42
      |} in
  let { ScratchProject.configuration; module_tracker; _ } =
    ScratchProject.setup ~context [handle_a, source_a; handle_b, source_b; handle_c, source_c]
  in
  let { Configuration.Analysis.local_root; _ } = configuration in
  let ast_environment = AstEnvironment.create module_tracker in
  let assert_source_path ~ast_environment ~expected reference =
    match
      AstEnvironment.ReadOnly.get_source_path (AstEnvironment.read_only ast_environment) reference
    with
    | None ->
        let message =
          Format.asprintf "Cannot find reference %a in the AST environment" Reference.pp reference
        in
        assert_failure message
    | Some source_path ->
        let actual = SourcePath.full_path ~configuration source_path in
        assert_equal ~cmp:PyrePath.equal ~printer:PyrePath.show expected actual
  in
  assert_source_path
    !&"a"
    ~ast_environment
    ~expected:(PyrePath.create_relative ~root:local_root ~relative:handle_a);
  assert_source_path
    !&"b"
    ~ast_environment
    ~expected:(PyrePath.create_relative ~root:local_root ~relative:handle_b);
  ()


let test_parse_stubs_modules_list context =
  let ast_environment, _ =
    let stub_content = "def f()->int: ...\n" in
    let source_content = "def f()->int:\n    return 1\n" in
    ScratchProject.setup
      ~context
      [
        "a.pyi", stub_content;
        "dir/b.pyi", stub_content;
        "2/c.pyi", stub_content;
        "2and3/d.pyi", stub_content;
        "moda.py", source_content;
        "dir/modb.py", source_content;
        "2/modc.py", source_content;
        "2and3/modd.py", source_content;
      ]
    |> ScratchProject.parse_sources
  in
  let assert_function_matches_name ~qualifier ?(is_stub = false) define =
    let name =
      match
        Analysis.AstEnvironment.ReadOnly.get_processed_source
          (AstEnvironment.read_only ast_environment)
          qualifier
      with
      | Some
          {
            Source.statements =
              [
                {
                  Node.value = Define ({ Statement.Define.signature = { name; _ }; _ } as define);
                  _;
                };
              ];
            _;
          }
        when Bool.equal (Statement.Define.is_stub define) is_stub ->
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
  let ast_environment, ast_environment_update_result =
    ScratchProject.setup
      ~context
      ~include_typeshed_stubs:false
      ["x.py", "def foo()->int:\n    return 1\n"]
    |> ScratchProject.parse_sources
  in
  let sources =
    let ast_environment = Analysis.AstEnvironment.read_only ast_environment in
    AstEnvironment.UpdateResult.invalidated_modules ast_environment_update_result
    |> List.filter_map ~f:(AstEnvironment.ReadOnly.get_processed_source ast_environment)
  in
  let handles =
    List.map sources ~f:(fun { Source.source_path = { SourcePath.relative; _ }; _ } -> relative)
  in
  assert_equal handles ["x.py"];
  let source =
    Analysis.AstEnvironment.ReadOnly.get_processed_source
      (AstEnvironment.read_only ast_environment)
      !&"x"
  in
  assert_equal (Option.is_some source) true;
  let { Source.source_path = { SourcePath.relative; _ }; statements; _ } =
    Option.value_exn source
  in
  assert_equal relative "x.py";
  match statements with
  | [{ Node.value = Define { Statement.Define.signature = { name; _ }; _ }; _ }] ->
      assert_equal ~cmp:Reference.equal ~printer:Reference.show name (Reference.create "x.foo")
  | _ -> assert_unreached ()


let test_parse_sources context =
  let scheduler = Test.mock_scheduler () in
  let content = "def foo() -> int: ..." in
  let source_handles, ast_environment =
    let local_root = PyrePath.create_absolute (bracket_tmpdir context) in
    let typeshed_root =
      PyrePath.create_relative ~root:local_root ~relative:".pyre/resource_cache/typeshed"
    in
    Sys_utils.mkdir_p (PyrePath.absolute typeshed_root);
    let module_root = PyrePath.create_absolute (bracket_tmpdir context) in
    let link_root = PyrePath.create_absolute (bracket_tmpdir context) in
    let write_file root relative =
      File.create ~content (PyrePath.create_relative ~root ~relative) |> File.write
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
      ~target:(PyrePath.absolute link_root ^/ "link.py")
      ~link_name:(PyrePath.absolute local_root ^/ "d.py");
    Unix.symlink
      ~target:(PyrePath.absolute link_root ^/ "seemingly_unrelated.pyi")
      ~link_name:(PyrePath.absolute local_root ^/ "d.pyi");
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~source_paths:[SearchPath.Root local_root]
        ~search_paths:[SearchPath.Root module_root; SearchPath.Root typeshed_root]
        ~filter_directories:[local_root]
        ()
    in
    let module_tracker = Analysis.ModuleTracker.create configuration in
    let ast_environment = Analysis.AstEnvironment.create module_tracker in
    let update_result = AstEnvironment.update ~scheduler ~configuration ast_environment ColdStart in
    let sources =
      AstEnvironment.UpdateResult.invalidated_modules update_result
      |> List.filter_map
           ~f:
             (AstEnvironment.ReadOnly.get_processed_source
                (AstEnvironment.read_only ast_environment))
    in
    let is_source_sorted =
      let compare
          { Source.source_path = { SourcePath.qualifier = left; _ }; _ }
          { Source.source_path = { SourcePath.qualifier = right; _ }; _ }
        =
        Reference.compare left right
      in
      List.is_sorted sources ~compare
    in
    assert_bool "Sources should be in sorted order" is_source_sorted;
    let sorted_handles =
      List.map sources ~f:(fun { Source.source_path = { SourcePath.relative; _ }; _ } -> relative)
      |> List.sort ~compare:String.compare
    in
    sorted_handles, ast_environment
  in
  assert_equal
    ~cmp:(List.equal String.equal)
    ~printer:(String.concat ~sep:", ")
    ["a.pyi"; "b.pyi"; "c.py"; "d.pyi"; "foo.pyi"]
    source_handles;
  let local_root = PyrePath.create_absolute (bracket_tmpdir context) in
  let stub_root = PyrePath.create_relative ~root:local_root ~relative:"stubs" in
  let source_handles =
    let configuration =
      Configuration.Analysis.create
        ~local_root
        ~source_paths:[SearchPath.Root local_root]
        ~search_paths:[SearchPath.Root stub_root]
        ~filter_directories:[local_root]
        ()
    in
    let write_file root relative =
      File.create ~content:"def foo() -> int: ..." (PyrePath.create_relative ~root ~relative)
      |> File.write
    in
    write_file local_root "a.py";
    write_file stub_root "stub.pyi";
    let module_tracker = Analysis.ModuleTracker.create configuration in
    let update_result =
      let { Configuration.Analysis.local_root; _ } = configuration in
      ModuleTracker.update
        ~configuration
        ~paths:
          [
            PyrePath.create_relative ~root:local_root ~relative:"a.py";
            PyrePath.create_relative ~root:stub_root ~relative:"stub.pyi";
          ]
        module_tracker
      |> (fun updates -> AstEnvironment.Update updates)
      |> AstEnvironment.update ~configuration ~scheduler:(mock_scheduler ()) ast_environment
    in
    let sources =
      AstEnvironment.UpdateResult.invalidated_modules update_result
      |> List.filter_map
           ~f:
             (AstEnvironment.ReadOnly.get_processed_source
                (AstEnvironment.read_only ast_environment))
    in
    List.map sources ~f:(fun { Source.source_path = { SourcePath.relative; _ }; _ } -> relative)
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
    ["stub.pyi"; "a.py"]


let test_ast_change _ =
  let assert_ast_changed ~old_source ~new_source ~expected =
    let handle = "test.py" in
    let old_source = Test.parse ~handle old_source in
    let new_source = Test.parse ~handle new_source in
    let compare_changed = not (Int.equal 0 (Source.compare old_source new_source)) in
    let hash_changed =
      let hash_source source = Hash.run Source.hash_fold_t source in
      not (Int.equal (hash_source old_source) (hash_source new_source))
    in
    assert_equal
      ~cmp:Bool.equal
      ~printer:(Format.sprintf "compare_changed = %b")
      expected
      compare_changed;

    (* This is not guaranteed to be the case in theory, but collisions should be rare in practice *)
    assert_equal ~cmp:Bool.equal ~printer:(Format.sprintf "hash changed = %b") expected hash_changed
  in
  (* Metadata *)
  assert_ast_changed ~old_source:"# pyre-strict" ~new_source:"# pyre-strict" ~expected:false;
  assert_ast_changed ~old_source:"# pyre-strict" ~new_source:"" ~expected:true;
  assert_ast_changed
    ~old_source:"# pyre-strict"
    ~new_source:"# pyre-ignore-all-errors"
    ~expected:true;

  (* Assignments. *)
  assert_ast_changed ~old_source:"a = 1" ~new_source:"a = 1" ~expected:false;
  assert_ast_changed ~old_source:"a: int = 1" ~new_source:"a: int = 1" ~expected:false;
  assert_ast_changed ~old_source:"a: str = 1" ~new_source:"a: int = 1" ~expected:true;
  assert_ast_changed ~old_source:"a = 2" ~new_source:"a = 1" ~expected:true;

  (* Defines *)
  assert_ast_changed
    ~old_source:"def foo() -> int: ..."
    ~new_source:"def foo() -> int: ..."
    ~expected:false;
  assert_ast_changed
    ~old_source:"def foo() -> int: ..."
    ~new_source:"def bar() -> int: ..."
    ~expected:true;
  assert_ast_changed
    ~old_source:"def foo(a: int): ..."
    ~new_source:"def foo(a: str): ..."
    ~expected:true;
  assert_ast_changed
    ~old_source:"def foo(a: int = 1): ..."
    ~new_source:"def foo(a: int = 2): ..."
    ~expected:true;
  assert_ast_changed
    ~old_source:"def foo() -> int: ..."
    ~new_source:"def foo() -> str: ..."
    ~expected:true;
  assert_ast_changed ~old_source:"def foo(): ..." ~new_source:"async def foo(): ..." ~expected:true;
  assert_ast_changed
    ~old_source:{|
        @decorator
        def foo(): ...
      |}
    ~new_source:{|
        @other_decorator
        def foo(): ...
      |}
    ~expected:true;
  assert_ast_changed
    ~old_source:{|
    def foo() -> int:
      return 42
    |}
    ~new_source:{|
    def foo() -> int:
      return 42
    |}
    ~expected:false;
  assert_ast_changed
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
  assert_ast_changed
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
  assert_ast_changed
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
  assert_ast_changed
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
  assert_ast_changed
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
    ~expected:true;

  (* Classes *)
  assert_ast_changed
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
  assert_ast_changed ~old_source:"class A: ..." ~new_source:"class B: ..." ~expected:true;
  assert_ast_changed ~old_source:"class A(B): ..." ~new_source:"class A(C): ..." ~expected:true;
  assert_ast_changed
    ~old_source:{|
       class A:
         attribute: int = 1
     |}
    ~new_source:{|
       class A:
         attribute: str = 1
     |}
    ~expected:true;
  assert_ast_changed
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
  assert_ast_changed
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
  assert_ast_changed
    ~old_source:{|
        if test:
          attribute = 1
      |}
    ~new_source:{|
        if other_test:
          attribute = 1
      |}
    ~expected:true;
  assert_ast_changed
    ~old_source:{|
        if test:
          attribute = 1
      |}
    ~new_source:{|
        if test:
          attribute = 2
      |}
    ~expected:true;
  assert_ast_changed
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
  assert_ast_changed ~old_source:"from a import b" ~new_source:"from a import b" ~expected:false;
  assert_ast_changed ~old_source:"import a" ~new_source:"import a" ~expected:false;
  assert_ast_changed ~old_source:"from a import b" ~new_source:"from a import c" ~expected:true;
  assert_ast_changed ~old_source:"import a" ~new_source:"import b" ~expected:true;

  (* With *)
  assert_ast_changed
    ~old_source:{|
        with resource:
          attribute = 1
      |}
    ~new_source:{|
        with resource:
          attribute = 1
      |}
    ~expected:false;
  assert_ast_changed
    ~old_source:{|
        with resource:
          attribute = 1
      |}
    ~new_source:{|
        with resource:
          attribute = 2
      |}
    ~expected:true;

  (* Location-only change *)
  assert_ast_changed
    ~old_source:{|
    def foo() -> int:
      return 42
    |}
    ~new_source:{|
    def foo() -> int:
        return 42
    |}
    ~expected:true;
  assert_ast_changed
    ~old_source:{|
    def foo(a: int, b: int) -> int:
      return a + b
    |}
    ~new_source:{|
    def foo(a: int,  b:    int) ->    int:
        return a   +   b
    |}
    ~expected:true;
  assert_ast_changed
    ~old_source:{|
    def foo() -> int:
      return 42
    |}
    ~new_source:{|
    def foo() -> int:

      return 42
    |}
    ~expected:true;
  assert_ast_changed
    ~old_source:{|
      class Foo:
        x: int = 1
    |}
    ~new_source:{|

      class Foo:
        x: int = 1
    |}
    ~expected:true;

  (* Trailing comment/spaces/empty lines shouldn't matter as they don't affect any AST locations. *)
  (* They may affect line counts and raw hashes, but they are ignored by the
     Source.Metadata.compare/hash *)
  assert_ast_changed
    ~old_source:{|
    def foo() -> int:
      return 42
    |}
    ~new_source:{|
    def foo() -> int:
      return 42  # some comment
    |}
    ~expected:false;
  assert_ast_changed
    ~old_source:{|
      class Foo:
        x: int = 1
    |}
    ~new_source:{|
      class Foo:
        x: int = 1

    |}
    ~expected:false;

  (* TODO: This should be false. It seems that docstrings are still counted as part of the function
     body *)
  assert_ast_changed
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


let test_parse_repository context =
  let assert_repository_parses_to repository ~expected =
    let actual =
      ScratchProject.setup ~context ~include_typeshed_stubs:false repository
      |> ScratchProject.parse_sources
      |> fun (ast_environment, ast_environment_update_result) ->
      let sources =
        let ast_environment = Analysis.AstEnvironment.read_only ast_environment in
        AstEnvironment.UpdateResult.invalidated_modules ast_environment_update_result
        |> List.filter_map ~f:(AstEnvironment.ReadOnly.get_processed_source ast_environment)
      in
      List.map sources ~f:(fun ({ Source.source_path = { SourcePath.relative; _ }; _ } as source) ->
          relative, source)
      |> List.sort ~compare:(fun (left_handle, _) (right_handle, _) ->
             String.compare left_handle right_handle)
    in
    let equal
        (expected_handle, { Ast.Source.statements = expected_source; _ })
        (handle, { Ast.Source.statements; _ })
      =
      let equal left right = Statement.location_insensitive_compare left right = 0 in
      String.equal expected_handle handle && List.equal equal expected_source statements
    in
    let printer (handle, source) = Format.sprintf "%s: %s" handle (Ast.Source.show source) in
    let expected =
      List.map expected ~f:(fun (handle, parsed_source) ->
          handle, Test.parse ~handle parsed_source |> Preprocessing.qualify)
    in
    assert_equal ~cmp:(List.equal equal) ~printer:(List.to_string ~f:printer) expected actual
  in
  assert_repository_parses_to
    ["a.py", "def foo() -> int: ..."]
    ~expected:["a.py", "def foo() -> int: ..."];
  assert_repository_parses_to
    ["a.py", "def foo() -> int: ..."; "b.pyi", "from a import *"]
    ~expected:["a.py", "def foo() -> int: ..."; "b.pyi", "from a import foo as foo"];
  assert_repository_parses_to
    ["a.py", "def foo() -> int: ..."; "b.pyi", "from a import *"; "c.py", "from b import *"]
    ~expected:
      [
        "a.py", "def foo() -> int: ...";
        "b.pyi", "from a import foo as foo";
        "c.py", "from b import foo as foo";
      ];
  (* Unparsable source turns into getattr-any *)
  assert_repository_parses_to
    ["a.py", "def foo() -> int:"]
    ~expected:["a.py", "import typing\ndef __getattr__(name: str) -> typing.Any: ..."];
  ()


module IncrementalTest = struct
  type t = {
    handle: string;
    old_source: string option;
    new_source: string option;
  }

  module Expectation = struct
    type t = { dependencies: Reference.t list }

    let create dependencies = { dependencies }
  end

  let assert_parser_update
      ?(external_setups = [])
      ?(preprocess_all_sources = false)
      ~context
      ~expected:{ Expectation.dependencies = expected_dependencies }
      setups
    =
    let get_old_inputs setups =
      List.filter_map setups ~f:(fun { handle; old_source; _ } ->
          old_source >>| fun source -> handle, source)
    in
    let update_filesystem_state { Configuration.Analysis.local_root; search_paths; _ } =
      let update_file ~root { handle; old_source; new_source } =
        let path = PyrePath.create_relative ~root ~relative:handle in
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
            PyrePath.remove path;
            Some path
        | _, _ -> None
      in
      let paths = List.filter_map setups ~f:(update_file ~root:local_root) in
      let external_paths =
        let external_root = List.hd_exn search_paths |> SearchPath.get_root in
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
      let ast_environment, update_result = ScratchProject.parse_sources project in
      if preprocess_all_sources then
        AstEnvironment.UpdateResult.invalidated_modules update_result
        |> List.iter ~f:(fun qualifier ->
               AstEnvironment.ReadOnly.get_processed_source
                 ~track_dependency:true
                 (AstEnvironment.read_only ast_environment)
                 qualifier
               |> ignore);
      configuration, module_tracker, ast_environment
    in
    (* Update filesystem *)
    let paths = update_filesystem_state configuration in
    (* Compute the dependencies *)
    let module_tracker_updates = ModuleTracker.update ~configuration ~paths module_tracker in
    let update_result =
      AstEnvironment.update
        ~configuration
        ~scheduler:(Test.mock_scheduler ())
        ast_environment
        (Update module_tracker_updates)
    in
    (* Check dependency expectations *)
    let assert_parser_dependency expected actual =
      let expected_set = Reference.Set.of_list expected in
      let actual_set = Reference.Set.of_list actual in
      assert_bool
        "Check if the actual parser dependency overapproximates the expected one"
        (Set.is_subset expected_set ~of_:actual_set)
    in
    AstEnvironment.UpdateResult.invalidated_modules update_result
    |> assert_parser_dependency expected_dependencies;

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
    ~expected:(Expectation.create [!&"test"]);

  assert_parser_update
    [{ handle = "test.py"; old_source = Some "def foo() -> None: ..."; new_source = None }]
    ~expected:(Expectation.create [!&"test"]);
  assert_parser_update
    [
      {
        handle = "test.py";
        old_source = Some "def foo() -> None: ...";
        (* Intentionally invalid syntax *)
        new_source = Some "def foo() -> None";
      };
    ]
    ~expected:(Expectation.create [!&"test"]);
  assert_parser_update
    [
      {
        handle = "test.py";
        old_source = Some "def foo() -> None: ...";
        new_source = Some "def foo() -> None: ...";
      };
    ]
    ~expected:(Expectation.create []);
  assert_parser_update
    [
      {
        handle = "test.py";
        old_source = Some "def foo(x: int) -> None: ...";
        new_source = Some "def foo   (x  :    int)  ->    None: ...";
      };
    ]
    ~expected:(Expectation.create []);
  assert_parser_update
    [
      {
        handle = "test.py";
        old_source = Some "def foo() -> None: ...";
        new_source = Some "def foo(x: int) -> int: ...";
      };
    ]
    ~expected:(Expectation.create [!&"test"]);

  (* Single external file update *)
  assert_parser_update
    ~external_setups:
      [{ handle = "test.pyi"; old_source = None; new_source = Some "def foo() -> None: ..." }]
    []
    ~expected:(Expectation.create [!&"test"]);
  assert_parser_update
    ~external_setups:
      [{ handle = "test.pyi"; old_source = Some "def foo() -> None: ..."; new_source = None }]
    []
    ~expected:(Expectation.create [!&"test"]);
  assert_parser_update
    ~external_setups:
      [
        {
          handle = "test.pyi";
          old_source = Some "def foo() -> None: ...";
          new_source = Some "def foo() -> None: ...";
        };
      ]
    []
    ~expected:(Expectation.create []);
  assert_parser_update
    ~external_setups:
      [
        {
          handle = "test.pyi";
          old_source = Some "def foo() -> None: ...";
          new_source = Some "def foo(x: int) -> int: ...";
        };
      ]
    []
    ~expected:(Expectation.create [!&"test"]);

  (* Multi-file updates *)
  assert_parser_update
    [
      { handle = "a.py"; old_source = None; new_source = Some "def foo() -> None: ..." };
      { handle = "b.py"; old_source = None; new_source = Some "def bar() -> None: ..." };
    ]
    ~expected:(Expectation.create [!&"a"; !&"b"]);
  assert_parser_update
    [
      { handle = "a.py"; old_source = Some "def foo() -> None: ..."; new_source = None };
      { handle = "b.py"; old_source = Some "def bar() -> None: ..."; new_source = None };
    ]
    ~expected:(Expectation.create [!&"a"; !&"b"]);
  assert_parser_update
    [
      { handle = "a.py"; old_source = Some "def foo() -> None: ..."; new_source = None };
      { handle = "b.py"; old_source = None; new_source = Some "def bar() -> None: ..." };
    ]
    ~expected:(Expectation.create [!&"a"; !&"b"]);
  assert_parser_update
    [
      { handle = "a.py"; old_source = None; new_source = Some "def foo() -> None: ..." };
      {
        handle = "b.py";
        old_source = Some "def bar() -> None: ...";
        new_source = Some "def bar() -> None: ...";
      };
    ]
    ~expected:(Expectation.create [!&"a"]);
  assert_parser_update
    [
      {
        handle = "a.py";
        old_source = Some "def foo() -> None: ...";
        new_source = Some "def foo() -> None: ...";
      };
      { handle = "b.py"; old_source = Some "def bar() -> None: ..."; new_source = None };
    ]
    ~expected:(Expectation.create [!&"b"]);
  assert_parser_update
    ~external_setups:
      [
        {
          handle = "a.py";
          old_source = Some "def foo() -> None: ...";
          new_source = Some "def foo(x: int) -> int: ...";
        };
      ]
    [
      {
        handle = "b.py";
        old_source = Some "def bar() -> None: ...";
        new_source = Some "def bar(x: str) -> str: ...";
      };
    ]
    ~expected:(Expectation.create [!&"a"; !&"b"]);

  (* Wildcard export tests *)
  assert_parser_update
    ~preprocess_all_sources:true
    ~external_setups:[{ handle = "a.py"; old_source = Some "x = 1"; new_source = Some "x = 1" }]
    [{ handle = "b.py"; old_source = Some "from a import *"; new_source = Some "from a import *" }]
    ~expected:(Expectation.create []);
  assert_parser_update
    ~preprocess_all_sources:true
    ~external_setups:[{ handle = "a.py"; old_source = Some "x = 1"; new_source = Some "x = 2" }]
    [{ handle = "b.py"; old_source = Some "from a import *"; new_source = Some "from a import *" }]
    ~expected:(Expectation.create [!&"a"]);
  assert_parser_update
    ~preprocess_all_sources:true
    ~external_setups:
      [{ handle = "a.py"; old_source = Some "x = 1"; new_source = Some "x = 1\ny = 2" }]
    [{ handle = "b.py"; old_source = Some "from a import *"; new_source = Some "from a import *" }]
    ~expected:(Expectation.create [!&"a"; !&"b"]);
  assert_parser_update
    ~preprocess_all_sources:true
    ~external_setups:
      [{ handle = "a.py"; old_source = Some "x = 1\ny = 2\n"; new_source = Some "x = 1" }]
    [{ handle = "b.py"; old_source = Some "from a import *"; new_source = Some "from a import *" }]
    ~expected:(Expectation.create [!&"a"; !&"b"]);
  assert_parser_update
    ~preprocess_all_sources:true
    ~external_setups:
      [{ handle = "a.py"; old_source = Some "x = 1"; new_source = Some "def foo() -> int: ..." }]
    [{ handle = "b.py"; old_source = Some "from a import *"; new_source = Some "from a import *" }]
    ~expected:(Expectation.create [!&"a"; !&"b"]);
  assert_parser_update
    ~preprocess_all_sources:true
    [
      { handle = "a.py"; old_source = Some "x = 1"; new_source = Some "x = 2" };
      { handle = "b.py"; old_source = Some "y = 1"; new_source = Some "y = 2" };
      {
        handle = "c.py";
        old_source = Some "from a import *\nfrom b import *";
        new_source = Some "from a import *\nfrom b import *";
      };
    ]
    ~expected:(Expectation.create [!&"a"; !&"b"]);
  assert_parser_update
    ~preprocess_all_sources:true
    [
      { handle = "a.py"; old_source = Some "x = 1"; new_source = Some "y = 2" };
      { handle = "b.py"; old_source = Some "y = 1"; new_source = Some "y = 1" };
      {
        handle = "c.py";
        old_source = Some "from a import *\nfrom b import *";
        new_source = Some "from a import *\nfrom b import *";
      };
    ]
    ~expected:(Expectation.create [!&"a"; !&"c"]);
  assert_parser_update
    ~preprocess_all_sources:true
    [
      { handle = "a.py"; old_source = Some "x = 1"; new_source = Some "x = 1" };
      { handle = "b.py"; old_source = Some "y = 1"; new_source = Some "x = 2" };
      {
        handle = "c.py";
        old_source = Some "from a import *\nfrom b import *";
        new_source = Some "from a import *\nfrom b import *";
      };
    ]
    ~expected:(Expectation.create [!&"b"; !&"c"]);
  assert_parser_update
    ~preprocess_all_sources:true
    [
      { handle = "a.py"; old_source = Some "x = 1"; new_source = Some "x = 1" };
      { handle = "b.py"; old_source = Some "from a import *"; new_source = Some "from a import *" };
      { handle = "c.py"; old_source = Some "from b import *"; new_source = Some "from b import *" };
    ]
    ~expected:(Expectation.create []);
  assert_parser_update
    ~preprocess_all_sources:true
    [
      { handle = "a.py"; old_source = Some "x = 1"; new_source = Some "x = 2" };
      { handle = "b.py"; old_source = Some "from a import *"; new_source = Some "from a import *" };
      { handle = "c.py"; old_source = Some "from b import *"; new_source = Some "from b import *" };
    ]
    ~expected:(Expectation.create [!&"a"]);
  assert_parser_update
    ~preprocess_all_sources:true
    [
      { handle = "a.py"; old_source = Some "x = 1"; new_source = Some "y = 1" };
      { handle = "b.py"; old_source = Some "from a import *"; new_source = Some "from a import *" };
      { handle = "c.py"; old_source = Some "from b import *"; new_source = Some "from b import *" };
    ]
    ~expected:(Expectation.create [!&"a"; !&"b"; !&"c"]);

  (* This is expected -- the parser knows only names, not types *)
  assert_parser_update
    ~preprocess_all_sources:true
    ~external_setups:
      [{ handle = "a.py"; old_source = Some "x = 1"; new_source = Some "def x() -> None: ..." }]
    [{ handle = "b.py"; old_source = Some "from a import *"; new_source = Some "from a import *" }]
    ~expected:(Expectation.create [!&"a"]);
  ()


let test_ast_transformer context =
  let assert_transformed repository ~additional_preprocessing ~expected =
    let actual =
      ScratchProject.setup ~context ~include_typeshed_stubs:false repository
      |> ScratchProject.parse_sources
      |> fun (ast_environment, ast_environment_update_result) ->
      let sources =
        let transformed_ast_environment =
          AstEnvironment.with_additional_preprocessing ~additional_preprocessing ast_environment
        in
        let ast_environment = Analysis.AstEnvironment.read_only transformed_ast_environment in
        AstEnvironment.UpdateResult.invalidated_modules ast_environment_update_result
        |> List.filter_map ~f:(AstEnvironment.ReadOnly.get_processed_source ast_environment)
      in
      List.map sources ~f:(fun { Source.source_path = { SourcePath.relative; _ }; statements; _ } ->
          relative, statements)
      |> List.sort ~compare:(fun (left_handle, _) (right_handle, _) ->
             String.compare left_handle right_handle)
    in
    let equal (expected_handle, expected_source) (handle, statements) =
      let equal left right = Statement.location_insensitive_compare left right = 0 in
      String.equal expected_handle handle && List.equal equal expected_source statements
    in
    let printer (handle, statements) =
      Format.sprintf
        "%s: %s"
        handle
        (List.map statements ~f:Statement.show |> String.concat ~sep:"; ")
    in
    assert_equal ~cmp:(List.equal equal) ~printer:(List.to_string ~f:printer) expected actual
  in
  let open Statement in
  let open Expression in
  assert_transformed
    ["a.py", "def foo() -> int: ..."]
    ~additional_preprocessing:None
    ~expected:
      [
        ( "a.py",
          [
            +Statement.Define
               {
                 Define.signature =
                   {
                     Define.Signature.name = !&"a.foo";
                     parameters = [];
                     decorators = [];
                     return_annotation = Some !"int";
                     async = false;
                     generator = false;
                     parent = None;
                     nesting_define = None;
                   };
                 captures = [];
                 unbound_names = [];
                 body = [+Statement.Expression (+Expression.Constant Constant.Ellipsis)];
               };
          ] );
      ];
  let remove_first_statement ({ Source.statements; _ } as source) =
    { source with statements = List.tl_exn statements }
  in
  assert_transformed
    ["a.py", "def foo() -> int: ...\nbar: int = 1\n"]
    ~additional_preprocessing:(Some remove_first_statement)
    ~expected:
      [
        ( "a.py",
          [
            +Statement.Assign
               {
                 Assign.target = !"$local_a$bar";
                 value = +Expression.Constant (Constant.Integer 1);
                 annotation = Some !"int";
               };
          ] );
      ];
  ()


let () =
  "ast_environment"
  >::: [
         "basic" >:: test_basic;
         "parse_stubs_modules_list" >:: test_parse_stubs_modules_list;
         "parse_source" >:: test_parse_source;
         "parse_sources" >:: test_parse_sources;
         "ast_change" >:: test_ast_change;
         "parse_repository" >:: test_parse_repository;
         "parser_update" >:: test_parser_update;
         "ast_transformer" >:: test_ast_transformer;
       ]
  |> Test.run
