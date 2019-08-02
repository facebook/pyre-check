(* Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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
  let source_a = parse ~handle:handle_a source_a in
  let source_b = parse ~handle:handle_b source_b in
  let source_c = parse ~handle:handle_c source_c in
  let assert_source_path ~ast_environment ~expected reference =
    match AstEnvironment.get_source_path ast_environment reference with
    | None ->
        let message =
          Format.asprintf "Cannot find reference %a in the AST environment" Reference.pp reference
        in
        assert_failure message
    | Some source_path ->
        let actual = SourcePath.full_path ~configuration source_path in
        assert_equal ~cmp:Path.equal ~printer:Path.show expected actual
  in
  assert_source_path
    !&"a"
    ~ast_environment
    ~expected:(Path.create_relative ~root:local_root ~relative:handle_a);
  assert_source_path
    !&"b"
    ~ast_environment
    ~expected:(Path.create_relative ~root:local_root ~relative:handle_b);

  let assert_no_source ~get_source reference =
    match get_source reference with
    | None -> ()
    | Some _ ->
        let message =
          Format.asprintf "Getting source for %a is supposed to fail" Reference.pp reference
        in
        assert_failure message
  in
  let assert_source ~get_source ~expected reference =
    match get_source reference with
    | None ->
        let message = Format.asprintf "Cannot find source for %a" Reference.pp reference in
        assert_failure message
    | Some actual -> assert_source_equal expected actual
  in
  let () =
    let get_source = AstEnvironment.get_source ast_environment in
    assert_no_source ~get_source !&"a";
    assert_no_source ~get_source !&"b";
    assert_no_source ~get_source !&"c";
    AstEnvironment.add_source ast_environment source_a;
    assert_source ~get_source ~expected:source_a !&"a";
    assert_no_source ~get_source !&"b";
    assert_no_source ~get_source !&"c";
    AstEnvironment.add_source ast_environment source_b;
    assert_source ~get_source ~expected:source_a !&"a";
    assert_source ~get_source ~expected:source_b !&"b";
    assert_no_source ~get_source !&"c"
  in
  let () =
    let read_only_environment = AstEnvironment.read_only ast_environment in
    let get_source = AstEnvironment.ReadOnly.get_source read_only_environment in
    assert_source ~get_source ~expected:source_a !&"a";
    assert_source ~get_source ~expected:source_b !&"b";
    assert_no_source ~get_source !&"c"
  in
  let () =
    let get_source = AstEnvironment.get_source ast_environment in
    AstEnvironment.remove_sources ast_environment [!&"a"];
    assert_no_source ~get_source !&"a";
    assert_source ~get_source ~expected:source_b !&"b";
    assert_no_source ~get_source !&"c";

    AstEnvironment.add_source ast_environment source_c;
    assert_no_source ~get_source !&"a";
    assert_source ~get_source ~expected:source_b !&"b";
    assert_source ~get_source ~expected:source_c !&"c";

    AstEnvironment.remove_sources ast_environment [!&"c"; !&"b"];
    assert_no_source ~get_source !&"a";
    assert_no_source ~get_source !&"b";
    assert_no_source ~get_source !&"c"
  in
  ()


let () = "ast_environment" >::: ["basic" >:: test_basic] |> Test.run
