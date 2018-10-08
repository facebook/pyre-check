(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)
open Core

open OUnit2

open Test
open Ast


let modules_equal left right =
  let left = { left with Codex.PythonModule.filename = ""  } in
  let right = { right with Codex.PythonModule.filename = "" } in
  Codex.PythonModule.equal left right


let assert_python_module_equal expected source =
  let configuration = Configuration.Analysis.create () in
  let actual =
    Codex.source_to_codex_representation ~configuration (parse source)
  in
  assert_equal
    ~cmp:modules_equal
    ~printer:(
      fun python_module ->
        python_module
        |> Codex.PythonModule.to_yojson
        |> Yojson.Safe.pretty_to_string)
    expected
    actual


let default_python_module members = {
  Codex.PythonModule.name = "test";
  docstring = None;
  rank = 0;
  filename = (Filename.realpath "/tmp") ^ "/test.py";
  members;
}


let test_function _ =
  let source =
    {|
      def foo(a):
          """doc
          multiline"""
          return a
    |}
  in
  let expected_codex_representation =
    default_python_module [
      Codex.CodexNode.FunctionNode {
        Codex.CodexNode.Function.docstring = Some "doc\nmultiline";
        name = "foo";
        rank = 0;
        comments = None;
        location = [2; 0];
        ty = "function";
        source = "";
        symbols = [];
        decorators = [];
        arguments = {
          Codex.ArgumentData.args = ["a"];
          ty = "arguments";
          defaults = [];
          kwarg = None;
          vararg = None;
        };
      }]
  in
  assert_python_module_equal expected_codex_representation source

let test_variable _ =
  let source = "x = 1" in
  let expected_codex_representation =
    default_python_module [
      Codex.CodexNode.VariableNode {
        Codex.CodexNode.Variable.name = "x";
        default = Some "1";
        location = [1; 0];
        ty = "variable";
      };
    ]
  in
  assert_python_module_equal expected_codex_representation source


let test_class _ =
  let source =
    {|
      class A():
        def foo(self):
          return a
    |}
  in
  let expected_codex_representation =
    default_python_module [
      Codex.CodexNode.ClassNode {
        Codex.CodexNode.Class.docstring = None;
        name = "A";
        rank = 0;
        comments = None;
        location = [2; 0];
        ty = "class";
        supers = [];
        mro = [];
        members = [
          Codex.CodexNode.FunctionNode {
            Codex.CodexNode.Function.name = "A.foo";
            docstring = None;
            rank = 0;
            comments = None;
            location = [3; 2];
            ty = "function";
            source = "";
            symbols = [];
            decorators = [];
            arguments = {
              Codex.ArgumentData.args = ["self"];
              ty = "arguments";
              defaults = [];
              kwarg = None;
              vararg = None;
            };
          }];
      }];
  in
  assert_python_module_equal expected_codex_representation source


let test_arguments _ =
  let source =
    {|
      def foo(a, b = 1, *c, **d):
          pass
    |}
  in
  let expected_codex_representation =
    default_python_module [
      Codex.CodexNode.FunctionNode {
        Codex.CodexNode.Function.name = "foo";
        docstring = None;
        rank = 0;
        comments = None;
        location = [2; 0];
        ty = "function";
        source = "";
        symbols = [];
        decorators = [];
        arguments = {
          Codex.ArgumentData.args = ["a"; "b"];
          ty = "arguments";
          defaults = ["1"];
          vararg = Some "*c";
          kwarg = Some "**d";
        };
      }
    ]
  in
  assert_python_module_equal expected_codex_representation source

let test_source context =
  let directory = bracket_tmpdir context in
  let file = directory ^/ "test.py" in
  let source =
    {|
      def foo():
          pass
    |}
    |> trim_extra_indentation in
  Out_channel.write_all ~data:source file;
  let expected_codex_representation = {
    Codex.PythonModule.name = "test";
    docstring = None;
    rank = 0;
    filename = directory ^/ "test.py";
    members = [
      Codex.CodexNode.FunctionNode {
        Codex.CodexNode.Function.docstring = None;
        name = "foo";
        rank = 0;
        comments = None;
        location = [2; 0];
        ty = "function";
        source = "def foo():\n    pass\n";
        symbols = [];
        decorators = [];
        arguments = {
          Codex.ArgumentData.args = [];
          ty = "arguments";
          defaults = [];
          kwarg = None;
          vararg = None;
        };
      };
    ]
  }
  in
  (* We need to actually read the file in order for the codex representation
   * to contain the source. *)
  let codex_representation =
    source
    |> parse_untrimmed
    |> Codex.source_to_codex_representation
      ~configuration:(
        Configuration.Analysis.create ~local_root:(Pyre.Path.create_absolute directory) ())
  in
  assert_equal
    ~cmp:modules_equal
    ~printer:Codex.PythonModule.show
    expected_codex_representation
    codex_representation


let () =
  "codex">:::[
    "function">::test_function;
    "class">::test_class;
    "variable">::test_variable;
    "arguments">::test_arguments;
    "source">::test_source;
  ]
  |> Test.run
