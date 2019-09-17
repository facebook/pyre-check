(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
open Core
open OUnit2
open Analysis
open Ast
open Test

let default_environment context =
  let _, _, environment = ScratchProject.setup ~context [] |> ScratchProject.build_environment in
  environment


let assert_dependencies ~environment ~modules ~expected function_to_test =
  let ast_environment = Environment.ast_environment environment in
  let dependencies = Dependencies.create ast_environment in
  let dependencies =
    function_to_test dependencies ~modules:(List.map modules ~f:Reference.create)
    |> Set.to_list
    |> List.map ~f:Reference.show
    |> List.sort ~compare:String.compare
  in
  let expected = List.sort ~compare:String.compare expected in
  assert_equal ~printer:(List.to_string ~f:ident) expected dependencies


let add_dependent environment handle dependent =
  let ast_environment = Environment.ast_environment environment in
  let dependencies = Dependencies.create ast_environment in
  Dependencies.add_manual_dependency_for_test
    dependencies
    ~source:(Reference.create dependent)
    ~target:(Reference.create handle)


let purge () = Memory.reset_shared_memory ()

let test_dependent_of_list context =
  let environment = default_environment context in
  purge ();
  add_dependent environment "b" "a";
  add_dependent environment "c" "a";
  add_dependent environment "c" "b";
  add_dependent environment "a" "test";
  let assert_dependencies ~modules ~expected =
    assert_dependencies ~environment ~modules ~expected Dependencies.of_list
  in
  assert_dependencies ~modules:["b"; "c"] ~expected:["a"];
  assert_dependencies ~modules:["a"] ~expected:["test"];
  assert_dependencies ~modules:["c"] ~expected:["a"; "b"];
  assert_dependencies ~modules:["test"] ~expected:[]


let test_dependent_of_list_duplicates context =
  let environment = default_environment context in
  purge ();
  add_dependent environment "a" "b";
  add_dependent environment "a" "c";
  add_dependent environment "a" "b";
  add_dependent environment "a" "c";
  let assert_dependencies ~modules ~expected =
    assert_dependencies ~environment ~modules ~expected Dependencies.of_list
  in
  assert_dependencies ~modules:["a"] ~expected:["b"; "c"]


let test_transitive_dependent_of_list context =
  let environment = default_environment context in
  purge ();
  add_dependent environment "b" "a";
  add_dependent environment "c" "a";
  add_dependent environment "c" "b";
  add_dependent environment "a" "test";
  let assert_dependencies ~modules ~expected =
    assert_dependencies ~environment ~modules ~expected Dependencies.transitive_of_list
  in
  assert_dependencies ~modules:["b"; "c"] ~expected:["a"; "test"];
  assert_dependencies ~modules:["c"] ~expected:["a"; "b"; "test"];
  assert_dependencies ~modules:["test"] ~expected:[]


let test_transitive_dependents context =
  let environment = default_environment context in
  purge ();
  add_dependent environment "b" "a";
  add_dependent environment "c" "a";
  add_dependent environment "c" "b";
  add_dependent environment "a" "test";
  let assert_dependents ~handle ~expected =
    assert_dependencies ~environment ~modules:[handle] ~expected Dependencies.transitive_of_list
  in
  assert_dependents ~handle:"c" ~expected:["a"; "b"; "test"];
  ()


let test_import_dependencies context =
  purge ();
  let project =
    ScratchProject.setup
      ~context
      [
        ( "test.py",
          {|
         import a
         from builtins import str
         from subdirectory.b import c
         import sys
         from . import ignored
        |}
        );
        "a.py", "";
        "subdirectory/b.py", "";
      ]
  in
  let ast_environment, update = ScratchProject.parse_sources project in
  let dependencies = Dependencies.create (AstEnvironment.read_only ast_environment) in
  AstEnvironment.UpdateResult.reparsed update
  |> List.filter_map
       ~f:(AstEnvironment.ReadOnly.get_source (AstEnvironment.read_only ast_environment))
  |> Dependencies.register_all_dependencies dependencies;

  let dependencies qualifier =
    Dependencies.of_list dependencies ~modules:[!&qualifier]
    |> Set.to_list
    |> List.map ~f:Reference.show
  in
  let printer = List.to_string ~f:Fn.id in
  assert_equal ~printer (dependencies "subdirectory.b") ["test"];
  assert_equal ~printer (dependencies "a") ["test"];
  assert_equal ~printer (dependencies "") ["test"];
  assert_equal ~printer (dependencies "sys") ["numbers"; "test"];
  ()


let test_register_dependencies context =
  purge ();
  let ast_environment =
    let project =
      ScratchProject.setup
        ~context
        [
          "foo.py", "class Foo: ...";
          "bar/a.py", "class A: ...";
          "bar/b.py", "x = 42";
          "bar/c.py", "";
          "baz.py", "";
        ]
    in
    ScratchProject.parse_sources project |> fst
  in
  let source_test1 =
    {|
         import foo, baz
         from bar.a import A
         from bar import b
      |}
  in
  let source_test2 =
    {|
         import baz
         from bar.b import x
         from builtins import str
      |}
  in
  let dependencies = Dependencies.create (AstEnvironment.read_only ast_environment) in
  Dependencies.register_all_dependencies
    dependencies
    [parse ~handle:"test1.py" source_test1; parse ~handle:"test2.py" source_test2];
  let assert_dependency_equal ~expected qualifier =
    let actual = Dependencies.of_list dependencies ~modules:[qualifier] |> Set.to_list in
    assert_equal
      ~cmp:(List.equal Reference.equal)
      ~printer:(List.to_string ~f:Reference.show)
      expected
      actual
  in
  assert_dependency_equal !&"foo" ~expected:[!&"test1"];
  assert_dependency_equal !&"bar" ~expected:[!&"test1"];
  assert_dependency_equal !&"bar.a" ~expected:[!&"test1"];
  assert_dependency_equal !&"bar.b" ~expected:[!&"test1"; !&"test2"];
  assert_dependency_equal !&"bar.c" ~expected:[];
  assert_dependency_equal !&"baz" ~expected:[!&"test1"; !&"test2"];
  assert_dependency_equal !&"foo.Foo" ~expected:[];
  assert_dependency_equal !&"bar.a.A" ~expected:[];
  assert_dependency_equal !&"bar.b.x" ~expected:[];
  assert_dependency_equal !&"str" ~expected:[];

  Memory.reset_shared_memory ();
  ()


let () =
  "dependencies"
  >::: [
         "transitive_dependents" >:: test_transitive_dependents;
         "transitive_dependent_of_list" >:: test_transitive_dependent_of_list;
         "dependent_of_list" >:: test_dependent_of_list;
         "dependent_of_list_duplicates" >:: test_dependent_of_list_duplicates;
         "import_dependencies" >:: test_import_dependencies;
         "register_dependencies" >:: test_register_dependencies;
       ]
  |> Test.run
