(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)
open Core
open OUnit2

open Analysis
open Ast
open Expression

open Test


let configuration = Configuration.Analysis.create ()


let populate source =
  let environment = Environment.Builder.create () in
  Service.Environment.populate
    ~configuration
    (Environment.handler ~configuration environment)
    [parse source];
  environment
  |> Environment.handler ~configuration


let access names =
  List.map ~f:Expression.Access.create names |> List.concat


let primitive name = Type.Primitive ~~name


let test_index _ =
  let environment = Environment.Builder.create () in
  let source = {|
      class baz.baz(): pass
      _T = typing.TypeVar("_T")
      def foo(): pass
    |}
  in
  Service.Environment.populate
    ~configuration
    (Environment.handler ~configuration environment)
    [parse ~handle:"test.py" source];
  let {
    Dependencies.class_keys;
    function_keys;
    alias_keys;
    _;
  } = environment.Environment.dependencies.Dependencies.index in
  let assert_table_contains_key table key =
    let keyset = Hashtbl.find_exn table (File.Handle.create "test.py") in
    assert_true (Hash_set.mem keyset key)
  in
  assert_table_contains_key class_keys (primitive "baz.baz");
  assert_table_contains_key function_keys (access ["foo"]);
  assert_table_contains_key alias_keys (primitive "_T")


let add_dependent table handle dependent =
  let handle = File.Handle.create handle in
  let source = Source.qualifier ~handle in
  let dependent = File.Handle.create dependent in
  match Hashtbl.find table source with
  | None -> Hashtbl.set table ~key:source ~data:[dependent]
  | Some dependents -> Hashtbl.set table ~key:source ~data:(dependent :: dependents)


let get_dependencies (module Handler: Environment.Handler) handle =
  Handler.dependencies (Source.qualifier ~handle)


let test_dependent_of_list _ =
  let table = Access.Table.create () in
  let environment = Environment.Builder.create () in
  let dependencies =
    { environment.Environment.dependencies with Dependencies.dependents = table }
  in
  let environment = { environment with Environment.dependencies } in
  add_dependent table "b.py" "a.py";
  add_dependent table "c.py" "a.py";
  add_dependent table "c.py" "b.py";
  add_dependent table "a.py" "test.py";
  let assert_dependencies ~handles ~expected =
    let get_dependencies = get_dependencies (Environment.handler ~configuration environment) in
    let dependencies =
      Dependencies.of_list ~handles:(List.map handles ~f:File.Handle.create) ~get_dependencies
      |> Set.to_list
      |> List.map ~f:File.Handle.show
      |> List.sort ~compare:String.compare
    in
    let expected = List.sort ~compare:String.compare expected in
    assert_equal expected dependencies
  in
  assert_dependencies
    ~handles:["b.py"; "c.py"]
    ~expected:["a.py"];
  assert_dependencies
    ~handles:["a.py"]
    ~expected:["test.py"];
  assert_dependencies
    ~handles:["c.py"]
    ~expected:["a.py"; "b.py"];
  assert_dependencies
    ~handles:["test.py"]
    ~expected:[]


let test_transitive_dependent_of_list _ =
  let table = Access.Table.create () in
  let environment = Environment.Builder.create () in
  let dependencies =
    { environment.Environment.dependencies with Dependencies.dependents = table }
  in
  let environment = { environment with Environment.dependencies } in
  add_dependent table "b.py" "a.py";
  add_dependent table "c.py" "a.py";
  add_dependent table "c.py" "b.py";
  add_dependent table "a.py" "test.py";
  let assert_dependencies ~handles ~expected =
    let get_dependencies = get_dependencies (Environment.handler ~configuration environment) in
    let dependencies =
      Dependencies.transitive_of_list
        ~handles:(List.map handles ~f:File.Handle.create)
        ~get_dependencies
      |> Set.to_list
      |> List.map ~f:File.Handle.show
      |> List.sort ~compare:String.compare
    in
    let expected = List.sort ~compare:String.compare expected in
    assert_equal expected dependencies
  in
  assert_dependencies
    ~handles:["b.py"; "c.py"]
    ~expected:["a.py"; "test.py"];
  assert_dependencies
    ~handles:["c.py"]
    ~expected:["a.py"; "b.py"; "test.py"];
  assert_dependencies
    ~handles:["test.py"]
    ~expected:[]


let test_transitive_dependents _ =
  let table = Access.Table.create () in
  let environment = Environment.Builder.create () in
  let dependencies =
    { environment.Environment.dependencies with Dependencies.dependents = table }
  in
  let environment = { environment with Environment.dependencies } in
  add_dependent table "b.py" "a.py";
  add_dependent table "c.py" "a.py";
  add_dependent table "c.py" "b.py";
  add_dependent table "a.py" "test.py";
  let assert_dependents ~handle ~expected =
    let get_dependencies = get_dependencies (Environment.handler ~configuration environment) in
    let dependencies =
      Dependencies.transitive
        ~handle:(File.Handle.create handle)
        ~get_dependencies
      |> Set.to_list
      |> List.map ~f:File.Handle.show
      |> List.sort ~compare:String.compare
    in
    let expected = List.sort ~compare:String.compare expected in
    assert_equal expected dependencies
  in
  assert_dependents
    ~handle:"c.py"
    ~expected:["a.py"; "b.py"; "test.py"]


let () =
  "dependencies">:::[
    "index">::test_index;
    "transitive_dependents">::test_transitive_dependents;
    "transitive_dependent_of_list">::test_transitive_dependent_of_list;
    "dependent_of_list">::test_dependent_of_list;
  ]
  |> Test.run
