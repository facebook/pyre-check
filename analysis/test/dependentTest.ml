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
  Service.Environment.populate ~configuration (Environment.handler environment) [parse source];
  environment
  |> Environment.handler


let access names =
  List.map ~f:Expression.Access.create names |> List.concat


let primitive name = Type.Primitive name


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
    (Environment.handler environment)
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
  let update entry =
    match entry with
    | None ->
        File.Handle.Set.singleton dependent
    | Some set ->
        Set.add set dependent
  in
  Hashtbl.update table source ~f:update


let get_dependencies (module Handler: Environment.Handler) handle =
  Handler.dependencies (Source.qualifier ~handle)


let assert_dependencies ~environment ~handles ~expected function_to_test =
  let get_dependencies = get_dependencies (Environment.handler environment) in
  let dependencies =
    function_to_test
      ~get_dependencies
      ~handles:(List.map handles ~f:File.Handle.create)
    |> Set.to_list
    |> List.map ~f:File.Handle.show
    |> List.sort ~compare:String.compare
  in
  let expected = List.sort ~compare:String.compare expected in
  assert_equal ~printer:(List.to_string ~f:ident) expected dependencies


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
    assert_dependencies ~environment ~handles ~expected Dependencies.of_list
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


let test_dependent_of_list_duplicates _ =
  let table = Access.Table.create () in
  let environment = Environment.Builder.create () in
  let dependencies =
    { environment.Environment.dependencies with Dependencies.dependents = table }
  in
  let environment = { environment with Environment.dependencies } in
  add_dependent table "a.py" "b.py";
  add_dependent table "a.py" "c.py";
  add_dependent table "a.py" "b.py";
  add_dependent table "a.py" "c.py";
  let assert_dependencies ~handles ~expected =
    assert_dependencies ~environment ~handles ~expected Dependencies.of_list
  in
  assert_dependencies
    ~handles:["a.py"]
    ~expected:["b.py"; "c.py"]


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
    assert_dependencies ~environment ~handles ~expected Dependencies.transitive_of_list
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
    let get_dependencies = get_dependencies (Environment.handler environment) in
    let dependencies =
      Dependencies.transitive_of_list
        ~handles:[File.Handle.create handle]
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


let test_normalize _ =
  let assert_normalized ~edges expected =
    let (module Handler: Environment.Handler) =
      Environment.Builder.create ()
      |> Environment.handler
    in
    let add_dependent (left, right) =
      Handler.DependencyHandler.add_dependent
        ~handle:(File.Handle.create (left ^ ".py"))
        (Access.create right)
    in
    List.iter edges ~f:add_dependent;
    let assert_dependents_equal (node, expected) =
      Handler.DependencyHandler.normalize ~handle:(File.Handle.create node);
      let expected =
        List.map expected ~f:(fun name -> File.Handle.create name)
        |> List.sort ~compare:File.Handle.compare
        |> File.Handle.Set.Tree.of_list
      in
      let printer = function
        | None -> "None"
        | Some dependents ->
            File.Handle.Set.Tree.sexp_of_t dependents
            |> Sexp.to_string
      in
      (* If the printer shows identical sets here but the equality fails, the underlying
         representation must have diverged. *)
      assert_equal
        ~printer
        (Some expected)
        (Handler.DependencyHandler.dependents (Access.create node))
    in
    List.iter expected ~f:assert_dependents_equal
  in
  assert_normalized ~edges:["a", "b"] ["b", ["a.py"]];
  assert_normalized ~edges:["a", "c"; "b", "c"] ["c", ["a.py"; "b.py"]];
  assert_normalized ~edges:["b", "c"; "a", "c"] ["c", ["a.py"; "b.py"]];
  assert_normalized
    ~edges:[
      "a", "h";
      "b", "h";
      "c", "h";
      "d", "h";
      "e", "h";
      "f", "h";
      "g", "h";
    ]
    ["h", ["a.py"; "b.py"; "c.py"; "d.py"; "e.py"; "f.py"; "g.py"]];
  assert_normalized
    ~edges:[
      "g", "h";
      "f", "h";
      "e", "h";
      "d", "h";
      "c", "h";
      "b", "h";
      "a", "h";
    ]
    ["h", ["a.py"; "b.py"; "c.py"; "d.py"; "e.py"; "f.py"; "g.py"]];
  assert_normalized
    ~edges:[
      "d", "h";
      "e", "h";
      "f", "h";
      "g", "h";
      "c", "h";
      "b", "h";
      "a", "h";
    ]
    ["h", ["a.py"; "b.py"; "c.py"; "d.py"; "e.py"; "f.py"; "g.py"]]


let () =
  "dependencies">:::[
    "index">::test_index;
    "transitive_dependents">::test_transitive_dependents;
    "transitive_dependent_of_list">::test_transitive_dependent_of_list;
    "dependent_of_list">::test_dependent_of_list;
    "dependent_of_list_duplicates">::test_dependent_of_list_duplicates;
    "normalize">::test_normalize;
  ]
  |> Test.run
