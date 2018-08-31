(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)
open Core
open OUnit2

open Analysis
open Ast
open Expression

open Test


let configuration = Configuration.create ()


let populate source =
  let environment = Environment.Builder.create () in
  Service.Environment.populate
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
    (Environment.handler ~configuration environment)
    [parse ~path:"test.py" source];
  let {
    Dependencies.class_keys;
    function_keys;
    alias_keys;
    _;
  } = environment.Environment.dependencies.Dependencies.index in
  let assert_table_contains_key table key =
    let keyset = Hashtbl.find_exn table "test.py" in
    assert_true (Hash_set.mem keyset key)
  in
  assert_table_contains_key class_keys (primitive "baz.baz");
  assert_table_contains_key function_keys (access ["foo"]);
  assert_table_contains_key alias_keys (primitive "_T")


let add_dependent table path dependent =
  let source = Source.qualifier ~path in
  match Hashtbl.find table source with
  | None -> Hashtbl.set table ~key:source ~data:[dependent]
  | Some dependents -> Hashtbl.set table ~key:source ~data:(dependent :: dependents)


let get_dependencies (module Handler: Environment.Handler) path =
  Handler.dependencies (Source.qualifier ~path)


let test_dependent_of_list _ =
  let table = Access.Table.create () in
  let environment = Environment.Builder.create () in
  let dependencies =
    { environment.Environment.dependencies with Dependencies.dependents = table }
  in
  let environment = { environment with Environment.dependencies } in
  let get_dependencies = get_dependencies (Environment.handler ~configuration environment) in
  add_dependent table "b.py" "a.py";
  add_dependent table "c.py" "a.py";
  add_dependent table "c.py" "b.py";
  add_dependent table "a.py" "test.py";
  assert_equal
    ~cmp:String.Set.equal
    (String.Set.of_list ["a.py"])
    (Dependencies.of_list ~handles:["b.py"; "c.py"] ~get_dependencies);
  assert_equal
    ~cmp:String.Set.equal
    (String.Set.of_list ["test.py"])
    (Dependencies.of_list ~handles:["a.py"] ~get_dependencies);
  assert_equal
    ~cmp:String.Set.equal
    (String.Set.of_list ["a.py"; "b.py"])
    (Dependencies.of_list ~handles:["c.py"] ~get_dependencies);
  assert_equal
    ~cmp:String.Set.equal
    (String.Set.of_list [])
    (Dependencies.of_list ~handles:["test.py"] ~get_dependencies)


let test_transitive_dependent_of_list _ =
  let table = Access.Table.create () in
  let environment = Environment.Builder.create () in
  let dependencies =
    { environment.Environment.dependencies with Dependencies.dependents = table }
  in
  let environment = { environment with Environment.dependencies } in
  let get_dependencies = get_dependencies (Environment.handler ~configuration environment) in
  add_dependent table "b.py" "a.py";
  add_dependent table "c.py" "a.py";
  add_dependent table "c.py" "b.py";
  add_dependent table "a.py" "test.py";
  assert_equal
    ~cmp:String.Set.equal
    (String.Set.of_list ["a.py"; "test.py"])
    (Dependencies.transitive_of_list
       ~handles:["b.py"; "c.py"]
       ~get_dependencies);
  assert_equal
    ~cmp:String.Set.equal
    (String.Set.of_list ["a.py"; "b.py"; "test.py"])
    (Dependencies.transitive_of_list ~handles:["c.py"] ~get_dependencies);
  assert_equal
    ~cmp:String.Set.equal
    (String.Set.of_list [])
    (Dependencies.transitive_of_list ~handles:["test.py"] ~get_dependencies)


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
  let get_dependencies = get_dependencies (Environment.handler ~configuration environment) in
  assert_equal
    ~cmp:String.Set.equal
    ~printer:(fun set -> Set.to_list set |> String.concat ~sep:",")
    (String.Set.of_list ["a.py"; "b.py"; "test.py"])
    (Dependencies.transitive ~get_dependencies ~handle:"c.py")


let () =
  "dependencies">:::[
    "index">::test_index;
    "transitive_dependents">::test_transitive_dependents;
    "transitive_dependent_of_list">::test_transitive_dependent_of_list;
    "dependent_of_list">::test_dependent_of_list;
  ]
  |> run_test_tt_main
