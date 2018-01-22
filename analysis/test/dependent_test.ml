(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)
open Core
open OUnit2

open Analysis
open Ast

open Test

let populate ?source_root source =
  let environment = Environment.Builder.create () in
  Environment.populate ?source_root (Environment.reader environment) [parse source];
  environment |> Environment.reader


let access names = List.map ~f:Expression.Access.create names |> List.concat


let primitive name = Type.Primitive ~~name


let test_index _ =
  let environment = Environment.Builder.create () in
  let source = {|
      import enum
      class baz.baz(): pass
      _T = typing.TypeVar("_T")
      def foo(): pass
      class Color(enum.Enum):
        RED = 0
    |}
  in
  Environment.populate (Environment.reader environment) [parse ~path:"test.py" source];
  let {
    Dependencies.class_keys;
    function_keys;
    alias_keys;
    global_keys;
    _;
  } = environment.Environment.dependencies.Dependencies.index in
  let assert_table_contains_key table key =
    let keyset = Hashtbl.find_exn table "test.py" in
    assert_true (Hash_set.mem keyset key)
  in
  assert_table_contains_key class_keys (primitive "baz.baz");
  assert_table_contains_key class_keys (primitive "Color");
  assert_table_contains_key function_keys (access ["foo"]);
  assert_table_contains_key alias_keys (primitive "_T");
  assert_table_contains_key global_keys (access ["Color"; "RED"])


let test_dependent_of_list _ =
  let table = String.Table.create () in
  let add_dependent source dependent =
    match Hashtbl.find table source with
    | None -> Hashtbl.set table ~key:source ~data:[dependent]
    | Some dependents -> Hashtbl.set table ~key:source ~data:(dependent :: dependents)
  in
  let environment = Environment.Builder.create () in
  let dependencies =
    { environment.Environment.dependencies with Dependencies.dependents = table }
  in
  let environment = { environment with Environment.dependencies } in
  let (module Reader: Environment.Reader) =
    Environment.reader environment
  in
  add_dependent "b.py" "a.py";
  add_dependent "c.py" "a.py";
  add_dependent "c.py" "b.py";
  add_dependent "a.py" "test.py";
  assert_equal
    ~cmp:String.Set.equal
    (String.Set.of_list ["a.py"])
    (Dependencies.of_list ~paths:["b.py"; "c.py"] ~get_dependencies:Reader.dependencies);
  assert_equal
    ~cmp:String.Set.equal
    (String.Set.of_list ["test.py"])
    (Dependencies.of_list ~paths:["a.py"] ~get_dependencies:Reader.dependencies);
  assert_equal
    ~cmp:String.Set.equal
    (String.Set.of_list ["a.py"; "b.py"])
    (Dependencies.of_list ~paths:["c.py"] ~get_dependencies:Reader.dependencies);
  assert_equal
    ~cmp:String.Set.equal
    (String.Set.of_list [])
    (Dependencies.of_list ~paths:["test.py"] ~get_dependencies:Reader.dependencies)


let test_transitive_dependent_of_list _ =
  let table = String.Table.create () in
  let add_dependent source dependent =
    match Hashtbl.find table source with
    | None -> Hashtbl.set table ~key:source ~data:[dependent]
    | Some dependents -> Hashtbl.set table ~key:source ~data:(dependent :: dependents)
  in
  let environment = Environment.Builder.create () in
  let dependencies =
    { environment.Environment.dependencies with Dependencies.dependents = table }
  in
  let environment = { environment with Environment.dependencies } in
  let (module Reader: Environment.Reader) =
    Environment.reader environment
  in
  add_dependent "b.py" "a.py";
  add_dependent "c.py" "a.py";
  add_dependent "c.py" "b.py";
  add_dependent "a.py" "test.py";
  assert_equal
    ~cmp:String.Set.equal
    (String.Set.of_list ["a.py"; "test.py"])
    (Dependencies.transitive_of_list ~paths:["b.py"; "c.py"] ~get_dependencies:Reader.dependencies);
  assert_equal
    ~cmp:String.Set.equal
    (String.Set.of_list ["a.py"; "b.py"; "test.py"])
    (Dependencies.transitive_of_list ~paths:["c.py"] ~get_dependencies:Reader.dependencies);
  assert_equal
    ~cmp:String.Set.equal
    (String.Set.of_list [])
    (Dependencies.transitive_of_list ~paths:["test.py"] ~get_dependencies:Reader.dependencies)


let test_transitive_dependents _ =
  let table = String.Table.create () in
  let environment = Environment.Builder.create () in
  let dependencies =
    { environment.Environment.dependencies with Dependencies.dependents = table }
  in
  let environment = { environment with Environment.dependencies } in
  let (module Reader: Environment.Reader) =
    Environment.reader environment
  in
  let add_dependency source dependency =
    match Hashtbl.find table source with
    | None -> Hashtbl.set table ~key:source ~data:[dependency]
    | Some dependencies -> Hashtbl.set table ~key:source ~data:(dependency :: dependencies)
  in
  add_dependency "b.py" "a.py";
  add_dependency "c.py" "a.py";
  add_dependency "c.py" "b.py";
  add_dependency "a.py" "test.py";
  assert_equal
    ~cmp:String.Set.equal
    ~printer:(fun set -> Set.to_list set |> String.concat ~sep:",")
    (String.Set.of_list ["a.py"; "b.py"; "test.py"])
    (Dependencies.transitive ~get_dependencies:Reader.dependencies ~path:"c.py")


let () =
  "dependencies">:::[
    "index">::test_index;
    "transitive_dependents">::test_transitive_dependents;
    "transitive_dependent_of_list">::test_transitive_dependent_of_list;
    "dependent_of_list">::test_dependent_of_list;
  ]
  |> run_test_tt_main
