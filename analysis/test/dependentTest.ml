(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)
open Core
open OUnit2
open Analysis
open Ast
open Test

let configuration = Configuration.Analysis.create ()

let test_index _ =
  let environment = Environment.Builder.create () in
  let source =
    {|
      class baz.baz(): pass
      _T = typing.TypeVar("_T")
      def foo(): pass
    |}
  in
  Test.populate
    ~configuration
    (Environment.handler environment)
    [parse ~handle:"test.py" ~qualifier:(Reference.create "test") source];
  let { Dependencies.class_keys; function_keys; alias_keys; _ } =
    environment.Environment.dependencies.Dependencies.index
  in
  let assert_table_contains_key table key =
    let keyset = Hashtbl.find_exn table (Reference.create "test") in
    assert_true (Hash_set.mem keyset key)
  in
  assert_table_contains_key class_keys "baz.baz";
  assert_table_contains_key function_keys !&"foo";
  assert_table_contains_key alias_keys "test._T"


let add_dependent table handle dependent =
  let handle = File.Handle.create_for_testing handle in
  let source = Source.qualifier ~handle in
  let dependent =
    File.Handle.create_for_testing dependent |> fun handle -> Source.qualifier ~handle
  in
  let update entry =
    match entry with
    | None -> Reference.Set.singleton dependent
    | Some set -> Set.add set dependent
  in
  Hashtbl.update table source ~f:update


let get_dependencies (module Handler : Environment.Handler) qualifier =
  Handler.dependencies qualifier


let assert_dependencies ~environment ~modules ~expected function_to_test =
  let get_dependencies = get_dependencies (Environment.handler environment) in
  let dependencies =
    function_to_test ~get_dependencies ~modules:(List.map modules ~f:Reference.create)
    |> Set.to_list
    |> List.map ~f:Reference.show
    |> List.sort ~compare:String.compare
  in
  let expected = List.sort ~compare:String.compare expected in
  assert_equal ~printer:(List.to_string ~f:ident) expected dependencies


let test_dependent_of_list _ =
  let table = Reference.Table.create () in
  let environment = Environment.Builder.create () in
  let dependencies =
    { environment.Environment.dependencies with Dependencies.dependents = table }
  in
  let environment = { environment with Environment.dependencies } in
  add_dependent table "b" "a";
  add_dependent table "c" "a";
  add_dependent table "c" "b";
  add_dependent table "a" "test";
  let assert_dependencies ~modules ~expected =
    assert_dependencies ~environment ~modules ~expected Dependencies.of_list
  in
  assert_dependencies ~modules:["b"; "c"] ~expected:["a"];
  assert_dependencies ~modules:["a"] ~expected:["test"];
  assert_dependencies ~modules:["c"] ~expected:["a"; "b"];
  assert_dependencies ~modules:["test"] ~expected:[]


let test_dependent_of_list_duplicates _ =
  let table = Reference.Table.create () in
  let environment = Environment.Builder.create () in
  let dependencies =
    { environment.Environment.dependencies with Dependencies.dependents = table }
  in
  let environment = { environment with Environment.dependencies } in
  add_dependent table "a" "b";
  add_dependent table "a" "c";
  add_dependent table "a" "b";
  add_dependent table "a" "c";
  let assert_dependencies ~modules ~expected =
    assert_dependencies ~environment ~modules ~expected Dependencies.of_list
  in
  assert_dependencies ~modules:["a"] ~expected:["b"; "c"]


let test_transitive_dependent_of_list _ =
  let table = Reference.Table.create () in
  let environment = Environment.Builder.create () in
  let dependencies =
    { environment.Environment.dependencies with Dependencies.dependents = table }
  in
  let environment = { environment with Environment.dependencies } in
  add_dependent table "b" "a";
  add_dependent table "c" "a";
  add_dependent table "c" "b";
  add_dependent table "a" "test";
  let assert_dependencies ~modules ~expected =
    assert_dependencies ~environment ~modules ~expected Dependencies.transitive_of_list
  in
  assert_dependencies ~modules:["b"; "c"] ~expected:["a"; "test"];
  assert_dependencies ~modules:["c"] ~expected:["a"; "b"; "test"];
  assert_dependencies ~modules:["test"] ~expected:[]


let test_transitive_dependents _ =
  let table = Reference.Table.create () in
  let environment = Environment.Builder.create () in
  let dependencies =
    { environment.Environment.dependencies with Dependencies.dependents = table }
  in
  let environment = { environment with Environment.dependencies } in
  add_dependent table "b" "a";
  add_dependent table "c" "a";
  add_dependent table "c" "b";
  add_dependent table "a" "test";
  let assert_dependents ~handle ~expected =
    let get_dependencies = get_dependencies (Environment.handler environment) in
    let dependencies =
      Dependencies.transitive_of_list
        ~modules:[Source.qualifier ~handle:(File.Handle.create_for_testing handle)]
        ~get_dependencies
      |> Set.to_list
      |> List.map ~f:Reference.show
      |> List.sort ~compare:String.compare
    in
    let expected = List.sort ~compare:String.compare expected in
    assert_equal expected dependencies
  in
  assert_dependents ~handle:"c.py" ~expected:["a"; "b"; "test"]


let test_normalize _ =
  let assert_normalized ~edges expected =
    let (module Handler : Environment.Handler) =
      Environment.Builder.create () |> Environment.handler
    in
    let add_dependent (left, right) =
      Handler.DependencyHandler.add_dependent ~qualifier:(Reference.create left) !&right
    in
    List.iter edges ~f:add_dependent;
    let all_modules =
      edges
      |> List.concat_map ~f:(fun (left, right) -> [left; right])
      |> List.map ~f:(fun name -> Reference.create name)
    in
    Handler.DependencyHandler.normalize all_modules;
    let assert_dependents_equal (node, expected) =
      let expected =
        List.map expected ~f:(fun name -> Reference.create name)
        |> List.sort ~compare:Reference.compare
        |> Reference.Set.Tree.of_list
      in
      let printer = function
        | None -> "None"
        | Some dependents -> Reference.Set.Tree.sexp_of_t dependents |> Sexp.to_string
      in
      (* If the printer shows identical sets here but the equality fails, the underlying
         representation must have diverged. *)
      assert_equal ~printer (Some expected) (Handler.DependencyHandler.dependents !&node)
    in
    List.iter expected ~f:assert_dependents_equal
  in
  assert_normalized ~edges:["a", "b"] ["b", ["a"]];
  assert_normalized ~edges:["a", "c"; "b", "c"] ["c", ["a"; "b"]];
  assert_normalized ~edges:["b", "c"; "a", "c"] ["c", ["a"; "b"]];
  assert_normalized
    ~edges:["a", "h"; "b", "h"; "c", "h"; "d", "h"; "e", "h"; "f", "h"; "g", "h"]
    ["h", ["a"; "b"; "c"; "d"; "e"; "f"; "g"]];
  assert_normalized
    ~edges:["g", "h"; "f", "h"; "e", "h"; "d", "h"; "c", "h"; "b", "h"; "a", "h"]
    ["h", ["a"; "b"; "c"; "d"; "e"; "f"; "g"]];
  assert_normalized
    ~edges:["d", "h"; "e", "h"; "f", "h"; "g", "h"; "c", "h"; "b", "h"; "a", "h"]
    ["h", ["a"; "b"; "c"; "d"; "e"; "f"; "g"]]


let () =
  "dependencies"
  >::: [ "index" >:: test_index;
         "transitive_dependents" >:: test_transitive_dependents;
         "transitive_dependent_of_list" >:: test_transitive_dependent_of_list;
         "dependent_of_list" >:: test_dependent_of_list;
         "dependent_of_list_duplicates" >:: test_dependent_of_list_duplicates;
         "normalize" >:: test_normalize ]
  |> Test.run
