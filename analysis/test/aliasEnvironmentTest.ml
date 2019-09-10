(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Ast
open Analysis
open Pyre
open Test

let test_simple_registration context =
  let assert_registers source name expected =
    let project =
      ScratchProject.setup ["test.py", source] ~include_typeshed_stubs:false ~context
    in
    let _, ast_environment = ScratchProject.parse_sources project in
    let unannotated_global_environment =
      UnannotatedGlobalEnvironment.create (AstEnvironment.read_only ast_environment)
    in
    let alias_environment =
      AliasEnvironment.create
        (UnannotatedGlobalEnvironment.read_only unannotated_global_environment)
    in
    let _ =
      UnannotatedGlobalEnvironment.update
        unannotated_global_environment
        ~scheduler:(mock_scheduler ())
        ~configuration:(Configuration.Analysis.create ())
        (Reference.Set.singleton (Reference.create "test"))
      |> AliasEnvironment.update
           alias_environment
           ~scheduler:(mock_scheduler ())
           ~configuration:(Configuration.Analysis.create ())
    in
    let read_only = AliasEnvironment.read_only alias_environment in
    let expected = expected >>| fun expected -> Type.TypeAlias (Type.Primitive expected) in
    let printer v = v >>| Type.show_alias |> Option.value ~default:"none" in
    assert_equal ~printer expected (AliasEnvironment.ReadOnly.get_alias read_only name)
  in
  assert_registers {|
    class C:
      pass
    X = C
  |} "test.X" (Some "test.C");
  assert_registers {|
    class D:
      pass
    X = D
    Y = X
  |} "test.Y" (Some "test.D");
  assert_registers
    {|
    class E:
      pass
    X = E
    Y = X
    Z = Y
  |}
    "test.Z"
    (Some "test.E");
  assert_registers {|
    X = Z
    Y = X
    Z = Y
  |} "test.Z" None;
  ()


let test_harder_registrations context =
  let assert_registers source name ~parser expected =
    let project = ScratchProject.setup ["test.py", source] ~context in
    let sources, ast_environment = ScratchProject.parse_sources project in
    let qualifiers =
      List.map sources ~f:(fun { Source.source_path = { SourcePath.qualifier; _ }; _ } ->
          qualifier)
      |> Reference.Set.of_list
    in
    let unannotated_global_environment =
      UnannotatedGlobalEnvironment.create (AstEnvironment.read_only ast_environment)
    in
    let alias_environment =
      AliasEnvironment.create
        (UnannotatedGlobalEnvironment.read_only unannotated_global_environment)
    in
    let _ =
      UnannotatedGlobalEnvironment.update
        unannotated_global_environment
        ~scheduler:(mock_scheduler ())
        ~configuration:(Configuration.Analysis.create ())
        qualifiers
      |> AliasEnvironment.update
           alias_environment
           ~scheduler:(mock_scheduler ())
           ~configuration:(Configuration.Analysis.create ())
    in
    let read_only = AliasEnvironment.read_only alias_environment in
    let expected = expected >>| parser >>| fun alias -> Type.TypeAlias alias in
    let printer v =
      v >>| Type.sexp_of_alias >>| Sexp.to_string_hum |> Option.value ~default:"none"
    in
    assert_equal ~printer expected (AliasEnvironment.ReadOnly.get_alias read_only name)
  in
  let parsed_assert_registers =
    let parser x = parse_single_expression x |> Type.create ~aliases:(fun _ -> None) in
    assert_registers ~parser
  in
  let unparsed_assert_registers = assert_registers ~parser:Fn.id in
  parsed_assert_registers {|
    X = int
  |} "test.X" (Some "int");
  parsed_assert_registers
    {|
    from typing import Tuple
    X = int
    Y = Tuple[X, X]
  |}
    "test.Y"
    (Some "typing.Tuple[int, int]");
  parsed_assert_registers
    {|
    from typing import Tuple, List
    B = int
    A = List[B]
    Z = Tuple[A, B]
  |}
    "test.Z"
    (Some "typing.Tuple[typing.List[int], int]");
  unparsed_assert_registers
    {|
    from mypy_extensions import TypedDict
    X = int
    class Q(TypedDict):
      a: X
  |}
    "test.Q"
    (Some
       (Type.TypedDictionary
          { name = "Q"; total = true; fields = [{ name = "a"; annotation = Type.integer }] }));
  ()


let test_updates context =
  let assert_updates
      ?original_source
      ?new_source
      ~middle_actions
      ~expected_triggers
      ?post_actions
      ()
    =
    Memory.reset_shared_memory ();
    let sources = original_source >>| (fun source -> "test.py", source) |> Option.to_list in
    let project =
      ScratchProject.setup
        ~include_typeshed_stubs:false
        ~incremental_style:FineGrained
        sources
        ~context
    in
    let _, ast_environment = ScratchProject.parse_sources project in
    let configuration = ScratchProject.configuration_of project in
    let update () =
      update_environments
        ~configuration
        ~ast_environment
        ~qualifiers:(Reference.Set.singleton (Reference.create "test"))
        ()
    in
    let alias_environment = update () |> fst in
    let read_only = AliasEnvironment.read_only alias_environment in
    let execute_action (alias_name, dependency, expectation) =
      let printer v =
        v >>| Type.sexp_of_alias >>| Sexp.to_string_hum |> Option.value ~default:"none"
      in
      let expectation =
        expectation
        >>| parse_single_expression
        >>| Type.create ~aliases:(fun _ -> None)
        >>| fun alias -> Type.TypeAlias alias
      in
      AliasEnvironment.ReadOnly.get_alias read_only ~dependency alias_name
      |> assert_equal ~printer expectation
    in
    List.iter middle_actions ~f:execute_action;
    AstEnvironment.remove_sources ast_environment [Reference.create "test"];
    new_source
    >>| parse ~handle:"test.py"
    >>| Preprocessing.preprocess
    >>| AstEnvironment.add_source ast_environment
    |> Option.value ~default:();
    let update_result = update () |> snd in
    let printer set =
      SharedMemoryKeys.ReferenceDependencyKey.KeySet.elements set
      |> List.to_string ~f:Reference.show
    in
    let expected_triggers =
      SharedMemoryKeys.ReferenceDependencyKey.KeySet.of_list expected_triggers
    in
    assert_equal
      ~printer
      expected_triggers
      (AliasEnvironment.UpdateResult.triggered_dependencies update_result);
    post_actions >>| List.iter ~f:execute_action |> Option.value ~default:()
  in
  let dependency = Reference.create "dep" in
  assert_updates
    ~original_source:{|
      class C:
        pass
      X = C
    |}
    ~new_source:{|
      class C:
        pass
      X = C
    |}
    ~middle_actions:["test.X", dependency, Some "test.C"]
    ~expected_triggers:[]
    ~post_actions:["test.X", dependency, Some "test.C"]
    ();
  assert_updates
    ~original_source:{|
      class C:
        pass
      X = C
    |}
    ~new_source:{|
      X = C
    |}
    ~middle_actions:["test.X", dependency, Some "test.C"]
    ~expected_triggers:[dependency]
    ~post_actions:["test.X", dependency, None]
    ();
  assert_updates
    ~original_source:{|
      class C:
        pass
      X = C
    |}
    ~new_source:{|
      class C:
        pass
      Y = C
      X = Y
    |}
    ~middle_actions:
      ["test.X", dependency, Some "test.C"]
      (* Even if the route to the alias changed, no trigger *)
    ~expected_triggers:[]
    ~post_actions:["test.X", dependency, Some "test.C"]
    ();
  ()


let () =
  "environment"
  >::: [ "simple_registration" >:: test_simple_registration;
         "compounds" >:: test_harder_registrations;
         "updates" >:: test_updates ]
  |> Test.run
