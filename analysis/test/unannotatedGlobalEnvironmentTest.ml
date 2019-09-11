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
  let assert_registers ?(expected = true) source name =
    let project = ScratchProject.setup ["test.py", source] ~context in
    let _, ast_environment, _ = ScratchProject.build_environment project in
    let unannotated_global_environment =
      UnannotatedGlobalEnvironment.create (AstEnvironment.read_only ast_environment)
    in
    let _ =
      UnannotatedGlobalEnvironment.update
        unannotated_global_environment
        ~scheduler:(mock_scheduler ())
        ~configuration:(Configuration.Analysis.create ())
        (Reference.Set.singleton (Reference.create "test"))
    in
    let read_only = UnannotatedGlobalEnvironment.read_only unannotated_global_environment in
    assert_equal (UnannotatedGlobalEnvironment.ReadOnly.class_exists read_only name) expected
  in
  assert_registers {|
   class Bar:
     pass
  |} "test.Bar";
  assert_registers ~expected:false {|
   class Foo:
     pass
  |} "test.Bar";
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
    let unannotated_global_environment =
      UnannotatedGlobalEnvironment.create (AstEnvironment.read_only ast_environment)
    in
    let configuration = ScratchProject.configuration_of project in
    let _ =
      UnannotatedGlobalEnvironment.update
        unannotated_global_environment
        ~scheduler:(mock_scheduler ())
        ~configuration
        (Reference.Set.singleton (Reference.create "test"))
    in
    let read_only = UnannotatedGlobalEnvironment.read_only unannotated_global_environment in
    let execute_action = function
      | `Get (class_name, dependency, expected_number_of_statements) ->
          let printer number =
            number
            >>| Format.sprintf "number of statements: %d"
            |> Option.value ~default:"No class"
          in
          UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
            read_only
            ~dependency
            class_name
          >>| (fun { Node.value = { body; _ }; _ } -> body)
          >>| List.length
          |> assert_equal ~printer expected_number_of_statements
      | `Mem (class_name, dependency, expectation) ->
          UnannotatedGlobalEnvironment.ReadOnly.class_exists read_only ~dependency class_name
          |> assert_equal expectation
      | `AllClasses expectation ->
          UnannotatedGlobalEnvironment.ReadOnly.all_classes read_only
          |> assert_equal ~printer:(List.to_string ~f:Fn.id) expectation
      | `Global (global_name, dependency, expectation) ->
          let printer optional =
            optional
            >>| UnannotatedGlobalEnvironment.show_unannotated_global
            |> Option.value ~default:"none"
          in
          let cmp left right =
            Option.compare UnannotatedGlobalEnvironment.compare_unannotated_global left right = 0
          in
          UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
            read_only
            global_name
            ~dependency
          |> assert_equal ~cmp ~printer expectation
    in
    List.iter middle_actions ~f:execute_action;
    AstEnvironment.remove_sources ast_environment [Reference.create "test"];
    new_source
    >>| parse ~handle:"test.py"
    >>| Preprocessing.preprocess
    >>| AstEnvironment.add_source ast_environment
    |> Option.value ~default:();
    let update_result =
      UnannotatedGlobalEnvironment.update
        unannotated_global_environment
        ~scheduler:(mock_scheduler ())
        ~configuration
        (Reference.Set.singleton (Reference.create "test"))
    in
    let printer set =
      UnannotatedGlobalEnvironment.DependencyKey.KeySet.elements set
      |> List.to_string ~f:UnannotatedGlobalEnvironment.show_dependency
    in
    let expected_triggers =
      UnannotatedGlobalEnvironment.DependencyKey.KeySet.of_list expected_triggers
    in
    assert_equal
      ~printer
      expected_triggers
      (UnannotatedGlobalEnvironment.UpdateResult.triggered_dependencies update_result);
    post_actions >>| List.iter ~f:execute_action |> Option.value ~default:()
  in
  let dependency = UnannotatedGlobalEnvironment.TypeCheckSource (Reference.create "dep") in
  (* get_class_definition *)
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
    |}
    ~new_source:{|
      class Foo:
        x: str
    |}
    ~middle_actions:[`Get ("test.Foo", dependency, Some 1)]
    ~expected_triggers:[dependency]
    ();
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
    |}
    ~new_source:{|
      class Foo:
        x: str
    |}
    ~middle_actions:[`Get ("test.Missing", dependency, None)]
    ~expected_triggers:[]
    ();
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
    |}
    ~new_source:{|
      class Unrelated:
        x: int
      class Foo:
        x: int
    |}
    ~middle_actions:[`Get ("test.Foo", dependency, Some 1)]
    ~expected_triggers:[]
    ();

  (* First class definition wins *)
  assert_updates
    ~original_source:
      {|
      class Foo:
        x: int
      class Foo:
        x: int
        y: int
    |}
    ~new_source:{|
      class Unrelated:
        x: int
      class Foo:
        x: int
    |}
    ~middle_actions:[`Get ("test.Foo", dependency, Some 1)]
    ~expected_triggers:[]
    ();

  (* class_exists *)
  assert_updates
    ~new_source:{|
      class Foo:
        x: int
    |}
    ~middle_actions:[`Mem ("test.Foo", dependency, false)]
    ~expected_triggers:[dependency]
    ();
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
    |}
    ~middle_actions:[`Mem ("test.Foo", dependency, true)]
    ~expected_triggers:[dependency]
    ();
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
    |}
    ~new_source:{|
      class Foo:
        x: int
    |}
    ~middle_actions:[`Mem ("test.Foo", dependency, true)]
    ~expected_triggers:[]
    ();

  (* TODO(T53500184): need to add an existence-only dependency "kind" *)
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
    |}
    ~new_source:{|
      class Foo:
        x: str
    |}
    ~middle_actions:[`Mem ("test.Foo", dependency, true)]
    ~expected_triggers:[dependency]
    ();

  (* all_classes *)
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
      class Bar:
        y: str
    |}
    ~new_source:{|
      class Foo:
        x: str
    |}
    ~middle_actions:[`AllClasses ["test.Bar"; "test.Foo"]]
    ~expected_triggers:[]
    ~post_actions:[`AllClasses ["test.Foo"]]
    ();

  (* get_unannotated_global *)
  let dependency = UnannotatedGlobalEnvironment.AliasRegister (Reference.create "dep") in
  assert_updates
    ~original_source:{|
      x: int = 7
    |}
    ~new_source:{|
      x: int = 9
    |}
    ~middle_actions:
      [
        `Global
          ( Reference.create "test.x",
            dependency,
            Some
              (UnannotatedGlobalEnvironment.SimpleAssign
                 {
                   explicit_annotation = Some (parse_single_expression "int");
                   value = parse_single_expression "7";
                 }) );
      ]
    ~expected_triggers:[dependency]
    ~post_actions:
      [
        `Global
          ( Reference.create "test.x",
            dependency,
            Some
              (UnannotatedGlobalEnvironment.SimpleAssign
                 {
                   explicit_annotation = Some (parse_single_expression "int");
                   value = parse_single_expression "9";
                 }) );
      ]
    ();
  assert_updates
    ~original_source:{|
      import target.member as alias
    |}
    ~new_source:{|
      import target.member as new_alias
    |}
    ~middle_actions:
      [
        `Global
          ( Reference.create "test.alias",
            dependency,
            Some (UnannotatedGlobalEnvironment.Imported (Reference.create "target.member")) );
      ]
    ~expected_triggers:[dependency]
    ~post_actions:[`Global (Reference.create "test.alias", dependency, None)]
    ();
  assert_updates
    ~original_source:{|
      from target import member, other_member
    |}
    ~new_source:{|
      from target import other_member, member
    |}
    ~middle_actions:
      [
        `Global
          ( Reference.create "test.member",
            dependency,
            Some (UnannotatedGlobalEnvironment.Imported (Reference.create "target.member")) );
        `Global
          ( Reference.create "test.other_member",
            dependency,
            Some (UnannotatedGlobalEnvironment.Imported (Reference.create "target.other_member"))
          );
      ]
      (* Location insensitive *)
    ~expected_triggers:[]
    ~post_actions:
      [
        `Global
          ( Reference.create "test.member",
            dependency,
            Some (UnannotatedGlobalEnvironment.Imported (Reference.create "target.member")) );
        `Global
          ( Reference.create "test.other_member",
            dependency,
            Some (UnannotatedGlobalEnvironment.Imported (Reference.create "target.other_member"))
          );
      ]
    ();

  (* Don't infer * as a real thing *)
  assert_updates
    ~original_source:{|
      from target import *
    |}
    ~middle_actions:[`Global (Reference.create "test.*", dependency, None)]
    ~expected_triggers:[]
    ();

  (* We do not pick up tuple assignments yet, as they are not relevant for aliases *)
  assert_updates
    ~original_source:{|
      X, Y, Z = int, str, bool
    |}
    ~middle_actions:
      [
        `Global (Reference.create "test.X", dependency, None);
        `Global (Reference.create "test.Y", dependency, None);
        `Global (Reference.create "test.Z", dependency, None);
      ]
    ~expected_triggers:[]
    ();

  (* First global wins. Kind of weird behavior, but that's the current approach so sticking with it
     for now *)
  assert_updates
    ~original_source:{|
      X = int
      X = str
    |}
    ~new_source:{|
      X = int
      X = str
    |}
    ~middle_actions:
      [
        `Global
          ( Reference.create "test.X",
            dependency,
            Some
              (UnannotatedGlobalEnvironment.SimpleAssign
                 { explicit_annotation = None; value = parse_single_expression "int" }) );
      ]
    ~expected_triggers:[]
    ();

  (* Keep different dependencies straight *)
  let alias_dependency =
    UnannotatedGlobalEnvironment.AliasRegister (Reference.create "same_dep")
  in
  let check_dependency =
    UnannotatedGlobalEnvironment.TypeCheckSource (Reference.create "same_dep")
  in
  assert_updates
    ~original_source:{|
      class Foo:
        x: int
    |}
    ~new_source:{|
      class Foo:
        x: str
    |}
    ~middle_actions:
      [`Get ("test.Foo", alias_dependency, Some 1); `Get ("test.Foo", check_dependency, Some 1)]
    ~expected_triggers:[check_dependency; alias_dependency]
    ();
  ()


let () =
  "environment"
  >::: ["simple_registration" >:: test_simple_registration; "updates" >:: test_updates]
  |> Test.run
