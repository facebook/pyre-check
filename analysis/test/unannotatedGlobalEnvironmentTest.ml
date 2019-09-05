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
      | `Get (class_name, dependency) ->
          UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
            read_only
            ~dependency
            class_name
          |> ignore
      | `Mem (class_name, dependency, expectation) ->
          UnannotatedGlobalEnvironment.ReadOnly.class_exists read_only ~dependency class_name
          |> assert_equal expectation
      | `AllClasses expectation ->
          UnannotatedGlobalEnvironment.ReadOnly.all_classes read_only
          |> assert_equal ~printer:(List.to_string ~f:Fn.id) expectation
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
      SharedMemoryKeys.ReferenceDependencyKey.KeySet.elements set
      |> List.to_string ~f:Reference.show
    in
    assert_equal
      ~printer
      (SharedMemoryKeys.ReferenceDependencyKey.KeySet.of_list expected_triggers)
      (UnannotatedGlobalEnvironment.UpdateResult.triggered_dependencies update_result);
    post_actions >>| List.iter ~f:execute_action |> Option.value ~default:()
  in
  let dependency = Reference.create "dep" in
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
    ~middle_actions:[`Get ("test.Foo", dependency)]
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
    ~middle_actions:[`Get ("test.Missing", dependency)]
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
    ~middle_actions:[`Get ("test.Foo", dependency)]
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
  ()


let () =
  "environment"
  >::: ["simple_registration" >:: test_simple_registration; "updates" >:: test_updates]
  |> Test.run
