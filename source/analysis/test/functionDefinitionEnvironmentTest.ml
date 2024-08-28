(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Analysis
open Pyre
open Test

let test_updates =
  let assert_updates
      ?(original_sources = [])
      ?(new_sources = [])
      ~middle_actions
      ~expected_triggers
      ?post_actions
      context
    =
    Memory.reset_shared_memory ();
    let project =
      ScratchProject.setup
        ~include_typeshed_stubs:false
        ~track_dependencies:true
        ~in_memory:false
        original_sources
        ~context
    in
    let configuration = ScratchProject.configuration_of project in
    let read_only =
      ScratchProject.errors_environment project
      |> ErrorsEnvironment.Testing.ReadOnly.function_definition_environment
    in
    let execute_action (qualifier, dependency, expectation) =
      let printer = [%show: Reference.t list] in
      FunctionDefinitionEnvironment.ReadOnly.define_names_of_qualifier
        read_only
        ~dependency
        qualifier
      |> List.sort ~compare:Reference.compare
      |> assert_equal ~printer (List.sort ~compare:Reference.compare expectation)
    in
    List.iter middle_actions ~f:execute_action;
    List.iter original_sources ~f:(fun (relative, _) ->
        ScratchProject.delete_from_local_root project ~relative);
    List.iter new_sources ~f:(fun (relative, content) ->
        ScratchProject.add_to_local_root project ~relative content);
    let update_result =
      let { Configuration.Analysis.local_root; _ } = configuration in
      List.map new_sources ~f:(fun (relative, _) ->
          Test.relative_artifact_path ~root:local_root ~relative
          |> ArtifactPath.Event.(create ~kind:Kind.Unknown))
      |> ScratchProject.update_environment project
      |> ErrorsEnvironment.Testing.UpdateResult.function_definition_environment
    in
    let printer set =
      SharedMemoryKeys.DependencyKey.RegisteredSet.elements set
      |> List.map ~f:SharedMemoryKeys.DependencyKey.get_key
      |> List.to_string ~f:SharedMemoryKeys.show_dependency
    in
    let expected_triggers =
      SharedMemoryKeys.DependencyKey.RegisteredSet.of_list expected_triggers
    in
    assert_equal
      ~printer
      expected_triggers
      (FunctionDefinitionEnvironment.UpdateResult.locally_triggered_dependencies update_result);
    post_actions >>| List.iter ~f:execute_action |> Option.value ~default:()
  in
  let dependency =
    SharedMemoryKeys.DependencyKey.Registry.register (TypeCheckDefine (Reference.create "dep"))
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_updates
           ~original_sources:
             [
               ( "test.py",
                 {|
                    def f():
                        x: int = 42
                  |}
               );
             ]
           ~new_sources:
             [
               ( "test.py",
                 {|
                    def f():
                        x: int = 43
                  |}
               );
             ]
           ~middle_actions:[!&"test", dependency, [!&"test.$toplevel"; !&"test.f"]]
           ~expected_triggers:[dependency]
           ~post_actions:[!&"test", dependency, [!&"test.$toplevel"; !&"test.f"]];
      (* Test the behavior of nesting in classes and in defines. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_updates
           ~original_sources:
             [
               ( "test.py",
                 {|
                    def outer():
                        class NestedClass: pass
                        def nested():
                            def nested2(): ...
                  |}
               );
             ]
           ~new_sources:
             [
               ( "test.py",
                 {|
                    class OuterClass():
                        def method(self):
                            def nested(): ...
                        class InnerClass():
                            pass
                  |}
               );
             ]
           ~middle_actions:
             [
               ( !&"test",
                 dependency,
                 [
                   !&"test.$toplevel";
                   !&"test.outer";
                   !&"test.outer.nested";
                   !&"test.outer.nested.nested2";
                 ] );
             ]
           ~expected_triggers:[dependency]
           ~post_actions:
             [
               ( !&"test",
                 dependency,
                 [
                   !&"test.$toplevel";
                   !&"test.OuterClass.$class_toplevel";
                   !&"test.OuterClass.method";
                   !&"test.OuterClass.method.nested";
                   !&"test.OuterClass.InnerClass.$class_toplevel";
                 ] );
             ];
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_updates
           ~original_sources:
             [
               "c.py", {|
                   x = 5
               |};
               ( "test.py",
                 {|
                   from c import *

                   def f():
                       x: C = C()
                 |}
               );
             ]
           ~new_sources:
             [
               "c.py", {|
                 class C: pass
               |};
               ( "test.py",
                 {|
                     from c import *

                     def f():
                         x: C = C()
                  |}
               );
             ]
           ~middle_actions:[!&"test", dependency, [!&"test.$toplevel"; !&"test.f"]]
           ~expected_triggers:[dependency]
           ~post_actions:[!&"test", dependency, [!&"test.$toplevel"; !&"test.f"]];
    ]


let () = Test.sanitized_module_name __MODULE__ >::: [test_updates] |> Test.run
