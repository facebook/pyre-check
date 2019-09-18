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
    let ast_environment, ast_environment_update_result = ScratchProject.parse_sources project in
    let unannotated_global_environment =
      UnannotatedGlobalEnvironment.create (AstEnvironment.read_only ast_environment)
    in
    let alias_environment =
      AliasEnvironment.create
        (UnannotatedGlobalEnvironment.read_only unannotated_global_environment)
    in
    let class_hierarchy_environment =
      ClassHierarchyEnvironment.create (AliasEnvironment.read_only alias_environment)
    in
    let class_metadata_environment =
      ClassMetadataEnvironment.create
        (ClassHierarchyEnvironment.read_only class_hierarchy_environment)
    in
    let _ =
      UnannotatedGlobalEnvironment.update
        unannotated_global_environment
        ~scheduler:(mock_scheduler ())
        ~configuration:(Configuration.Analysis.create ())
        ~ast_environment_update_result
        (Reference.Set.singleton (Reference.create "test"))
      |> AliasEnvironment.update
           alias_environment
           ~scheduler:(mock_scheduler ())
           ~configuration:(Configuration.Analysis.create ())
      |> ClassHierarchyEnvironment.update
           class_hierarchy_environment
           ~scheduler:(mock_scheduler ())
           ~configuration:(Configuration.Analysis.create ())
      |> ClassMetadataEnvironment.update
           class_metadata_environment
           ~scheduler:(mock_scheduler ())
           ~configuration:(Configuration.Analysis.create ())
    in
    let read_only = ClassMetadataEnvironment.read_only class_metadata_environment in
    let printer v =
      v >>| ClassMetadataEnvironment.show_class_metadata |> Option.value ~default:"none"
    in
    assert_equal
      ~printer
      expected
      (ClassMetadataEnvironment.ReadOnly.get_class_metadata read_only name)
  in
  assert_registers
    {|
    class C:
      pass
  |}
    "test.C"
    (Some
       {
         successors = ["object"];
         is_test = false;
         is_final = false;
         extends_placeholder_stub_class = false;
       });
  assert_registers
    {|
    class D:
     pass
    class C(D):
      pass
  |}
    "test.C"
    (Some
       {
         successors = ["test.D"; "object"];
         is_test = false;
         is_final = false;
         extends_placeholder_stub_class = false;
       });
  ()


let test_updates context =
  let assert_updates
      ?(original_sources = [])
      ?(new_sources = [])
      ~middle_actions
      ~expected_triggers
      ?post_actions
      ()
    =
    Memory.reset_shared_memory ();
    let project =
      ScratchProject.setup
        ~include_typeshed_stubs:false
        ~incremental_style:FineGrained
        original_sources
        ~context
    in
    let ast_environment, ast_environment_update_result = ScratchProject.parse_sources project in
    let configuration = ScratchProject.configuration_of project in
    let unannotated_global_environment =
      UnannotatedGlobalEnvironment.create (AstEnvironment.read_only ast_environment)
    in
    let alias_environment =
      AliasEnvironment.create
        (UnannotatedGlobalEnvironment.read_only unannotated_global_environment)
    in
    let class_hierarchy_environment =
      ClassHierarchyEnvironment.create (AliasEnvironment.read_only alias_environment)
    in
    let class_metadata_environment =
      ClassMetadataEnvironment.create
        (ClassHierarchyEnvironment.read_only class_hierarchy_environment)
    in
    let update ~ast_environment_update_result () =
      let scheduler = Test.mock_scheduler () in
      let qualifiers = AstEnvironment.UpdateResult.reparsed ast_environment_update_result in
      UnannotatedGlobalEnvironment.update
        unannotated_global_environment
        ~scheduler
        ~configuration
        ~ast_environment_update_result
        (Reference.Set.of_list qualifiers)
      |> AliasEnvironment.update alias_environment ~scheduler ~configuration
      |> ClassHierarchyEnvironment.update class_hierarchy_environment ~scheduler ~configuration
      |> ClassMetadataEnvironment.update class_metadata_environment ~scheduler ~configuration
    in
    let _ = update ~ast_environment_update_result () in
    let read_only = ClassMetadataEnvironment.read_only class_metadata_environment in
    let execute_action (class_name, dependency, expectation) =
      let printer v =
        v >>| ClassMetadataEnvironment.show_class_metadata |> Option.value ~default:"none"
      in
      ClassMetadataEnvironment.ReadOnly.get_class_metadata read_only ~dependency class_name
      |> assert_equal ~printer expectation
    in
    List.iter middle_actions ~f:execute_action;
    let delete_file
        { ScratchProject.configuration = { Configuration.Analysis.local_root; _ }; _ }
        relative
      =
      Path.create_relative ~root:local_root ~relative |> Path.absolute |> Core.Unix.remove
    in
    let add_file
        { ScratchProject.configuration = { Configuration.Analysis.local_root; _ }; _ }
        ~relative
        content
      =
      let content = trim_extra_indentation content in
      let file = File.create ~content (Path.create_relative ~root:local_root ~relative) in
      File.write file
    in
    List.iter original_sources ~f:(fun (path, _) -> delete_file project path);
    List.iter new_sources ~f:(fun (relative, content) -> add_file project ~relative content);
    let ast_environment_update_result =
      let { ScratchProject.module_tracker; _ } = project in
      let { Configuration.Analysis.local_root; _ } = configuration in
      let paths =
        List.map new_sources ~f:(fun (relative, _) ->
            Path.create_relative ~root:local_root ~relative)
      in
      ModuleTracker.update ~configuration ~paths module_tracker
      |> (fun updates -> AstEnvironment.Update updates)
      |> AstEnvironment.update ~configuration ~scheduler:(mock_scheduler ()) ast_environment
    in
    let update_result = update ~ast_environment_update_result () in
    let printer set =
      ClassMetadataEnvironment.DependencyKey.KeySet.elements set
      |> List.to_string ~f:ClassMetadataEnvironment.show_dependency
    in
    let expected_triggers =
      ClassMetadataEnvironment.DependencyKey.KeySet.of_list expected_triggers
    in
    assert_equal
      ~printer
      expected_triggers
      (ClassMetadataEnvironment.UpdateResult.triggered_dependencies update_result);
    post_actions >>| List.iter ~f:execute_action |> Option.value ~default:()
  in
  let dependency = ClassMetadataEnvironment.TypeCheckSource (Reference.create "dep") in
  assert_updates
    ~original_sources:["test.py", {|
      class C:
        pass
      |}]
    ~new_sources:["test.py", {|
      class C:
        pass
      |}]
    ~middle_actions:
      [
        ( "test.C",
          dependency,
          Some
            {
              successors = ["object"];
              is_test = false;
              is_final = false;
              extends_placeholder_stub_class = false;
            } );
      ]
    ~expected_triggers:[]
    ~post_actions:
      [
        ( "test.C",
          dependency,
          Some
            {
              successors = ["object"];
              is_test = false;
              is_final = false;
              extends_placeholder_stub_class = false;
            } );
      ]
    ();
  assert_updates
    ~original_sources:["test.py", {|
      class C:
        pass
      |}]
    ~new_sources:["test.py", {|
      class D:
       pass
      class C(D):
        pass
      |}]
    ~middle_actions:
      [
        ( "test.C",
          dependency,
          Some
            {
              successors = ["object"];
              is_test = false;
              is_final = false;
              extends_placeholder_stub_class = false;
            } );
      ]
    ~expected_triggers:[dependency]
    ~post_actions:
      [
        ( "test.C",
          dependency,
          Some
            {
              successors = ["test.D"; "object"];
              is_test = false;
              is_final = false;
              extends_placeholder_stub_class = false;
            } );
      ]
    ();
  assert_updates
    ~original_sources:
      [
        "test.py", {|
      from placeholder import Base
      class C(Base):
        pass
      |};
        "placeholder.pyi", {|
      # pyre-placeholder-stub
      |};
      ]
    ~new_sources:
      [
        "test.py", {|
      from placeholder import Base
      class C(Base):
        pass
      |};
        "placeholder.pyi", {|
       class Base:
        pass
      |};
      ]
    ~middle_actions:
      [
        ( "test.C",
          dependency,
          Some
            {
              successors = ["object"];
              is_test = false;
              is_final = false;
              extends_placeholder_stub_class = true;
            } );
      ]
    ~expected_triggers:[dependency]
    ~post_actions:
      [
        ( "test.C",
          dependency,
          Some
            {
              successors = ["placeholder.Base"; "object"];
              is_test = false;
              is_final = false;
              extends_placeholder_stub_class = false;
            } );
      ]
    ();
  assert_updates
    ~original_sources:
      [
        ( "test.py",
          {|
       from placeholder_stub import MadeUpClass
       class A: pass
       class B(A): pass
       class C:
         def __init__(self):
           self.x = 3
       class D(C):
         def __init__(self):
           self.y = 4
         D.z = 5
       class E(D, A): pass
       class F(B, MadeUpClass, A): pass
      |}
        );
        "placeholder_stub.pyi", {|
      # pyre-placeholder-stub
      |};
      ]
    ~new_sources:[]
    ~middle_actions:
      [
        ( "test.C",
          dependency,
          Some
            {
              successors = ["object"];
              is_test = false;
              is_final = false;
              extends_placeholder_stub_class = false;
            } );
        ( "test.D",
          dependency,
          Some
            {
              successors = ["test.C"; "object"];
              is_test = false;
              is_final = false;
              extends_placeholder_stub_class = false;
            } );
        ( "test.B",
          dependency,
          Some
            {
              successors = ["test.A"; "object"];
              is_test = false;
              is_final = false;
              extends_placeholder_stub_class = false;
            } );
        ( "test.E",
          dependency,
          Some
            {
              successors = ["test.D"; "test.C"; "test.A"; "object"];
              is_test = false;
              is_final = false;
              extends_placeholder_stub_class = false;
            } );
        ( "test.F",
          dependency,
          Some
            {
              successors = ["test.B"; "test.A"; "object"];
              is_test = false;
              is_final = false;
              extends_placeholder_stub_class = true;
            } );
      ]
    ~expected_triggers:[]
    ();

  ()


let () =
  "environment"
  >::: ["simple_registration" >:: test_simple_registration; "updates" >:: test_updates]
  |> Test.run
