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

let test_simple_registration context =
  let assert_registers ?(source_name = "test") source name expected =
    let project =
      ScratchProject.setup [source_name ^ ".py", source] ~include_typeshed_stubs:false ~context
    in
    let ast_environment = ScratchProject.build_ast_environment project in
    let class_metadata_environment = ClassMetadataEnvironment.create ast_environment in
    let read_only = ClassMetadataEnvironment.cold_start class_metadata_environment in
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
         is_protocol = false;
         is_abstract = false;
         is_typed_dictionary = false;
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
         is_protocol = false;
         is_abstract = false;
         is_typed_dictionary = false;
       });
  assert_registers
    {|
    class C(metaclass=abc.ABCMeta):
      pass
  |}
    "test.C"
    (Some
       {
         successors = ["object"];
         is_test = false;
         is_final = false;
         extends_placeholder_stub_class = false;
         is_protocol = false;
         is_abstract = true;
         is_typed_dictionary = false;
       });
  assert_registers
    {|
      class C(typing.Protocol):
        pass
    |}
    "test.C"
    (Some
       {
         successors = ["object"];
         is_test = false;
         is_final = false;
         extends_placeholder_stub_class = false;
         is_protocol = true;
         is_abstract = false;
         is_typed_dictionary = false;
       });
  assert_registers
    ~source_name:"unittest"
    {|
      class TestCase:
        pass
    |}
    "unittest.TestCase"
    (Some
       {
         successors = ["object"];
         is_test = true;
         is_final = false;
         extends_placeholder_stub_class = false;
         is_protocol = false;
         is_abstract = false;
         is_typed_dictionary = false;
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
        ~in_memory:false
        original_sources
        ~context
    in
    let ast_environment = ScratchProject.build_ast_environment project in
    let configuration = ScratchProject.configuration_of project in
    let class_metadata_environment = ClassMetadataEnvironment.create ast_environment in
    let read_only = ClassMetadataEnvironment.cold_start class_metadata_environment in
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
      PyrePath.create_relative ~root:local_root ~relative |> PyrePath.absolute |> Core.Unix.remove
    in
    let add_file
        { ScratchProject.configuration = { Configuration.Analysis.local_root; _ }; _ }
        ~relative
        content
      =
      let content = trim_extra_indentation content in
      let file = File.create ~content (PyrePath.create_relative ~root:local_root ~relative) in
      File.write file
    in
    List.iter original_sources ~f:(fun (path, _) -> delete_file project path);
    List.iter new_sources ~f:(fun (relative, content) -> add_file project ~relative content);
    let update_result =
      let { ScratchProject.module_tracker; _ } = project in
      let { Configuration.Analysis.local_root; _ } = configuration in
      let paths =
        List.map new_sources ~f:(fun (relative, _) ->
            Test.relative_artifact_path ~root:local_root ~relative)
      in
      ModuleTracker.update ~paths module_tracker
      |> (fun updates -> AstEnvironment.Update updates)
      |> ClassMetadataEnvironment.update_this_and_all_preceding_environments
           class_metadata_environment
           ~scheduler:(Test.mock_scheduler ())
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
      (ClassMetadataEnvironment.UpdateResult.locally_triggered_dependencies update_result);
    post_actions >>| List.iter ~f:execute_action |> Option.value ~default:()
  in
  let dependency =
    SharedMemoryKeys.DependencyKey.Registry.register (TypeCheckDefine (Reference.create "dep"))
  in
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
              is_protocol = false;
              is_abstract = false;
              is_typed_dictionary = false;
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
              is_protocol = false;
              is_abstract = false;
              is_typed_dictionary = false;
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
              is_protocol = false;
              is_abstract = false;
              is_typed_dictionary = false;
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
              is_protocol = false;
              is_abstract = false;
              is_typed_dictionary = false;
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
              is_protocol = false;
              is_abstract = false;
              is_typed_dictionary = false;
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
              is_protocol = false;
              is_abstract = false;
              is_typed_dictionary = false;
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
              is_protocol = false;
              is_abstract = false;
              is_typed_dictionary = false;
            } );
        ( "test.D",
          dependency,
          Some
            {
              successors = ["test.C"; "object"];
              is_test = false;
              is_final = false;
              extends_placeholder_stub_class = false;
              is_protocol = false;
              is_abstract = false;
              is_typed_dictionary = false;
            } );
        ( "test.B",
          dependency,
          Some
            {
              successors = ["test.A"; "object"];
              is_test = false;
              is_final = false;
              extends_placeholder_stub_class = false;
              is_protocol = false;
              is_abstract = false;
              is_typed_dictionary = false;
            } );
        ( "test.E",
          dependency,
          Some
            {
              successors = ["test.D"; "test.C"; "test.A"; "object"];
              is_test = false;
              is_final = false;
              extends_placeholder_stub_class = false;
              is_protocol = false;
              is_abstract = false;
              is_typed_dictionary = false;
            } );
        ( "test.F",
          dependency,
          Some
            {
              successors = ["test.B"; "test.A"; "object"];
              is_test = false;
              is_final = false;
              extends_placeholder_stub_class = true;
              is_protocol = false;
              is_abstract = false;
              is_typed_dictionary = false;
            } );
      ]
    ~expected_triggers:[]
    ();

  (* Addition should trigger previous failed reads *)
  assert_updates
    ~original_sources:["test.py", {|
      |}]
    ~new_sources:["test.py", {|
      class C:
        pass
      |}]
    ~middle_actions:["test.C", dependency, None]
    ~expected_triggers:[dependency]
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
              is_protocol = false;
              is_abstract = false;
              is_typed_dictionary = false;
            } );
      ]
    ();
  ()


let () =
  "environment"
  >::: ["simple_registration" >:: test_simple_registration; "updates" >:: test_updates]
  |> Test.run
