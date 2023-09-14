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
    let read_only =
      ScratchProject.errors_environment project
      |> ErrorsEnvironment.Testing.ReadOnly.class_metadata_environment
    in
    let printer v =
      v >>| ClassSuccessorMetadataEnvironment.show_class_metadata |> Option.value ~default:"none"
    in
    assert_equal
      ~printer
      expected
      (ClassSuccessorMetadataEnvironment.ReadOnly.get_class_metadata read_only name)
  in
  assert_registers
    {|
    class C:
      pass
  |}
    "test.C"
    (Some
       {
         successors = Some ["object"];
         is_test = false;
         is_mock = false;
         is_final = false;
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
         successors = Some ["test.D"; "object"];
         is_test = false;
         is_mock = false;
         is_final = false;
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
         successors = Some ["object"];
         is_test = false;
         is_mock = false;
         is_final = false;
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
         successors = Some ["object"];
         is_test = false;
         is_mock = false;
         is_final = false;
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
         successors = Some ["object"];
         is_test = true;
         is_mock = false;
         is_final = false;
         is_protocol = false;
         is_abstract = false;
         is_typed_dictionary = false;
       });
  assert_registers
    ~source_name:"mock"
    {|
      class NonCallableMock:
        pass
    |}
    "mock.NonCallableMock"
    (Some
       {
         successors = Some ["object"];
         is_test = false;
         is_mock = true;
         is_final = false;
         is_protocol = false;
         is_abstract = false;
         is_typed_dictionary = false;
       });
  assert_registers
    ~source_name:"unittest/mock"
    {|
      class Base:
        pass
    |}
    "unittest.mock.Base"
    (Some
       {
         successors = Some ["object"];
         is_test = false;
         is_mock = true;
         is_final = false;
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
        ~track_dependencies:true
        ~in_memory:false
        original_sources
        ~context
    in
    let configuration = ScratchProject.configuration_of project in
    let read_only =
      ScratchProject.errors_environment project
      |> ErrorsEnvironment.Testing.ReadOnly.class_metadata_environment
    in
    let execute_action (class_name, dependency, expectation) =
      let printer v =
        v >>| ClassSuccessorMetadataEnvironment.show_class_metadata |> Option.value ~default:"none"
      in
      ClassSuccessorMetadataEnvironment.ReadOnly.get_class_metadata read_only ~dependency class_name
      |> assert_equal ~printer expectation
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
      |> ScratchProject.update_environment project ~scheduler:(Test.mock_scheduler ())
      |> ErrorsEnvironment.Testing.UpdateResult.class_metadata_environment
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
      (ClassSuccessorMetadataEnvironment.UpdateResult.locally_triggered_dependencies update_result);
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
              successors = Some ["object"];
              is_test = false;
              is_mock = false;
              is_final = false;
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
              successors = Some ["object"];
              is_test = false;
              is_mock = false;
              is_final = false;
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
              successors = Some ["object"];
              is_test = false;
              is_mock = false;
              is_final = false;
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
              successors = Some ["test.D"; "object"];
              is_test = false;
              is_mock = false;
              is_final = false;
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
              successors = Some ["object"];
              is_test = false;
              is_mock = false;
              is_final = false;
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
              successors = Some ["placeholder.Base"; "object"];
              is_test = false;
              is_mock = false;
              is_final = false;
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
              successors = Some ["object"];
              is_test = false;
              is_mock = false;
              is_final = false;
              is_protocol = false;
              is_abstract = false;
              is_typed_dictionary = false;
            } );
        ( "test.D",
          dependency,
          Some
            {
              successors = Some ["test.C"; "object"];
              is_test = false;
              is_mock = false;
              is_final = false;
              is_protocol = false;
              is_abstract = false;
              is_typed_dictionary = false;
            } );
        ( "test.B",
          dependency,
          Some
            {
              successors = Some ["test.A"; "object"];
              is_test = false;
              is_mock = false;
              is_final = false;
              is_protocol = false;
              is_abstract = false;
              is_typed_dictionary = false;
            } );
        ( "test.E",
          dependency,
          Some
            {
              successors = Some ["test.D"; "test.C"; "test.A"; "object"];
              is_test = false;
              is_mock = false;
              is_final = false;
              is_protocol = false;
              is_abstract = false;
              is_typed_dictionary = false;
            } );
        ( "test.F",
          dependency,
          Some
            {
              successors = Some ["test.B"; "test.A"; "object"];
              is_test = false;
              is_mock = false;
              is_final = false;
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
              successors = Some ["object"];
              is_test = false;
              is_mock = false;
              is_final = false;
              is_protocol = false;
              is_abstract = false;
              is_typed_dictionary = false;
            } );
      ]
    ();
  ()


let test_is_transitive_successors context =
  let assert_is_successor ~source ~expected ~placeholder_subclass_extends_all ~target predecessor =
    let project =
      ScratchProject.setup
        ["test.py", source; "my_placeholder_stub.pyi", "# pyre-placeholder-stub"]
        ~include_typeshed_stubs:true
        ~include_helper_builtins:false
        ~context
    in
    let read_only =
      ScratchProject.errors_environment project
      |> ErrorsEnvironment.Testing.ReadOnly.class_metadata_environment
    in
    assert_equal
      ~cmp:Bool.equal
      ~printer:Bool.to_string
      expected
      (ClassSuccessorMetadataEnvironment.ReadOnly.is_transitive_successor
         read_only
         ~placeholder_subclass_extends_all
         ~target
         predecessor)
  in

  assert_is_successor
    ~source:{|
    class A: pass
  |}
    ~placeholder_subclass_extends_all:false
    ~target:"test.A"
    "test.A"
    ~expected:true;
  assert_is_successor
    ~source:{|
    class A: pass
  |}
    ~placeholder_subclass_extends_all:true
    ~target:"test.A"
    "test.A"
    ~expected:true;

  assert_is_successor
    ~source:{|
    class A: pass
    class B: pass
  |}
    ~placeholder_subclass_extends_all:false
    ~target:"test.A"
    "test.B"
    ~expected:false;
  assert_is_successor
    ~source:{|
    class A: pass
    class B: pass
  |}
    ~placeholder_subclass_extends_all:true
    ~target:"test.A"
    "test.B"
    ~expected:false;

  assert_is_successor
    ~source:{|
    class A: pass
    class B(A): pass
  |}
    ~placeholder_subclass_extends_all:false
    ~target:"test.A"
    "test.B"
    ~expected:true;
  assert_is_successor
    ~source:{|
    class A: pass
    class B(A): pass
  |}
    ~placeholder_subclass_extends_all:true
    ~target:"test.A"
    "test.B"
    ~expected:true;

  assert_is_successor
    ~source:{|
    class A: pass
    class B(A): pass
    class C(B): pass
  |}
    ~placeholder_subclass_extends_all:false
    ~target:"test.A"
    "test.C"
    ~expected:true;
  assert_is_successor
    ~source:{|
    class A: pass
    class B(A): pass
    class C(B): pass
  |}
    ~placeholder_subclass_extends_all:true
    ~target:"test.A"
    "test.C"
    ~expected:true;

  assert_is_successor
    ~source:{|
    class A: pass
    class B(A): pass
    class C: pass
    class D(C, B): pass
  |}
    ~placeholder_subclass_extends_all:false
    ~target:"test.A"
    "test.D"
    ~expected:true;
  assert_is_successor
    ~source:{|
    class A: pass
    class B(A): pass
    class C: pass
    class D(C, B): pass
  |}
    ~placeholder_subclass_extends_all:true
    ~target:"test.A"
    "test.D"
    ~expected:true;

  assert_is_successor
    ~source:{|
    from my_placeholder_stub import A
    class B: pass
    class C(A): pass
  |}
    ~placeholder_subclass_extends_all:true
    ~target:"test.B"
    "test.C"
    ~expected:true;
  assert_is_successor
    ~source:{|
    from my_placeholder_stub import A
    class B: pass
    class C(A): pass
  |}
    ~placeholder_subclass_extends_all:false
    ~target:"test.B"
    "test.C"
    ~expected:false;
  ()


let test_least_upper_bound context =
  let assert_least_upper_bound ~source ~expected left right =
    let project =
      ScratchProject.setup
        ["test.py", source]
        ~include_typeshed_stubs:false
        ~include_helper_builtins:false
        ~context
    in
    let read_only =
      ScratchProject.errors_environment project
      |> ErrorsEnvironment.Testing.ReadOnly.class_metadata_environment
    in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: Type.Primitive.t option]
      ~printer:(fun bound -> Sexp.to_string_hum ([%sexp_of: Type.Primitive.t option] bound))
      expected
      (ClassSuccessorMetadataEnvironment.ReadOnly.least_upper_bound read_only left right)
  in
  let source0 =
    {|
    class A0(A3): pass
    class A1(A3): pass
    class A2: pass
    class A3: pass
    class A4(A2): pass
    class A5: pass
    class Bottom(A4, A2, A1, A0): pass
  |}
  in
  let source1 =
    {|
    class A0(A3, A2): pass
    class A1(A3, A2): pass
    class A2: pass
    class A3: pass
  |}
  in
  let source2 =
    {|
    class One:
      pass
    class Two:
      pass
    class Zero(Two, One):
      pass
  |}
  in
  assert_least_upper_bound "test.A3" "test.A1" ~source:source0 ~expected:(Some "test.A3");
  assert_least_upper_bound "test.A4" "test.Bottom" ~source:source0 ~expected:(Some "test.A4");
  assert_least_upper_bound "test.A0" "test.A2" ~source:source0 ~expected:(Some "object");
  assert_least_upper_bound "test.A0" "test.A1" ~source:source1 ~expected:(Some "test.A3");
  assert_least_upper_bound "test.One" "test.Two" ~source:source2 ~expected:(Some "object");
  ()


let assert_overlay_parents ~context ~overlay ~qualified_class_name expected_successors =
  match
    ClassSuccessorMetadataEnvironment.ReadOnly.get_class_metadata
      (ClassSuccessorMetadataEnvironment.Overlay.read_only overlay)
      qualified_class_name
  with
  | Some { ClassSuccessorMetadataEnvironment.successors = Some successors; _ } ->
      assert_equal ~ctxt:context ~printer:[%show: Identifier.t list] expected_successors successors
  | _ -> failwith ("Failed to look up " ^ qualified_class_name)


let assert_overlay_state ~context ~overlay qualified_class_name_successors_pairs =
  let assert_pair (qualified_class_name, expected_successors) =
    assert_overlay_parents ~context ~overlay ~qualified_class_name expected_successors
  in
  List.iter qualified_class_name_successors_pairs ~f:assert_pair


let test_overlay_dependency_filtering context =
  let a_code_with_A_base base_type =
    Format.asprintf
      {|
        class Base0: pass

        class Base1: pass

        Alias = %s

        class A(%s): pass
      |}
      base_type
      base_type
    |> trim_extra_indentation
  in
  let b_code = trim_extra_indentation {|
    from a import Alias

    class B(Alias): pass
  |} in
  let c_code =
    trim_extra_indentation
      {|
        from a import A
        from b import B

        class Ca(A): pass

        class Cb(B): pass
      |}
  in
  let project =
    ScratchProject.setup
      ~context
      ["a.py", a_code_with_A_base "Base0"; "b.py", b_code; "c.py", c_code]
  in
  let parent =
    ScratchProject.errors_environment project
    |> ErrorsEnvironment.Testing.ReadOnly.class_metadata_environment
  in
  let overlay = ClassSuccessorMetadataEnvironment.Overlay.create parent in
  (* Initially, nothing inherits from Base1 *)
  assert_overlay_state
    ~context
    ~overlay
    [
      "a.A", ["a.Base0"; "object"];
      "b.B", ["a.Base0"; "object"];
      "c.Ca", ["a.A"; "a.Base0"; "object"];
      "c.Cb", ["b.B"; "a.Base0"; "object"];
    ];
  let local_root = ScratchProject.local_root_of project in
  (* Update just a.py so that A inherits from Base1 rather than Base0. This should not affect
     results for c.py at all, but should affect results for a.py *)
  ClassSuccessorMetadataEnvironment.Overlay.update_overlaid_code
    overlay
    ~code_updates:
      [
        ( Test.relative_artifact_path ~root:local_root ~relative:"a.py",
          ModuleTracker.Overlay.CodeUpdate.NewCode (a_code_with_A_base "Base1") );
      ]
  |> ignore;
  (* After updating just a.py, we should see the type error from int-vs-float mismatch. The overlay
     should see this update, but; "" c.py should behave exactly as before. *)
  assert_overlay_state
    ~context
    ~overlay
    [
      "a.A", ["a.Base1"; "object"];
      "b.B", ["a.Base0"; "object"];
      "c.Ca", ["a.A"; "a.Base0"; "object"];
      "c.Cb", ["b.B"; "a.Base0"; "object"];
    ];
  (* Add c.py to the overlay, without changing the actual code or including b.py in the overlay. to
     detect the overlay. But instances of b.B should still behave as if a.A were defined on disk,
     because we don't allow fanout to update the attribute table of b.B. *)
  ClassSuccessorMetadataEnvironment.Overlay.update_overlaid_code
    overlay
    ~code_updates:
      [
        ( Test.relative_artifact_path ~root:local_root ~relative:"c.py",
          ModuleTracker.Overlay.CodeUpdate.NewCode c_code );
      ]
  |> ignore;
  assert_overlay_state
    ~context
    ~overlay
    [
      "a.A", ["a.Base1"; "object"];
      "b.B", ["a.Base0"; "object"];
      "c.Ca", ["a.A"; "a.Base1"; "object"];
      "c.Cb", ["b.B"; "a.Base0"; "object"];
    ];
  ClassSuccessorMetadataEnvironment.Overlay.update_overlaid_code
    overlay
    ~code_updates:
      [
        ( Test.relative_artifact_path ~root:local_root ~relative:"b.py",
          ModuleTracker.Overlay.CodeUpdate.NewCode b_code );
      ]
  |> ignore;
  assert_overlay_state
    ~context
    ~overlay
    [
      "a.A", ["a.Base1"; "object"];
      "b.B", ["a.Base1"; "object"];
      "c.Ca", ["a.A"; "a.Base1"; "object"];
      "c.Cb", ["b.B"; "a.Base1"; "object"];
    ];
  ()


let test_overlay_propagation context =
  let sources =
    [
      ( "on_filesystem.py",
        {|
          class A: pass
          class B(A): pass
          class C: pass
        |} );
      ( "in_overlay.py",
        {|
          import on_filesystem

          class D(on_filesystem.B): pass
        |} );
    ]
  in
  let project = ScratchProject.setup ~context ~in_memory:false sources in
  let parent =
    ScratchProject.errors_environment project
    |> ErrorsEnvironment.Testing.ReadOnly.class_metadata_environment
  in
  let overlay = ClassSuccessorMetadataEnvironment.Overlay.create parent in
  (* Initially all metadata is from disk *)
  assert_overlay_state
    ~context
    ~overlay
    [
      "on_filesystem.A", ["object"];
      "on_filesystem.B", ["on_filesystem.A"; "object"];
      "on_filesystem.C", ["object"];
      "in_overlay.D", ["on_filesystem.B"; "on_filesystem.A"; "object"];
    ];
  (* After we update the overlay, the overlay should see consistent state *)
  let local_root = ScratchProject.local_root_of project in
  ClassSuccessorMetadataEnvironment.Overlay.update_overlaid_code
    overlay
    ~code_updates:
      [
        ( Test.relative_artifact_path ~root:local_root ~relative:"in_overlay.py",
          ModuleTracker.Overlay.CodeUpdate.NewCode
            (trim_extra_indentation
               {|
                 import on_filesystem

                 class D(on_filesystem.C): pass
               |})
        );
      ]
  |> ignore;
  assert_overlay_state
    ~context
    ~overlay
    [
      "on_filesystem.A", ["object"];
      "on_filesystem.B", ["on_filesystem.A"; "object"];
      "on_filesystem.C", ["object"];
      "in_overlay.D", ["on_filesystem.C"; "object"];
    ];
  (* Run an update on the parent, but do not propagate yet. The overlay-owned parts of the overlay
     environment should see old data about the on_filesystem module, while the parent-owned parts
     should see the updated information (this is a grey-box test, updating only the parent is an
     illegal operation with undefined behavior in production!) *)
  let update_code relative new_code =
    ScratchProject.delete_from_local_root project ~relative;
    ScratchProject.add_to_local_root project ~relative new_code;
    ()
  in
  update_code
    "on_filesystem.py"
    {|
      class A: pass
      class B: pass
      class C(A): pass
    |};
  let local_root = ScratchProject.local_root_of project in
  let parent_update_result =
    ScratchProject.update_environment
      project
      [
        (Test.relative_artifact_path ~root:local_root ~relative:"on_filesystem.py"
        |> ArtifactPath.Event.(create ~kind:Kind.CreatedOrChanged));
      ]
    |> ErrorsEnvironment.Testing.UpdateResult.class_metadata_environment
  in
  assert_overlay_state
    ~context
    ~overlay
    [
      "on_filesystem.A", ["object"];
      "on_filesystem.B", ["object"];
      "on_filesystem.C", ["on_filesystem.A"; "object"];
      "in_overlay.D", ["on_filesystem.C"; "object"];
    ];
  (* Now propagate the update. We should see a consistent state where the overlay-owned metadat afor
     in_overlay.py reflects the new on_filesystem.py source *)
  ClassSuccessorMetadataEnvironment.Overlay.propagate_parent_update overlay parent_update_result
  |> ignore;
  assert_overlay_state
    ~context
    ~overlay
    [
      "on_filesystem.A", ["object"];
      "on_filesystem.B", ["object"];
      "on_filesystem.C", ["on_filesystem.A"; "object"];
      "in_overlay.D", ["on_filesystem.C"; "on_filesystem.A"; "object"];
    ];
  ()


let () =
  "environment"
  >::: [
         "simple_registration" >:: test_simple_registration;
         "updates" >:: test_updates;
         "is_transitive_successors" >:: test_is_transitive_successors;
         "least_upper_bound" >:: test_least_upper_bound;
         "overlay_dependency_filtering" >:: test_overlay_dependency_filtering;
         "overlay_propagation" >:: test_overlay_propagation;
       ]
  |> Test.run
