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
  let assert_registers sources name ~expected_edges ~expected_extends_placeholder_stub =
    let project = ScratchProject.setup sources ~include_typeshed_stubs:false ~context in
    let read_only =
      ScratchProject.errors_environment project
      |> ErrorsEnvironment.Testing.ReadOnly.class_hierarchy_environment
    in
    let expected_edges =
      expected_edges
      >>| List.map ~f:(fun name ->
              { ClassHierarchy.Target.target = IndexTracker.index name; parameters = [] })
    in
    let printer v =
      let show_target_readable { ClassHierarchy.Target.target; parameters } =
        Format.asprintf
          "%s[%a]"
          (IndexTracker.annotation target)
          (Type.pp_parameters ~pp_type:Type.pp)
          parameters
      in
      v >>| List.to_string ~f:show_target_readable |> Option.value ~default:"none"
    in
    assert_equal
      ~printer
      expected_edges
      (ClassHierarchyEnvironment.ReadOnly.get_edges read_only (IndexTracker.index name));
    assert_equal
      ~printer:string_of_bool
      expected_extends_placeholder_stub
      (ClassHierarchyEnvironment.ReadOnly.extends_placeholder_stub
         read_only
         (IndexTracker.index name))
  in
  assert_registers
    ["test.py", {|
    class C:
      pass
  |}]
    "test.C"
    ~expected_edges:(Some ["object"])
    ~expected_extends_placeholder_stub:false;
  assert_registers
    ["test.py", {|
    class D:
     pass
    class C(D):
      pass
  |}]
    "test.C"
    ~expected_edges:(Some ["test.D"])
    ~expected_extends_placeholder_stub:false;
  assert_registers
    [
      "test.py", {|
    from placeholder import MadeUpClass
    class C(MadeUpClass):
     pass
  |};
      "placeholder.pyi", {|
      # pyre-placeholder-stub
  |};
    ]
    "test.C"
    ~expected_edges:(Some ["object"])
    ~expected_extends_placeholder_stub:true;
  assert_registers
    [
      ( "test.py",
        {|
    from placeholder import MadeUpClass
    class D(MadeUpClass):
     pass
    class C(D):
      pass
  |}
      );
      "placeholder.pyi", {|
      # pyre-placeholder-stub
  |};
    ]
    "test.C"
    ~expected_edges:(Some ["test.D"])
    ~expected_extends_placeholder_stub:false;
  assert_registers
    [
      ( "test.py",
        {|
    from placeholder import MadeUpClass
    class D:
     pass
    class C(D, MadeUpClass):
      pass
  |}
      );
      "placeholder.pyi", {|
      # pyre-placeholder-stub
  |};
    ]
    "test.C"
    ~expected_edges:(Some ["test.D"])
    ~expected_extends_placeholder_stub:true;
  ()


let test_inferred_generic_base context =
  let assert_registers source name expected =
    let project =
      ScratchProject.setup ["test.py", source] ~context ~incremental_style:FineGrained
    in
    let read_only =
      ScratchProject.errors_environment project
      |> ErrorsEnvironment.Testing.ReadOnly.class_hierarchy_environment
    in
    let expected =
      expected
      >>| List.map ~f:(fun (name, concretes) ->
              {
                ClassHierarchy.Target.target = IndexTracker.index name;
                parameters = List.map concretes ~f:(fun single -> Type.Parameter.Single single);
              })
    in
    let printer v =
      let show_target_readable { ClassHierarchy.Target.target; parameters } =
        (*Printf.sprintf*)
        (*"%s[%s]"*)
        Type.show (Type.parametric (IndexTracker.annotation target) parameters)
      in
      v >>| List.to_string ~f:show_target_readable |> Option.value ~default:"none"
    in
    assert_equal
      ~printer
      expected
      (ClassHierarchyEnvironment.ReadOnly.get_edges read_only (IndexTracker.index name))
  in
  assert_registers
    {|
       _T = typing.TypeVar('_T')
       class C:
         pass
     |}
    "test.C"
    (Some ["object", []]);
  assert_registers
    {|
       _T = typing.TypeVar("_T")
       class List(typing.Generic[_T]):
         pass
       class C(List[_T]):
         pass
     |}
    "test.C"
    (Some ["typing.Generic", [Type.variable "test._T"]; "test.List", [Type.variable "test._T"]]);
  assert_registers
    {|
       _T = typing.TypeVar("_T")
       class Iterable(typing.Generic[_T]):
         pass
       class List(Iterable[_T], typing.Generic[_T]):
         pass
     |}
    "test.List"
    (Some ["test.Iterable", [Type.variable "test._T"]; "typing.Generic", [Type.variable "test._T"]]);
  assert_registers
    {|
      _T1 = typing.TypeVar('_T1')
      _T2 = typing.TypeVar('_T2')
      class Dict(typing.Generic[_T1, _T2]):
        pass
      class Foo(Dict[_T1, _T2]): pass
    |}
    "test.Foo"
    (Some
       [
         "typing.Generic", [Type.variable "test._T1"; Type.variable "test._T2"];
         "test.Dict", [Type.variable "test._T1"; Type.variable "test._T2"];
       ]);
  assert_registers
    {|
      _T1 = typing.TypeVar('_T1')
      _T2 = typing.TypeVar('_T2')
      class Dict(typing.Generic[_T1, _T2]):
        pass
      class Foo(Dict[_T1, _T1]): pass
    |}
    "test.Foo"
    (Some
       [
         "typing.Generic", [Type.variable "test._T1"];
         "test.Dict", [Type.variable "test._T1"; Type.variable "test._T1"];
       ]);
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
        ~in_memory:false
        sources
        ~context
    in
    let configuration = ScratchProject.configuration_of project in
    let read_only =
      ScratchProject.errors_environment project
      |> ErrorsEnvironment.Testing.ReadOnly.class_hierarchy_environment
    in
    let execute_action = function
      | `Edges (class_name, dependency, expectation) ->
          let printer v =
            let show_target_readable { ClassHierarchy.Target.target; parameters } =
              Format.asprintf
                "%s[%a]"
                (IndexTracker.annotation target)
                (Type.pp_parameters ~pp_type:Type.pp)
                parameters
            in
            v >>| List.to_string ~f:show_target_readable |> Option.value ~default:"none"
          in
          let expectation =
            expectation
            >>| List.map ~f:(fun name ->
                    { ClassHierarchy.Target.target = IndexTracker.index name; parameters = [] })
          in
          ClassHierarchyEnvironment.ReadOnly.get_edges
            read_only
            ~dependency
            (IndexTracker.index class_name)
          |> assert_equal ~printer expectation
    in
    List.iter middle_actions ~f:execute_action;
    if Option.is_some original_source then
      ScratchProject.delete_file project ~relative:"test.py";
    Option.iter new_source ~f:(ScratchProject.add_file project ~relative:"test.py");
    let update_result =
      let { Configuration.Analysis.local_root; _ } = configuration in
      List.map ["test.py", ()] ~f:(fun (relative, _) ->
          Test.relative_artifact_path ~root:local_root ~relative)
      |> ScratchProject.update_environment project
      |> ErrorsEnvironment.Testing.UpdateResult.class_hierarchy_environment
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
      (ClassHierarchyEnvironment.UpdateResult.locally_triggered_dependencies update_result);
    post_actions >>| List.iter ~f:execute_action |> Option.value ~default:()
  in
  let dependency =
    SharedMemoryKeys.DependencyKey.Registry.register (TypeCheckDefine (Reference.create "dep"))
  in
  assert_updates
    ~original_source:{|
      class C:
        pass
    |}
    ~new_source:{|
      class C:
        pass
    |}
    ~middle_actions:[`Edges ("test.C", dependency, Some ["object"])]
    ~expected_triggers:[]
    ~post_actions:[`Edges ("test.C", dependency, Some ["object"])]
    ();
  assert_updates
    ~original_source:{|
      class C:
        pass
    |}
    ~new_source:{|
    |}
    ~middle_actions:[`Edges ("test.C", dependency, Some ["object"])]
    ~expected_triggers:[dependency]
    ~post_actions:[`Edges ("test.C", dependency, None)]
    ();

  (* Class definition changes trigger *)
  assert_updates
    ~original_source:{|
      class C:
        pass
    |}
    ~new_source:{|
      class D:
       pass
      class C(D):
       pass
    |}
    ~middle_actions:[`Edges ("test.C", dependency, Some ["object"])]
    ~expected_triggers:[dependency]
    ~post_actions:[`Edges ("test.C", dependency, Some ["test.D"])]
    ();

  (* Class attributes do not trigger *)
  assert_updates
    ~original_source:{|
      class C:
        pass
    |}
    ~new_source:{|
      class C:
       x: int = 9
    |}
    ~middle_actions:[`Edges ("test.C", dependency, Some ["object"])]
    ~expected_triggers:[]
    ~post_actions:[`Edges ("test.C", dependency, Some ["object"])]
    ();

  (* Alias changes trigger *)
  assert_updates
    ~original_source:
      {|
      class First:
       pass
      class Second:
       pass
      Alias = First
      class C(Alias):
        pass
    |}
    ~new_source:
      {|
      class First:
       pass
      class Second:
       pass
      Alias = Second
      class C(Alias):
       pass
    |}
    ~middle_actions:[`Edges ("test.C", dependency, Some ["test.First"])]
    ~expected_triggers:[dependency]
    ~post_actions:[`Edges ("test.C", dependency, Some ["test.Second"])]
    ();

  (* Addition should trigger previous failed reads *)
  assert_updates
    ~original_source:{|
    |}
    ~new_source:{|
      class C:
       pass
    |}
    ~middle_actions:[`Edges ("test.C", dependency, None)]
    ~expected_triggers:[dependency]
    ~post_actions:[`Edges ("test.C", dependency, Some ["object"])]
    ();
  ()


let () =
  "environment"
  >::: [
         "simple_registration" >:: test_simple_registration;
         "inferred_bases" >:: test_inferred_generic_base;
         "updates" >:: test_updates;
       ]
  |> Test.run
