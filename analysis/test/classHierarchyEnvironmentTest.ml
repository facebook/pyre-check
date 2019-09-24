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
    in
    let read_only = ClassHierarchyEnvironment.read_only class_hierarchy_environment in
    let expected =
      expected
      >>| List.map ~f:(fun name ->
              { ClassHierarchy.Target.target = IndexTracker.index name; parameters = Concrete [] })
    in
    let printer v =
      let show_target_readable { ClassHierarchy.Target.target; parameters } =
        Printf.sprintf
          "%s[%s]"
          (IndexTracker.annotation target)
          (Type.OrderedTypes.show parameters)
      in
      v >>| List.to_string ~f:show_target_readable |> Option.value ~default:"none"
    in
    assert_equal
      ~printer
      expected
      (ClassHierarchyEnvironment.ReadOnly.get_edges read_only (IndexTracker.index name))
  in
  assert_registers {|
    class C:
      pass
  |} "test.C" (Some ["object"]);
  assert_registers
    {|
    class D:
     pass
    class C(D):
      pass
  |}
    "test.C"
    (Some ["test.D"]);
  ()


let test_inferred_generic_base context =
  let assert_registers source name expected =
    let project = ScratchProject.setup ["test.py", source] ~context ~incremental_style:Shallow in
    let ast_environment, ast_environment_update_result = ScratchProject.parse_sources project in
    let sources =
      let ast_environment = Analysis.AstEnvironment.read_only ast_environment in
      AstEnvironment.UpdateResult.reparsed ast_environment_update_result
      |> List.filter_map ~f:(AstEnvironment.ReadOnly.get_source ast_environment)
    in
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
    let qualifiers =
      List.map sources ~f:(fun { Ast.Source.source_path = { SourcePath.qualifier; _ }; _ } ->
          qualifier)
    in
    let configuration = ScratchProject.configuration_of project in
    let _ =
      Test.update_environments
        ~ast_environment:(AstEnvironment.read_only ast_environment)
        ~configuration
        ~ast_environment_update_result
        ~qualifiers:(Reference.Set.of_list qualifiers)
        ()
    in
    let read_only = ClassHierarchyEnvironment.read_only class_hierarchy_environment in
    let expected =
      expected
      >>| List.map ~f:(fun (name, concretes) ->
              {
                ClassHierarchy.Target.target = IndexTracker.index name;
                parameters = Concrete concretes;
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
        sources
        ~context
    in
    let ast_environment, ast_environment_update_result = ScratchProject.parse_sources project in
    let configuration = ScratchProject.configuration_of project in
    let unannotated_global_environment =
      let ast_environment = AstEnvironment.read_only ast_environment in
      UnannotatedGlobalEnvironment.create ast_environment
    in
    let alias_environment =
      AliasEnvironment.create
        (UnannotatedGlobalEnvironment.read_only unannotated_global_environment)
    in
    let class_hierarchy_environment =
      ClassHierarchyEnvironment.create (AliasEnvironment.read_only alias_environment)
    in
    let update ~ast_environment_update_result () =
      let scheduler = Test.mock_scheduler () in
      UnannotatedGlobalEnvironment.update
        unannotated_global_environment
        ~scheduler
        ~configuration
        ~ast_environment_update_result
        (Reference.Set.singleton (Reference.create "test"))
      |> AliasEnvironment.update alias_environment ~scheduler ~configuration
      |> ClassHierarchyEnvironment.update class_hierarchy_environment ~scheduler ~configuration
    in
    let _ = update ~ast_environment_update_result () in
    let read_only = ClassHierarchyEnvironment.read_only class_hierarchy_environment in
    let execute_action = function
      | `Edges (class_name, dependency, expectation) ->
          let printer v =
            let show_target_readable { ClassHierarchy.Target.target; parameters } =
              Printf.sprintf
                "%s[%s]"
                (IndexTracker.annotation target)
                (Type.OrderedTypes.show parameters)
            in
            v >>| List.to_string ~f:show_target_readable |> Option.value ~default:"none"
          in
          let expectation =
            expectation
            >>| List.map ~f:(fun name ->
                    {
                      ClassHierarchy.Target.target = IndexTracker.index name;
                      parameters = Concrete [];
                    })
          in
          ClassHierarchyEnvironment.ReadOnly.get_edges
            read_only
            ~dependency
            (IndexTracker.index class_name)
          |> assert_equal ~printer expectation
      | `Undecorated (function_name, dependency, expectation) ->
          let parse expectation =
            match Type.create (parse_single_expression expectation) ~aliases:(fun _ -> None) with
            | Type.Callable { implementation; _ } -> implementation
            | _ -> failwith (expectation ^ "not a callable")
          in
          let printer implementation =
            implementation >>| Type.Callable.show_overload Type.pp |> Option.value ~default:"none"
          in
          let remove_define_location overload =
            { overload with Type.Record.Callable.define_location = None }
          in
          ClassHierarchyEnvironment.ReadOnly.get_undecorated_function
            read_only
            ~dependency
            (Reference.create function_name)
          >>| remove_define_location
          |> assert_equal ~printer (expectation >>| parse >>| remove_define_location)
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
        relative
        content
      =
      let content = trim_extra_indentation content in
      let file = File.create ~content (Path.create_relative ~root:local_root ~relative) in
      File.write file
    in
    if Option.is_some original_source then
      delete_file project "test.py";
    Option.iter new_source ~f:(add_file project "test.py");
    let ast_environment_update_result =
      let { ScratchProject.module_tracker; _ } = project in
      let { Configuration.Analysis.local_root; _ } = configuration in
      let paths =
        List.map ["test.py", ()] ~f:(fun (relative, _) ->
            Path.create_relative ~root:local_root ~relative)
      in
      ModuleTracker.update ~configuration ~paths module_tracker
      |> (fun updates -> AstEnvironment.Update updates)
      |> AstEnvironment.update ~configuration ~scheduler:(mock_scheduler ()) ast_environment
    in
    let update_result = update ~ast_environment_update_result () in
    let printer set =
      ClassHierarchyEnvironment.DependencyKey.KeySet.elements set
      |> List.to_string ~f:ClassHierarchyEnvironment.show_dependency
    in
    let expected_triggers =
      ClassHierarchyEnvironment.DependencyKey.KeySet.of_list expected_triggers
    in
    assert_equal
      ~printer
      expected_triggers
      (ClassHierarchyEnvironment.UpdateResult.triggered_dependencies update_result);
    post_actions >>| List.iter ~f:execute_action |> Option.value ~default:()
  in
  let dependency = ClassHierarchyEnvironment.TypeCheckSource (Reference.create "dep") in
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

  (* Undecorated functions *)
  assert_updates
    ~original_source:
      {|
      class First:
        pass
      class Second:
        pass
      Alias = First
      def decorator(__x: Alias) -> Second:
       pass
    |}
    ~new_source:
      {|
      class First:
        pass
      class Second:
        pass
      Alias = Second
      def decorator(__x: Alias) -> Second:
       pass
    |}
    ~middle_actions:
      [
        `Undecorated
          ("test.decorator", dependency, Some "typing.Callable[[test.First], test.Second]");
      ]
    ~expected_triggers:[dependency]
    ~post_actions:
      [
        `Undecorated
          ("test.decorator", dependency, Some "typing.Callable[[test.Second], test.Second]");
      ]
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
