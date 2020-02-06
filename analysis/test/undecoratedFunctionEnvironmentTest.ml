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
    let update ~ast_environment_update_result () =
      let scheduler = Test.mock_scheduler () in
      let ast_environment = AstEnvironment.read_only ast_environment in
      UndecoratedFunctionEnvironment.update_this_and_all_preceding_environments
        ast_environment
        ~scheduler
        ~configuration
        ~ast_environment_update_result
        (Reference.Set.singleton (Reference.create "test"))
    in
    let update_result = update ~ast_environment_update_result () in
    let read_only = UndecoratedFunctionEnvironment.UpdateResult.read_only update_result in
    let execute_action = function
      | `Undecorated (function_name, dependency, expectation) ->
          let parse expectation =
            match Type.create (parse_single_expression expectation) ~aliases:(fun _ -> None) with
            | Type.Callable { implementation; _ } -> implementation
            | _ -> failwith (expectation ^ "not a callable")
          in
          let printer implementation =
            implementation >>| Type.Callable.show_overload Type.pp |> Option.value ~default:"none"
          in
          UndecoratedFunctionEnvironment.ReadOnly.get_undecorated_function
            read_only
            ~dependency
            (Reference.create function_name)
          |> assert_equal ~printer (expectation >>| parse)
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
      SharedMemoryKeys.DependencyKey.KeySet.elements set
      |> List.to_string ~f:SharedMemoryKeys.show_dependency
    in
    let expected_triggers = SharedMemoryKeys.DependencyKey.KeySet.of_list expected_triggers in
    assert_equal
      ~printer
      expected_triggers
      (UndecoratedFunctionEnvironment.UpdateResult.locally_triggered_dependencies update_result);
    post_actions >>| List.iter ~f:execute_action |> Option.value ~default:()
  in
  let dependency = SharedMemoryKeys.TypeCheckDefine (Reference.create "dep") in
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


let () = "environment" >::: ["updates" >:: test_updates] |> Test.run
