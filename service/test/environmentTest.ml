(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis
open Test
open OUnit2

let empty_environment () =
  AstEnvironment.ReadOnly.create ()
  |> UnannotatedGlobalEnvironment.create
  |> UnannotatedGlobalEnvironment.read_only
  |> AliasEnvironment.create
  |> AliasEnvironment.read_only
  |> ClassHierarchyEnvironment.create
  |> ClassHierarchyEnvironment.read_only
  |> Environment.shared_memory_handler


let test_populate context =
  let configuration, sources, ast_environment, ast_environment_update_result =
    let project =
      ScratchProject.setup
        ~context
        [
          ( "a.py",
            {|
            class D: pass
            class C(D): pass
            T = typing.TypeVar("T")
            def foo(x: T) -> T: pass
            def bar(): pass
          |}
          );
        ]
    in
    let ast_environment, ast_environment_update_result = ScratchProject.parse_sources project in
    let sources =
      let ast_environment = Analysis.AstEnvironment.read_only ast_environment in
      AstEnvironment.UpdateResult.reparsed ast_environment_update_result
      |> List.filter_map ~f:(AstEnvironment.ReadOnly.get_source ast_environment)
    in
    ( ScratchProject.configuration_of project,
      sources,
      AstEnvironment.read_only ast_environment,
      ast_environment_update_result )
  in
  let qualifiers =
    List.map sources ~f:(fun { Ast.Source.source_path = { SourcePath.qualifier; _ }; _ } ->
        qualifier)
  in
  let class_hierarchy_environment, update_result =
    update_environments
      ~scheduler:(Scheduler.mock ())
      ~configuration
      ~ast_environment_update_result
      ~ast_environment
      ~qualifiers:(Reference.Set.of_list qualifiers)
      ()
  in
  let environment =
    Environment.shared_memory_handler
      (ClassHierarchyEnvironment.read_only class_hierarchy_environment)
  in
  let qualifiers =
    List.map sources ~f:(fun { Ast.Source.source_path = { Ast.SourcePath.qualifier; _ }; _ } ->
        qualifier)
  in
  Service.Environment.populate
    ~configuration
    ~scheduler:(Scheduler.mock ())
    ~update_result
    environment
    qualifiers;
  let global_resolution = Analysis.Environment.resolution environment () in
  assert_equal
    (GlobalResolution.undecorated_signature global_resolution (Reference.create "a.foo"))
    (Some
       {
         Type.Callable.annotation = Type.variable "a.T";
         parameters =
           Type.Callable.Defined
             [
               Type.Callable.Parameter.Named
                 { name = "$parameter$x"; annotation = Type.variable "a.T"; default = false };
             ];
         define_location = None;
       });
  let assert_successors name expected_successors =
    let metadata = Analysis.GlobalResolution.class_metadata global_resolution (Primitive name) in
    let { ClassMetadataEnvironment.successors; _ } = Option.value_exn metadata in
    assert_equal ~printer:(String.concat ~sep:", ") expected_successors successors
  in
  assert_successors "a.C" ["a.D"; "object"];

  (* Ensure that the memory doesn't get clobbered on a re-write. *)
  let scheduler = Test.mock_scheduler () in
  let _ =
    let _, update_result =
      Test.update_environments
        ~ast_environment_update_result:(AstEnvironment.UpdateResult.create_for_testing ())
        ~ast_environment
        ~configuration
        ~qualifiers:(Reference.Set.singleton (Reference.create "a"))
        ()
    in
    Service.Environment.populate
      environment
      ~configuration
      ~scheduler
      ~update_result
      [Reference.create "a"]
  in
  assert_successors "a.C" ["a.D"; "object"]


let test_purge context =
  let _, ast_environment, environment =
    ScratchProject.setup
      ~context
      ["x.py", {|
            class D: pass
            class C(D): pass
          |}]
    |> ScratchProject.build_environment
  in
  let global_resolution = Analysis.Environment.resolution environment () in
  assert_is_some (GlobalResolution.class_metadata global_resolution (Primitive "x.D"));
  let _, update_result =
    update_environments
      ~ast_environment:(AstEnvironment.read_only ast_environment)
      ~scheduler:(mock_scheduler ())
      ~configuration:(Configuration.Analysis.create ())
      ~ast_environment_update_result:(AstEnvironment.UpdateResult.create_for_testing ())
      ~qualifiers:(Reference.Set.singleton (Reference.create "x"))
      ()
  in
  Environment.purge environment [Reference.create "x"] ~update_result;
  assert_is_none (GlobalResolution.class_metadata global_resolution (Primitive "x.D"))


let () = "environment" >::: ["populate" >:: test_populate; "purge" >:: test_purge] |> Test.run
