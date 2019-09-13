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
  |> Environment.shared_memory_handler


let test_normalize_dependencies _ =
  let qualifier = Reference.create "dummy" in
  let environment = empty_environment () in
  let (module DependencyHandler) = Environment.dependency_handler environment in
  DependencyHandler.clear_keys_batch [qualifier];
  DependencyHandler.add_function_key ~qualifier !&"f";

  (* Only keep one copy. *)
  DependencyHandler.add_function_key ~qualifier !&"f";
  DependencyHandler.add_function_key ~qualifier !&"h";
  DependencyHandler.add_function_key ~qualifier !&"g";
  DependencyHandler.normalize [qualifier];
  assert_equal
    ~printer:(List.to_string ~f:Reference.show)
    (DependencyHandler.get_function_keys ~qualifier)
    [!&"f"; !&"g"; !&"h"];
  DependencyHandler.add_global_key ~qualifier !&"b";
  DependencyHandler.add_global_key ~qualifier !&"c";
  DependencyHandler.add_global_key ~qualifier !&"a";
  DependencyHandler.normalize [qualifier];
  assert_equal
    ~printer:(List.to_string ~f:Reference.show)
    (DependencyHandler.get_global_keys ~qualifier)
    [!&"a"; !&"b"; !&"c"];
  DependencyHandler.add_dependent_key ~qualifier !&"first.module";
  DependencyHandler.add_dependent_key ~qualifier !&"second.module";
  DependencyHandler.add_dependent_key ~qualifier !&"aardvark";
  DependencyHandler.normalize [qualifier];
  assert_equal
    ~printer:(List.to_string ~f:Reference.show)
    (DependencyHandler.get_dependent_keys ~qualifier)
    [!&"aardvark"; !&"first.module"; !&"second.module"];
  DependencyHandler.normalize [qualifier];
  DependencyHandler.add_alias_key ~qualifier "C_Alias";
  DependencyHandler.add_alias_key ~qualifier "A_Alias";
  DependencyHandler.add_alias_key ~qualifier "B_Alias";
  DependencyHandler.normalize [qualifier];
  assert_equal
    ~printer:(List.to_string ~f:ident)
    (DependencyHandler.get_alias_keys ~qualifier)
    ["A_Alias"; "B_Alias"; "C_Alias"]


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
  let unannotated_global_environment = UnannotatedGlobalEnvironment.create ast_environment in
  let alias_environment =
    AliasEnvironment.create (UnannotatedGlobalEnvironment.read_only unannotated_global_environment)
  in
  let qualifiers =
    List.map sources ~f:(fun { Ast.Source.source_path = { SourcePath.qualifier; _ }; _ } ->
        qualifier)
  in
  let update_result =
    UnannotatedGlobalEnvironment.update
      unannotated_global_environment
      ~scheduler:(Scheduler.mock ())
      ~configuration
      ~ast_environment_update_result
      (Reference.Set.of_list qualifiers)
    |> AliasEnvironment.update alias_environment ~scheduler:(Scheduler.mock ()) ~configuration
  in
  let environment =
    Environment.shared_memory_handler (AliasEnvironment.read_only alias_environment)
  in
  Service.Environment.populate
    ~configuration
    ~scheduler:(Scheduler.mock ())
    ~update_result
    environment
    (UnannotatedGlobalEnvironment.read_only unannotated_global_environment)
    sources;
  let global_resolution = Analysis.Environment.resolution environment () in
  let (module DependenciesHandler) = Environment.dependency_handler environment in
  assert_equal
    ~printer:(List.to_string ~f:Reference.show)
    (DependenciesHandler.get_global_keys ~qualifier:(Reference.create "a"))
    (List.map ~f:Reference.create ["a.T"; "a.C"; "a.D"; "a.foo"; "a.bar"]);
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
    let { GlobalResolution.successors; _ } = Option.value_exn metadata in
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
      (UnannotatedGlobalEnvironment.read_only (UnannotatedGlobalEnvironment.create ast_environment))
      ~configuration
      ~scheduler
      ~update_result
      [
        Option.value_exn (AstEnvironment.ReadOnly.get_source ast_environment (Reference.create "a"));
      ]
  in
  assert_successors "a.C" ["a.D"; "object"]


let test_purge context =
  let _, _, environment =
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
  let unannotated_global_environment =
    UnannotatedGlobalEnvironment.create (Environment.ast_environment environment)
  in
  let alias_environment =
    AliasEnvironment.create (UnannotatedGlobalEnvironment.read_only unannotated_global_environment)
  in
  let update_result =
    UnannotatedGlobalEnvironment.update
      unannotated_global_environment
      ~scheduler:(mock_scheduler ())
      ~configuration:(Configuration.Analysis.create ())
      ~ast_environment_update_result:(AstEnvironment.UpdateResult.create_for_testing ())
      (Reference.Set.singleton (Reference.create "x"))
    |> AliasEnvironment.update
         alias_environment
         ~scheduler:(mock_scheduler ())
         ~configuration:(Configuration.Analysis.create ())
  in
  Environment.purge environment [Reference.create "x"] ~update_result;
  assert_is_none (GlobalResolution.class_metadata global_resolution (Primitive "x.D"))


let () =
  "environment"
  >::: [
         "normalize_dependencies" >:: test_normalize_dependencies;
         "populate" >:: test_populate;
         "purge" >:: test_purge;
       ]
  |> Test.run
