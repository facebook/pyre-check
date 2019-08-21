(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis
open Test
open OUnit2

let empty_environment () = AstEnvironment.ReadOnly.create () |> Environment.shared_memory_handler

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
  DependencyHandler.add_class_key ~qualifier "T1";
  DependencyHandler.add_class_key ~qualifier "T3";
  DependencyHandler.add_class_key ~qualifier "T2";
  DependencyHandler.normalize [qualifier];
  assert_equal
    ~printer:(String.concat ~sep:", ")
    (DependencyHandler.get_class_keys ~qualifier)
    ["T1"; "T2"; "T3"];
  DependencyHandler.add_alias_key ~qualifier "C_Alias";
  DependencyHandler.add_alias_key ~qualifier "A_Alias";
  DependencyHandler.add_alias_key ~qualifier "B_Alias";
  DependencyHandler.normalize [qualifier];
  assert_equal
    ~printer:(List.to_string ~f:ident)
    (DependencyHandler.get_alias_keys ~qualifier)
    ["A_Alias"; "B_Alias"; "C_Alias"]


let test_populate context =
  let configuration, sources, ast_environment =
    let project =
      ScratchProject.setup
        ~context
        [ ( "a.py",
            {|
            class D: pass
            class C(D): pass
            T = typing.TypeVar("T")
            def foo(x: T) -> T: pass
            def bar(): pass
          |}
          ) ]
    in
    let sources, ast_environment = ScratchProject.parse_sources project in
    ScratchProject.configuration_of project, sources, AstEnvironment.read_only ast_environment
  in
  let environment =
    Service.Environment.populate_shared_memory
      ~configuration
      ~scheduler:(Scheduler.mock ())
      ~ast_environment
      sources
  in
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
             [ Type.Callable.Parameter.Named
                 { name = "$parameter$x"; annotation = Type.variable "a.T"; default = false } ];
         define_location = None;
       });
  let assert_successors name expected_successors =
    let metadata = Analysis.GlobalResolution.class_metadata global_resolution (Primitive name) in
    let { GlobalResolution.successors; _ } = Option.value_exn metadata in
    assert_equal ~printer:(String.concat ~sep:", ") expected_successors successors
  in
  assert_successors "a.C" ["a.D"; "object"];

  (* Ensure that the memory doesn't get clobbered on a re-write. *)
  let _ =
    Service.Environment.populate
      environment
      ~configuration
      ~scheduler:(Scheduler.mock ())
      [Option.value_exn (AstEnvironment.ReadOnly.get_source ast_environment (Reference.create "a"))]
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
  Environment.purge environment [Reference.create "x"];
  assert_is_none (GlobalResolution.class_metadata global_resolution (Primitive "x.D"))


let () =
  "environment"
  >::: [ "normalize_dependencies" >:: test_normalize_dependencies;
         "populate" >:: test_populate;
         "purge" >:: test_purge ]
  |> Test.run
