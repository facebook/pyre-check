(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis
open Test
open OUnit2

let assert_keys ~add ~get keys expected =
  let qualifier = Reference.create "dummy" in
  let handler = Service.Environment.shared_handler in
  let (module DependencyHandler) = Environment.dependency_handler handler in
  DependencyHandler.clear_keys_batch [qualifier];
  List.iter keys ~f:(add ~qualifier);
  assert_equal expected (get ~qualifier)


let test_dependency_keys _ =
  let assert_dependencies references ~expected =
    let references = List.map references ~f:Reference.create in
    let expected = List.map expected ~f:Reference.create in
    let handler = Service.Environment.shared_handler in
    let (module DependencyHandler) = Environment.dependency_handler handler in
    assert_keys
      ~add:DependencyHandler.add_dependent_key
      ~get:DependencyHandler.get_dependent_keys
      references
      expected
  in
  assert_dependencies ["a.b.c"; "d"; "e"] ~expected:["e"; "d"; "a.b.c"];
  assert_dependencies ["a.b.c"] ~expected:["a.b.c"]


let test_alias_keys _ =
  let assert_aliases keys ~expected =
    let handler = Service.Environment.shared_handler in
    let (module DependencyHandler) = Environment.dependency_handler handler in
    assert_keys
      ~add:DependencyHandler.add_alias_key
      ~get:DependencyHandler.get_alias_keys
      keys
      expected
  in
  assert_aliases ["first_key"] ~expected:["first_key"];
  assert_aliases ["first_key"; "second_key"] ~expected:["second_key"; "first_key"]


let test_class_keys _ =
  let assert_classes keys ~expected =
    let handler = Service.Environment.shared_handler in
    let (module DependencyHandler) = Environment.dependency_handler handler in
    assert_keys
      ~add:DependencyHandler.add_class_key
      ~get:DependencyHandler.get_class_keys
      keys
      expected
  in
  assert_classes ["C"] ~expected:["C"];
  assert_classes ["C"; "D"] ~expected:["D"; "C"]


let test_function_keys _ =
  let assert_function_keys keys ~expected =
    let keys = List.map keys ~f:Reference.create in
    let expected = List.map expected ~f:Reference.create in
    let handler = Service.Environment.shared_handler in
    let (module DependencyHandler) = Environment.dependency_handler handler in
    assert_keys
      ~add:DependencyHandler.add_function_key
      ~get:DependencyHandler.get_function_keys
      keys
      expected
  in
  assert_function_keys ["f1"] ~expected:["f1"];
  assert_function_keys ["f"; "g"; "class_method"] ~expected:["class_method"; "g"; "f"]


let test_global_keys _ =
  let assert_global_keys keys ~expected =
    let keys = List.map keys ~f:Reference.create in
    let expected = List.map expected ~f:Reference.create in
    let handler = Service.Environment.shared_handler in
    let (module DependencyHandler) = Environment.dependency_handler handler in
    assert_keys
      ~add:DependencyHandler.add_global_key
      ~get:DependencyHandler.get_global_keys
      keys
      expected
  in
  assert_global_keys ["a"] ~expected:["a"];
  assert_global_keys ["a"; "b"; "f"] ~expected:["f"; "b"; "a"]


let test_normalize_dependencies _ =
  let qualifier = Reference.create "dummy" in
  let handler = Service.Environment.shared_handler in
  let (module DependencyHandler) = Environment.dependency_handler handler in
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


let test_normalize _ =
  let handler = Service.Environment.shared_handler in
  let order = Analysis.Environment.class_hierarchy handler in
  let (module TypeOrderHandler) = order in
  ClassHierarchy.insert order "int";
  ClassHierarchy.insert order "str";
  let indices =
    let index_of annotation =
      TypeOrderHandler.find_unsafe (TypeOrderHandler.indices ()) annotation
    in
    [index_of "int"; index_of "str"] |> List.sort ~compare:Int.compare
  in
  Analysis.Environment.normalize_shared_memory [];
  assert_equal (TypeOrderHandler.keys ()) indices


let test_populate context =
  let configuration =
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
    let _ = ScratchProject.parse_sources project in
    ScratchProject.configuration_of project
  in
  let environment = Analysis.Environment.shared_memory_handler () in
  Service.Environment.populate_shared_memory ~configuration ~scheduler:(Scheduler.mock ()) [!&"a"];
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
  let handler = Service.Environment.shared_handler in
  Service.Environment.populate
    handler
    ~configuration
    ~scheduler:(Scheduler.mock ())
    [Option.value_exn (Ast.SharedMemory.Sources.get (Reference.create "a"))];
  assert_successors "a.C" ["a.D"; "object"]


let test_purge context =
  let configuration =
    let project =
      ScratchProject.setup
        ~context
        ["x.py", {|
            class D: pass
            class C(D): pass
          |}]
    in
    let _ = ScratchProject.parse_sources project in
    ScratchProject.configuration_of project
  in
  Service.Environment.populate_shared_memory ~configuration ~scheduler:(Scheduler.mock ()) [!&"x"];
  let handler = Service.Environment.shared_handler in
  let global_resolution = Analysis.Environment.resolution handler () in
  assert_is_some (GlobalResolution.class_metadata global_resolution (Primitive "x.D"));
  Environment.purge handler [Reference.create "x"];
  assert_is_none (GlobalResolution.class_metadata global_resolution (Primitive "x.D"))


let () =
  "environment"
  >::: [ "alias_keys" >:: test_alias_keys;
         "class_keys" >:: test_class_keys;
         "dependent_keys" >:: test_dependency_keys;
         "function_keys" >:: test_function_keys;
         "global_keys" >:: test_global_keys;
         "normalize_dependencies" >:: test_normalize_dependencies;
         "normalize" >:: test_normalize;
         "populate" >:: test_populate;
         "purge" >:: test_purge ]
  |> Test.run
