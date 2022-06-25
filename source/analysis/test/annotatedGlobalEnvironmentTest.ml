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
  let assert_registers source name ?original expected =
    let project = ScratchProject.setup ["test.py", source] ~context in
    let read_only =
      ScratchProject.errors_environment project
      |> ErrorsEnvironment.Testing.ReadOnly.attribute_resolution
    in
    let location_insensitive_compare left right =
      Option.compare Annotation.compare left right = 0
    in
    let printer global =
      global >>| Annotation.sexp_of_t >>| Sexp.to_string_hum |> Option.value ~default:"None"
    in
    let expectation = expected >>| Annotation.create_immutable ?original in
    AttributeResolution.ReadOnly.get_global read_only (Reference.create name)
    >>| (fun { annotation; _ } -> annotation)
    |> assert_equal ~cmp:location_insensitive_compare ~printer expectation
  in
  assert_registers "x = 1" "test.x" (Some Type.integer);
  assert_registers "x, y, z  = 'A', True, 1.8" "test.x" (Some Type.string);
  assert_registers "x, y, z  = 'A', True, 1.8" "test.z" (Some Type.float);

  (* Tuple assignment is all or nothing *)
  assert_registers "x, y  = 'A', True, 1.8" "test.x" (Some Type.Top);
  assert_registers
    {|
      class P: pass
    |}
    "test.P"
    (Some (Type.meta (Type.Primitive "test.P")));
  assert_registers
    {|
      class P: pass
      class R: pass
      def foo(x: P) -> R:
       ...
    |}
    "test.foo"
    (Some
       (Type.Callable.create
          ~name:(Reference.create "test.foo")
          ~parameters:
            (Defined
               [
                 Named
                   { annotation = Type.Primitive "test.P"; default = false; name = "$parameter$x" };
               ])
          ~annotation:(Type.Primitive "test.R")
          ()));
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
      |> ErrorsEnvironment.Testing.ReadOnly.attribute_resolution
    in
    let execute_action = function
      | global_name, dependency, expectation ->
          let location_insensitive_compare left right =
            Option.compare Annotation.compare left right = 0
          in
          let printer global =
            global >>| Annotation.sexp_of_t >>| Sexp.to_string_hum |> Option.value ~default:"None"
          in
          let expectation = expectation >>| Annotation.create_immutable in
          AttributeResolution.ReadOnly.get_global
            read_only
            (Reference.create global_name)
            ~dependency
          >>| (fun { annotation; _ } -> annotation)
          |> assert_equal ~cmp:location_insensitive_compare ~printer expectation
    in
    List.iter middle_actions ~f:execute_action;
    if Option.is_some original_source then
      ScratchProject.delete_file project ~relative:"test.py";
    new_source >>| ScratchProject.add_file project ~relative:"test.py" |> Option.value ~default:();
    let { Configuration.Analysis.local_root; _ } = configuration in
    let update_result =
      ScratchProject.update_environment
        project
        [Test.relative_artifact_path ~root:local_root ~relative:"test.py"]
    in
    let printer set =
      SharedMemoryKeys.DependencyKey.RegisteredSet.elements set
      |> List.map ~f:SharedMemoryKeys.DependencyKey.get_key
      |> List.to_string ~f:SharedMemoryKeys.show_dependency
    in
    let expected_triggers =
      SharedMemoryKeys.DependencyKey.RegisteredSet.of_list expected_triggers
    in
    let triggered_type_check_define_dependencies =
      ErrorsEnvironment.Testing.UpdateResult.annotated_global_environment update_result
      |> AnnotatedGlobalEnvironment.UpdateResult.all_triggered_dependencies
      |> List.fold
           ~f:SharedMemoryKeys.DependencyKey.RegisteredSet.union
           ~init:SharedMemoryKeys.DependencyKey.RegisteredSet.empty
      |> SharedMemoryKeys.DependencyKey.RegisteredSet.filter (function registered ->
             (match SharedMemoryKeys.DependencyKey.get_key registered with
             | SharedMemoryKeys.TypeCheckDefine _ -> true
             | _ -> false))
    in
    assert_equal ~printer expected_triggers triggered_type_check_define_dependencies;
    post_actions >>| List.iter ~f:execute_action |> Option.value ~default:()
  in
  let dependency =
    SharedMemoryKeys.DependencyKey.Registry.register (TypeCheckDefine (Reference.create "dep"))
  in
  assert_updates
    ~original_source:{|
      x = 7
    |}
    ~new_source:{|
      y = 9
    |}
    ~middle_actions:["test.x", dependency, Some Type.integer]
    ~expected_triggers:[dependency]
    ~post_actions:["test.x", dependency, None]
    ();
  assert_updates
    ~original_source:{|
      x = 7
    |}
    ~new_source:{|
      x = 9
    |}
    ~middle_actions:["test.x", dependency, Some Type.integer]
    ~expected_triggers:[]
    ~post_actions:["test.x", dependency, Some Type.integer]
    ();
  assert_updates
    ~original_source:{|
      x = 7
    |}
    ~new_source:{|
      x, y = 7, 8
    |}
    ~middle_actions:["test.x", dependency, Some Type.integer]
    ~expected_triggers:[]
    ~post_actions:["test.x", dependency, Some Type.integer]
    ();

  (* Addition should trigger previous failed reads *)
  assert_updates
    ~original_source:{|
    |}
    ~new_source:{|
     x = 7
    |}
    ~middle_actions:["test.x", dependency, None]
    ~expected_triggers:[dependency]
    ~post_actions:["test.x", dependency, Some Type.integer]
    ();
  assert_updates
    ~original_source:{|
      x = 7
    |}
    ~new_source:{|
      otra = "A"
      x = 9
    |}
    ~middle_actions:["test.x", dependency, Some Type.integer]
    ~expected_triggers:[]
    ~post_actions:["test.x", dependency, Some Type.integer]
    ();
  ()


let () =
  "environment"
  >::: ["simple_registration" >:: test_simple_registration; "updates" >:: test_updates]
  |> Test.run
