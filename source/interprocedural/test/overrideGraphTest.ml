(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Interprocedural
open Test

let setup ?(update_environment_with = []) ~context ~handle source =
  let project =
    let external_sources =
      List.map update_environment_with ~f:(fun { handle; source } -> handle, source)
    in
    ScratchProject.setup ~context ~external_sources [handle, source]
  in
  let { ScratchProject.BuiltTypeEnvironment.sources; type_environment; _ } =
    ScratchProject.build_type_environment project
  in
  let source =
    List.find_exn sources ~f:(fun { Source.module_path = { ModulePath.relative; _ }; _ } ->
        String.equal relative handle)
  in
  source, type_environment, ScratchProject.configuration_of project


let test_method_overrides context =
  let assert_method_overrides ?(update_environment_with = []) ?(handle = "test.py") source ~expected
    =
    let expected =
      let create_callables (member, overriding_types) =
        Target.create_method !&member, List.map overriding_types ~f:Reference.create
      in
      List.map expected ~f:create_callables
    in
    let source, environment, _ = setup ~update_environment_with ~context ~handle source in
    let overrides_map =
      OverrideGraph.Heap.from_source ~environment ~include_unit_tests:false ~source
    in
    let expected_overrides = OverrideGraph.Heap.of_alist_exn expected in
    assert_equal
      ~cmp:OverrideGraph.Heap.equal
      ~printer:OverrideGraph.Heap.show
      expected_overrides
      overrides_map
  in
  assert_method_overrides
    {|
      class Foo:
        def foo(): pass
      class Bar(Foo):
        def foo(): pass
      class Baz(Bar):
        def foo(): pass
        def baz(): pass
      class Qux(Foo):
        def foo(): pass
    |}
    ~expected:["test.Bar.foo", ["test.Baz"]; "test.Foo.foo", ["test.Bar"; "test.Qux"]];

  (* We don't register any overrides at all for classes in test files. *)
  assert_method_overrides
    {|
      class Foo:
        def foo(): pass
      class Bar(Foo):
        def foo(): pass
      class Test(unittest.case.TestCase):
        class Baz(Foo):
          def foo(): pass
    |}
    ~expected:[];
  assert_method_overrides
    ~update_environment_with:
      [
        {
          handle = "module.py";
          source =
            {|
        import module
        class Baz(module.Foo):
          def foo(): pass
      |};
        };
      ]
    ~handle:"test_module.py"
    {|
      import module
      class Test(unittest.case.TestCase):
        class Bar(module.Foo):
          def foo(): pass
    |}
    ~expected:[]


let () =
  Scheduler.Daemon.check_entry_point ();
  "overrideGraph" >::: ["overrides" >:: test_method_overrides] |> Test.run
