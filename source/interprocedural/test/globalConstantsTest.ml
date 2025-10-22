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

let test_from_source context =
  let assert_global_constants ~all_sources ~test_source_qualifier ~expected =
    let project =
      Test.ScratchPyrePysaProject.setup ~context ~requires_type_of_expressions:false all_sources
    in
    let pyre_api = Test.ScratchPyrePysaProject.read_only_api project in
    let configuration = Test.ScratchPyrePysaProject.configuration_of project in
    let scheduler = Test.mock_scheduler () in
    let scheduler_policy = Scheduler.Policy.legacy_fixed_chunk_count () in
    let qualifiers = PyrePysaApi.ReadOnly.explicit_qualifiers pyre_api in
    let all_initial_callables =
      Interprocedural.FetchCallables.from_qualifiers
        ~scheduler
        ~scheduler_policy
        ~configuration
        ~pyre_api
        ~qualifiers
    in
    let definitions_and_stubs =
      Interprocedural.FetchCallables.get all_initial_callables ~definitions:true ~stubs:true
    in
    let callables_to_definitions_map =
      CallablesSharedMemory.ReadWrite.from_callables
        ~scheduler
        ~scheduler_policy
        ~pyre_api
        definitions_and_stubs
    in
    let global_constants =
      GlobalConstants.Heap.from_qualifier
        ~pyre_api
        ~callables_to_definitions_map:
          (CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
        (Reference.create test_source_qualifier)
    in
    let expected =
      expected
      |> List.map ~f:(fun (key, value) -> Reference.create key, value)
      |> GlobalConstants.Heap.of_alist_exn
    in
    assert_equal
      expected
      global_constants
      ~printer:GlobalConstants.Heap.show
      ~cmp:GlobalConstants.Heap.equal
  in
  assert_global_constants
    ~all_sources:["a.py", {|
      x = "123"
      y = "456"
    |}]
    ~test_source_qualifier:"a"
    ~expected:["a.x", "123"; "a.y", "456"];
  assert_global_constants
    ~all_sources:["a.py", {|
      x = "123"
      x = "456"
    |}]
    ~test_source_qualifier:"a"
    ~expected:["a.x", "123"];
  assert_global_constants
    ~all_sources:["a.py", {|
      class Foo:
        x = "123"
    |}]
    ~test_source_qualifier:"a"
    ~expected:[];
  assert_global_constants
    ~all_sources:["a/__init__.py", {|
      x = "123"
    |}]
    ~test_source_qualifier:"a"
    ~expected:["a.x", "123"];
  assert_global_constants
    ~all_sources:
      ["a.py", {|
      import b
      b.y = ""
    |}; "b.py", {|
        y = "456"
    |}]
    ~test_source_qualifier:"a"
    ~expected:[]
  (* Modify another module's global variable. We don't support collecting constants in this case. *);
  assert_global_constants
    ~all_sources:
      ["a.py", {|
      from b import y
      y = "123"
    |}; "b.py", {|
        y = "456"
    |}]
    ~test_source_qualifier:"a"
    ~expected:["a.y", "123"]


let test_from_qualifiers context =
  let from_sources sources =
    let project =
      Test.ScratchPyrePysaProject.setup ~context ~requires_type_of_expressions:false sources
    in
    let pyre_api = Test.ScratchPyrePysaProject.read_only_api project in
    let configuration = Test.ScratchPyrePysaProject.configuration_of project in
    let scheduler = Test.mock_scheduler () in
    let scheduler_policy = Scheduler.Policy.legacy_fixed_chunk_count () in
    let qualifiers = PyrePysaApi.ReadOnly.explicit_qualifiers pyre_api in
    let all_initial_callables =
      Interprocedural.FetchCallables.from_qualifiers
        ~scheduler
        ~scheduler_policy
        ~configuration
        ~pyre_api
        ~qualifiers
    in
    let definitions_and_stubs =
      Interprocedural.FetchCallables.get all_initial_callables ~definitions:true ~stubs:true
    in
    let callables_to_definitions_map =
      CallablesSharedMemory.ReadWrite.from_callables
        ~scheduler
        ~scheduler_policy
        ~pyre_api
        definitions_and_stubs
    in
    let qualifiers =
      List.map sources ~f:(fun (qualifier, _) -> ModulePath.qualifier_from_relative_path qualifier)
    in
    GlobalConstants.Heap.from_qualifiers
      ~pyre_api
      ~callables_to_definitions_map:
        (CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
      ~qualifiers
  in
  let assert_global_constants ~sources ~expected =
    let global_constants = from_sources sources in
    let expected =
      expected
      |> List.map ~f:(fun (key, value) -> Reference.create key, value)
      |> GlobalConstants.Heap.of_alist_exn
    in
    assert_equal
      expected
      global_constants
      ~printer:GlobalConstants.Heap.show
      ~cmp:GlobalConstants.Heap.equal
  in
  let assert_raises_exception ~sources =
    try
      let _ = from_sources sources in
      assert_failure "Expected an exception to be raised"
    with
    | _ -> ()
  in
  assert_global_constants
    ~sources:
      ["a/__init__.py", {|
      value = '123'
    |}; "a/b.py", {|
      value = '456'
    |}]
    ~expected:["a.value", "123"; "a.b.value", "456"];
  (* The same qualified global variable name can refer to different variables. *)
  assert_raises_exception
    ~sources:
      [
        ( "a/__init__.py",
          {|
      class Test(object):
        pass
      b = Test()
      b.value = '123'
    |} );
        "a/b.py", {|
      value = '456'
    |};
      ]


let () =
  "global_constants"
  >::: ["from_source" >:: test_from_source; "from_qualifiers" >:: test_from_qualifiers]
  |> Test.run
