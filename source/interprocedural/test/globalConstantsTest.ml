(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Test
open Ast
open Interprocedural

let test_from_source context =
  let assert_global_constants ~all_sources ~test_source_qualifier ~expected =
    let test_source =
      let project = Test.ScratchProject.setup ~context all_sources in
      let { ScratchProject.BuiltTypeEnvironment.sources; _ } =
        ScratchProject.build_type_environment project
      in
      List.find_map_exn
        sources
        ~f:(fun ({ Source.module_path = { ModulePath.qualifier; _ }; _ } as source) ->
          Option.some_if (String.equal (Reference.show qualifier) test_source_qualifier) source)
    in
    let global_constants =
      GlobalConstants.Heap.from_source
        ~qualifier:(Reference.create test_source_qualifier)
        test_source
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


let () = "global_constants" >::: ["from_source" >:: test_from_source] |> Test.run
