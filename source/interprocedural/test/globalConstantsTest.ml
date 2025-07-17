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


let test_from_qualifiers context =
  let from_sources sources =
    let project = Test.ScratchProject.setup ~context sources in
    let { ScratchProject.BuiltTypeEnvironment.sources; _ } =
      ScratchProject.build_type_environment project
    in
    let pyre_api =
      project |> ScratchProject.pyre_pysa_read_only_api |> PyrePysaApi.ReadOnly.from_pyre1_api
    in
    let qualifiers =
      List.map sources ~f:(fun { Source.module_path = { ModulePath.qualifier; _ }; _ } -> qualifier)
    in
    GlobalConstants.Heap.from_qualifiers ~pyre_api ~qualifiers
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
