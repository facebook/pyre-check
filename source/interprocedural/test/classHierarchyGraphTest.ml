(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Test
open Analysis
open Ast
open Interprocedural
open ClassHierarchyGraph

let test_from_source context =
  let assert_class_hierarchy ~source ~expected =
    let test_source, environment =
      let project = Test.ScratchProject.setup ~context ["test.py", source] in
      let { ScratchProject.BuiltTypeEnvironment.type_environment; sources } =
        ScratchProject.build_type_environment project
      in
      let test_source =
        List.find_map_exn
          sources
          ~f:(fun ({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source) ->
            Option.some_if (String.equal (Reference.show qualifier) "test") source)
      in
      test_source, TypeEnvironment.read_only type_environment
    in
    let class_hierarchy = from_source ~environment ~source:test_source in
    assert_equal
      expected
      class_hierarchy
      ~printer:ClassHierarchyGraph.show
      ~cmp:ClassHierarchyGraph.equal
  in
  assert_class_hierarchy
    ~source:
      {|
      class A: pass
      class B(A): pass
      class C(A): pass
      class D(B, C): pass
    |}
    ~expected:
      (create
         ~roots:["object"]
         ~edges:
           [
             "object", ["test.A"];
             "test.A", ["test.B"; "test.C"];
             "test.B", ["test.D"];
             "test.C", ["test.D"];
             "test.D", [];
           ]);
  assert_class_hierarchy
    ~source:
      {|
      class Meta(type): pass
      class A(metaclass=Meta): pass
      class B(Meta, object): pass
    |}
    ~expected:
      (create
         ~roots:["object"; "type"]
         ~edges:
           [
             "object", ["test.A"];
             "type", ["test.Meta"];
             "test.Meta", ["test.B"];
             "test.A", [];
             "test.B", [];
           ]);
  assert_class_hierarchy
    ~source:
      {|
      class A: pass
      class B: pass
      class C(A): pass
      class D(A, B): pass
      class E(D): pass
      class F(A, D): pass
    |}
    ~expected:
      (create
         ~roots:["object"]
         ~edges:
           [
             "object", ["test.A"; "test.B"];
             "test.A", ["test.C"; "test.D"; "test.F"];
             "test.B", ["test.D"];
             "test.C", [];
             "test.D", ["test.E"; "test.F"];
             "test.E", [];
             "test.F", [];
           ])


let () = "class_hierarchy" >::: ["from_source" >:: test_from_source] |> Test.run
