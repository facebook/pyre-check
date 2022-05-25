(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open OUnit2
open Ast
open Test
open Interprocedural

let test_get_module_and_definition context =
  let assert_get_module_and_definition ~source ~target ~expected =
    let resolution =
      Test.ScratchProject.setup ~context ["test.py", source]
      |> Test.ScratchProject.build_resolution
      |> Analysis.Resolution.global_resolution
    in
    let actual =
      Target.get_module_and_definition ~resolution target
      >>| fun (qualifier, { Node.value = { Statement.Define.body; _ }; _ }) -> qualifier, body
    in
    let equal (first_qualifier, first_body) (second_qualifier, second_body) =
      Reference.equal first_qualifier second_qualifier
      && List.equal
           (fun left right -> Statement.location_insensitive_compare left right = 0)
           first_body
           second_body
    in
    let printer = function
      | None -> "None"
      | Some (qualifier, body) ->
          Format.sprintf "%s: %s" (Reference.show qualifier) (List.to_string body ~f:Statement.show)
    in
    assert_equal ~printer ~cmp:(Option.equal equal) actual expected
  in
  assert_get_module_and_definition
    ~source:
      {|
    class C:
      @property
      def foo(self) -> int:
        return 0
      @foo.setter
      def foo(self, value: int) -> None:
        self._foo = value
  |}
    ~target:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = PropertySetter })
    ~expected:
      (Some
         ( Reference.create "test",
           [
             +Statement.Statement.Assign
                {
                  Statement.Assign.target = !"$parameter$self._foo";
                  annotation = None;
                  value = !"$parameter$value";
                };
             +Statement.Statement.Return { Statement.Return.is_implicit = true; expression = None };
           ] ))


let test_resolve_method context =
  let assert_get_resolve_method ~source ~class_type ~method_name expected =
    let resolution =
      Test.ScratchProject.setup ~context ["test.py", source]
      |> Test.ScratchProject.build_global_resolution
    in
    assert_equal
      ~printer:(show_optional Target.show_pretty)
      expected
      (Target.resolve_method ~resolution ~class_type ~method_name)
  in
  assert_get_resolve_method
    ~source:
      {|
      from typing import Callable
      class Foo:
        method: Callable(cls.named)[[int], str]
     |}
    ~class_type:(Primitive "test.Foo")
    ~method_name:"method"
    (Some (Target.Method { class_name = "cls"; method_name = "named"; kind = Normal }));
  assert_get_resolve_method
    ~source:
      {|
      from typing import Callable
      class Foo:
        method: BoundMethod[Callable(cls.named)[[int], str], Foo]
     |}
    ~class_type:(Primitive "test.Foo")
    ~method_name:"method"
    (Some (Target.Method { class_name = "cls"; method_name = "named"; kind = Normal }));
  ()


let () =
  "callable"
  >::: [
         "get_module_and_definition" >:: test_get_module_and_definition;
         "resolve_method" >:: test_resolve_method;
       ]
  |> Test.run
