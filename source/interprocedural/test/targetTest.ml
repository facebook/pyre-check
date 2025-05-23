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
    let pyre_api =
      Test.ScratchProject.setup ~context ["test.py", source]
      |> Test.ScratchProject.pyre_pysa_read_only_api
    in
    let actual =
      target
      |> Target.from_regular
      |> Target.get_module_and_definition_for_test ~pyre_api
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
    ~target:
      (Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = PropertySetter })
    ~expected:
      (Some
         ( Reference.create "test",
           [
             +Statement.Statement.Assign
                {
                  Statement.Assign.target = !"$parameter$self._foo";
                  annotation = None;
                  value = Some !"$parameter$value";
                  origin = None;
                };
             +Statement.Statement.Return { Statement.Return.is_implicit = true; expression = None };
           ] ))


let test_resolve_method context =
  let assert_get_resolve_method ~source ~class_type ~method_name expected =
    let pyre_api =
      Test.ScratchProject.setup ~context ["test.py", source]
      |> Test.ScratchProject.pyre_pysa_read_only_api
    in
    assert_equal
      ~printer:(show_optional Target.show_pretty)
      expected
      (Target.resolve_method ~pyre_api ~class_type ~method_name)
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
    (Some
       (Target.Regular.Method { class_name = "cls"; method_name = "named"; kind = Normal }
       |> Target.from_regular));
  assert_get_resolve_method
    ~source:
      {|
      from typing import Callable
      class Foo:
        method: BoundMethod[Callable(cls.named)[[int], str], Foo]
     |}
    ~class_type:(Primitive "test.Foo")
    ~method_name:"method"
    (Some
       (Target.Regular.Method { class_name = "cls"; method_name = "named"; kind = Normal }
       |> Target.from_regular));
  ()


let test_pretty_print _ =
  let assert_equal ~expected ~actual =
    assert_equal ~cmp:String.equal ~printer:Fn.id expected (Target.show_pretty actual)
  in
  assert_equal
    ~expected:"foo[]"
    ~actual:
      (Target.Parameterized
         {
           regular = Target.Regular.Function { name = "foo"; kind = Normal };
           parameters = Target.ParameterMap.empty;
         });
  assert_equal
    ~expected:"foo[local(x)=bar, local(y)=baz]"
    ~actual:
      (Target.Parameterized
         {
           regular = Target.Regular.Function { name = "foo"; kind = Normal };
           parameters =
             [
               ( AccessPath.Root.Variable "x",
                 Target.Regular.Function { name = "bar"; kind = Normal } |> Target.from_regular );
               ( AccessPath.Root.Variable "y",
                 Target.Regular.Function { name = "baz"; kind = Normal } |> Target.from_regular );
             ]
             |> Target.ParameterMap.of_alist_exn;
         });
  ()


let test_contain_recursive_targets _ =
  let assert_contain_recursive_target ~result ~target =
    assert_equal ~printer:Bool.to_string result (Target.contain_recursive_target target)
  in
  assert_contain_recursive_target
    ~result:false
    ~target:(Target.Regular.Function { name = "foo"; kind = Normal } |> Target.from_regular);
  assert_contain_recursive_target
    ~result:false
    ~target:
      (Target.Parameterized
         {
           regular = Target.Regular.Function { name = "foo"; kind = Normal };
           parameters =
             [
               ( AccessPath.Root.Variable "x",
                 Target.Regular.Function { name = "bar"; kind = Normal } |> Target.from_regular );
             ]
             |> Target.ParameterMap.of_alist_exn;
         });
  assert_contain_recursive_target
    ~result:true
    ~target:
      (Target.Parameterized
         {
           regular = Target.Regular.Function { name = "foo"; kind = Normal };
           parameters =
             [
               ( AccessPath.Root.Variable "x",
                 Target.Regular.Function { name = "foo"; kind = Normal } |> Target.from_regular );
             ]
             |> Target.ParameterMap.of_alist_exn;
         });
  assert_contain_recursive_target
    ~result:true
    ~target:
      (Target.Parameterized
         {
           regular = Target.Regular.Function { name = "foo"; kind = Normal };
           parameters =
             [
               ( AccessPath.Root.Variable "x",
                 Target.Parameterized
                   {
                     regular = Target.Regular.Function { name = "bar"; kind = Normal };
                     parameters =
                       [
                         ( AccessPath.Root.Variable "y",
                           Target.Regular.Function { name = "foo"; kind = Normal }
                           |> Target.from_regular );
                       ]
                       |> Target.ParameterMap.of_alist_exn;
                   } );
             ]
             |> Target.ParameterMap.of_alist_exn;
         })


let test_target_depth _ =
  let assert_depth ~result ~target =
    assert_equal ~printer:Int.to_string result (Target.depth target)
  in
  assert_depth
    ~result:1
    ~target:(Target.Regular.Function { name = "foo"; kind = Normal } |> Target.from_regular);
  assert_depth
    ~result:1
    ~target:
      (Target.Parameterized
         {
           regular = Target.Regular.Function { name = "foo"; kind = Normal };
           parameters = Target.ParameterMap.empty;
         });
  assert_depth
    ~result:2
    ~target:
      (Target.Parameterized
         {
           regular = Target.Regular.Function { name = "foo"; kind = Normal };
           parameters =
             [
               ( AccessPath.Root.Variable "x",
                 Target.Regular.Function { name = "bar"; kind = Normal } |> Target.from_regular );
             ]
             |> Target.ParameterMap.of_alist_exn;
         });
  assert_depth
    ~result:3
    ~target:
      (Target.Parameterized
         {
           regular = Target.Regular.Function { name = "foo"; kind = Normal };
           parameters =
             [
               ( AccessPath.Root.Variable "x",
                 Target.Parameterized
                   {
                     regular = Target.Regular.Function { name = "bar"; kind = Normal };
                     parameters =
                       [
                         ( AccessPath.Root.Variable "y",
                           Target.Regular.Function { name = "baz"; kind = Normal }
                           |> Target.from_regular );
                       ]
                       |> Target.ParameterMap.of_alist_exn;
                   } );
             ]
             |> Target.ParameterMap.of_alist_exn;
         });
  assert_depth
    ~result:3
    ~target:
      (Target.Parameterized
         {
           regular = Target.Regular.Function { name = "foo"; kind = Normal };
           parameters =
             [
               ( AccessPath.Root.Variable "x",
                 Target.Parameterized
                   {
                     regular = Target.Regular.Function { name = "bar"; kind = Normal };
                     parameters =
                       [
                         ( AccessPath.Root.Variable "y",
                           Target.Regular.Function { name = "baz"; kind = Normal }
                           |> Target.from_regular );
                       ]
                       |> Target.ParameterMap.of_alist_exn;
                   } );
               ( AccessPath.Root.Variable "y",
                 Target.Regular.Function { name = "test"; kind = Normal } |> Target.from_regular );
             ]
             |> Target.ParameterMap.of_alist_exn;
         })


let () =
  "callable"
  >::: [
         "get_module_and_definition" >:: test_get_module_and_definition;
         "resolve_method" >:: test_resolve_method;
         "pretty_print" >:: test_pretty_print;
         "contain_recursive_targets" >:: test_contain_recursive_targets;
         "target_depth" >:: test_target_depth;
       ]
  |> Test.run
