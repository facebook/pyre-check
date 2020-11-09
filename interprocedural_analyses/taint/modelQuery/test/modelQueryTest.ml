(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Core
open Test
open Taint
open Model.ModelQuery

type query_rule_element = Taint.Model.annotation_kind * Taint.Model.taint_annotation
[@@deriving show, compare]

let test_apply_rule context =
  let source name =
    Model.Source
      { source = Sources.NamedSource name; breadcrumbs = []; path = []; leaf_name_provided = false }
  in
  let assert_applied_rules ~source ~rule ~callable ~expected =
    let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
    in
    let resolution =
      Analysis.AnnotatedGlobalEnvironment.read_only global_environment
      |> Analysis.GlobalResolution.create
    in
    let actual =
      TaintModelQuery.ModelQuery.apply_query_rule ~verbose:false ~resolution ~rule ~callable
    in
    assert_equal
      ~cmp:(List.equal (fun left right -> compare_query_rule_element left right = 0))
      ~printer:(List.to_string ~f:show_query_rule_element)
      expected
      actual
  in
  assert_applied_rules
    ~source:{|
      def foo(): ...
      |}
    ~rule:
      {
        name = None;
        query = [NameConstraint "foo"];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(`Function "test.foo")
    ~expected:[Taint.Model.ReturnAnnotation, source "Test"];

  (* Test multiple constraints. *)
  assert_applied_rules
    ~source:{|
      def foo(): ...
      def barfoo(): ...
     |}
    ~rule:
      {
        name = None;
        query = [NameConstraint "foo"; NameConstraint "bar"];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(`Function "test.barfoo")
    ~expected:[Taint.Model.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:{|
      def foo(): ...
      def barfoo(): ...
     |}
    ~rule:
      {
        name = None;
        query = [NameConstraint "foo"; NameConstraint "bar"];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(`Function "test.foo")
    ~expected:[];

  (* Method vs. callable productions. *)
  assert_applied_rules
    ~source:{|
      class C:
        def foo(): ...
     |}
    ~rule:
      {
        name = None;
        query = [NameConstraint "foo"];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(`Method { Interprocedural.Callable.class_name = "test.C"; method_name = "foo" })
    ~expected:[];

  assert_applied_rules
    ~source:{|
      class C:
        def foo(): ...
     |}
    ~rule:
      {
        name = None;
        query = [NameConstraint "foo"];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = MethodModel;
      }
    ~callable:(`Method { Interprocedural.Callable.class_name = "test.C"; method_name = "foo" })
    ~expected:[Taint.Model.ReturnAnnotation, source "Test"];

  (* Multiple productions. *)
  assert_applied_rules
    ~source:{|
      class C:
        def foo(x: int): ...
     |}
    ~rule:
      {
        name = None;
        query = [NameConstraint "foo"];
        productions =
          [
            ReturnTaint [TaintAnnotation (source "Test")];
            ParameterTaint { name = "x"; taint = [TaintAnnotation (source "Test")] };
          ];
        rule_kind = MethodModel;
      }
    ~callable:(`Method { Interprocedural.Callable.class_name = "test.C"; method_name = "foo" })
    ~expected:
      [
        Taint.Model.ReturnAnnotation, source "Test";
        ( Taint.Model.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 0; name = "x"; positional_only = false }),
          source "Test" );
      ];
  (* All parameter taint. *)
  assert_applied_rules
    ~source:{|
      class C:
        def foo(x: int, y: str): ...
     |}
    ~rule:
      {
        name = None;
        query = [NameConstraint "foo"];
        productions = [AllParametersTaint [TaintAnnotation (source "Test")]];
        rule_kind = MethodModel;
      }
    ~callable:(`Method { Interprocedural.Callable.class_name = "test.C"; method_name = "foo" })
    ~expected:
      [
        ( Taint.Model.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 1; name = "y"; positional_only = false }),
          source "Test" );
        ( Taint.Model.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 0; name = "x"; positional_only = false }),
          source "Test" );
      ];
  (* Annotated returns. *)
  assert_applied_rules
    ~source:
      {|
       from typing import Annotated
       def foo(x: int, y: str) -> Annotated[int, "annotation"]: ...
     |}
    ~rule:
      {
        name = None;
        query = [ReturnConstraint IsAnnotatedTypeConstraint];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(`Function "test.foo")
    ~expected:[Taint.Model.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:{|
       def foo(x: int, y: str) -> int: ...
     |}
    ~rule:
      {
        name = None;
        query = [ReturnConstraint IsAnnotatedTypeConstraint];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(`Function "test.foo")
    ~expected:[];
  assert_applied_rules
    ~source:{|
       def foo(x: int, y: str): ...
     |}
    ~rule:
      {
        name = None;
        query = [ReturnConstraint IsAnnotatedTypeConstraint];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(`Function "test.foo")
    ~expected:[];
  assert_applied_rules
    ~source:{|
       def foo(x: typing.Annotated[int, "annotation"], y: str): ...
     |}
    ~rule:
      {
        name = None;
        query = [AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint)];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(`Function "test.foo")
    ~expected:[Taint.Model.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:{|
       def foo(a, b: typing.Annotated[int, "annotation"], c: str): ...
     |}
    ~rule:
      {
        name = None;
        query = [AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint)];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(`Function "test.foo")
    ~expected:[Taint.Model.ReturnAnnotation, source "Test"];
  ();
  (* Any of. *)
  assert_applied_rules
    ~source:{|
       def foo(a, b: typing.Annotated[int, "annotation"], c: str): ...
     |}
    ~rule:
      {
        name = None;
        query =
          [
            AnyOf
              [
                AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint);
                ReturnConstraint IsAnnotatedTypeConstraint;
              ];
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(`Function "test.foo")
    ~expected:[Taint.Model.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:{|
       def foo(a, b, c: str) -> typing.Annotated[int, "annotation"]: ...
     |}
    ~rule:
      {
        name = None;
        query =
          [
            AnyOf
              [
                AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint);
                ReturnConstraint IsAnnotatedTypeConstraint;
              ];
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(`Function "test.foo")
    ~expected:[Taint.Model.ReturnAnnotation, source "Test"];
  ()


let () = "modelQuery" >::: ["apply_rule" >:: test_apply_rule] |> Test.run
