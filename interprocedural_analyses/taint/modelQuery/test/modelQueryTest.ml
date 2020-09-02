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
    let actual = TaintModelQuery.ModelQuery.apply_query_rule ~resolution ~rule ~callable in
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
        Model.ModelQuery.query = [Model.ModelQuery.NameConstraint "foo"];
        productions = [Model.ModelQuery.ReturnTaint [source "Test"]];
        rule_kind = Model.ModelQuery.FunctionModel;
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
        Model.ModelQuery.query =
          [Model.ModelQuery.NameConstraint "foo"; Model.ModelQuery.NameConstraint "bar"];
        productions = [Model.ModelQuery.ReturnTaint [source "Test"]];
        rule_kind = Model.ModelQuery.FunctionModel;
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
        Model.ModelQuery.query =
          [Model.ModelQuery.NameConstraint "foo"; Model.ModelQuery.NameConstraint "bar"];
        productions = [Model.ModelQuery.ReturnTaint [source "Test"]];
        rule_kind = Model.ModelQuery.FunctionModel;
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
        Model.ModelQuery.query = [Model.ModelQuery.NameConstraint "foo"];
        productions = [Model.ModelQuery.ReturnTaint [source "Test"]];
        rule_kind = Model.ModelQuery.FunctionModel;
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
        Model.ModelQuery.query = [Model.ModelQuery.NameConstraint "foo"];
        productions = [Model.ModelQuery.ReturnTaint [source "Test"]];
        rule_kind = Model.ModelQuery.MethodModel;
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
        Model.ModelQuery.query = [Model.ModelQuery.NameConstraint "foo"];
        productions =
          [
            Model.ModelQuery.ReturnTaint [source "Test"];
            Model.ModelQuery.ParameterTaint { name = "x"; taint = [source "Test"] };
          ];
        rule_kind = Model.ModelQuery.MethodModel;
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
        Model.ModelQuery.query = [Model.ModelQuery.NameConstraint "foo"];
        productions = [Model.ModelQuery.AllParametersTaint [source "Test"]];
        rule_kind = Model.ModelQuery.MethodModel;
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
      ]


let () = "modelQuery" >::: ["apply_rule" >:: test_apply_rule] |> Test.run
