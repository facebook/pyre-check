(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Core
open Pyre
open Test
open Taint
open Interprocedural

module ModelParser = struct
  include Taint.ModelParser
  include Taint.ModelParser.Internal
end

open Taint.ModelParser.Internal.ModelQuery

type query_rule_element = ModelParser.annotation_kind * ModelParser.taint_annotation
[@@deriving show, compare]

let test_apply_rule context =
  let source ?subkind name =
    let source =
      match subkind with
      | None -> Sources.NamedSource name
      | Some subkind -> Sources.ParametricSource { source_name = name; subkind }
    in
    ModelParser.Source
      {
        source;
        breadcrumbs = [];
        via_features = [];
        path = [];
        leaf_names = [];
        leaf_name_provided = false;
        trace_length = None;
      }
  in
  let sink name =
    let sink = Sinks.NamedSink name in
    ModelParser.Sink
      {
        sink;
        breadcrumbs = [];
        via_features = [];
        path = [];
        leaf_names = [];
        leaf_name_provided = false;
        trace_length = None;
      }
  in
  let assert_applied_rules ~source ~rule ~callable ~expected =
    let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
    in
    let resolution = Analysis.GlobalResolution.create global_environment in
    let actual =
      TaintModelQuery.ModelQuery.apply_callable_query_rule
        ~verbose:false
        ~resolution
        ~rule
        ~callable
      |> String.Map.data
      |> List.concat
    in
    assert_equal
      ~cmp:(List.equal (fun left right -> compare_query_rule_element left right = 0))
      ~printer:(List.to_string ~f:show_query_rule_element)
      expected
      actual
  in
  let assert_applied_rules_for_attribute ~source ~rule ~name ~annotation ~expected =
    let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_global_environment
    in
    let resolution = Analysis.GlobalResolution.create global_environment in
    let annotation_expression =
      annotation
      >>= fun annotation ->
      try
        let parsed = PyreParser.Parser.parse_exn [annotation] in
        match parsed with
        | [{ Ast.Node.value = Expression expression; _ }] -> Some expression
        | _ -> None
      with
      | _ -> None
    in
    let actual =
      TaintModelQuery.ModelQuery.apply_attribute_query_rule
        ~verbose:false
        ~resolution
        ~rule
        ~name:(Ast.Reference.create name)
        ~annotation:annotation_expression
      |> String.Map.data
      |> List.concat
    in
    assert_equal
      ~cmp:(List.equal (fun left right -> ModelParser.compare_taint_annotation left right = 0))
      ~printer:(List.to_string ~f:ModelParser.show_taint_annotation)
      expected
      actual
  in
  assert_applied_rules
    ~source:{|
      def foo(): ...
      |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Matches (Re2.create_exn "foo"))];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:{|
      def foo(): ...
      |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Equals "foo")];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[];
  assert_applied_rules
    ~source:{|
      def foo(): ...
      |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Equals "test.foo")];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];

  (* Test multiple constraints. *)
  assert_applied_rules
    ~source:{|
      def foo(): ...
      def barfoo(): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            NameConstraint (Matches (Re2.create_exn "foo"));
            NameConstraint (Matches (Re2.create_exn "bar"));
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.barfoo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:{|
      def foo(): ...
      def barfoo(): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            NameConstraint (Matches (Re2.create_exn "foo"));
            NameConstraint (Matches (Re2.create_exn "bar"));
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[];

  (* Method vs. callable productions. *)
  assert_applied_rules
    ~source:{|
      class C:
        def foo(): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Matches (Re2.create_exn "foo"))];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[];

  assert_applied_rules
    ~source:{|
      class C:
        def foo(): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Matches (Re2.create_exn "foo"))];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];

  (* Multiple productions. *)
  assert_applied_rules
    ~source:{|
      class C:
        def foo(x: int): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Matches (Re2.create_exn "foo"))];
        productions =
          [
            ReturnTaint [TaintAnnotation (source "Test")];
            NamedParameterTaint { name = "x"; taint = [TaintAnnotation (source "Test")] };
          ];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ModelParser.ReturnAnnotation, source "Test";
        ( ModelParser.ParameterAnnotation
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
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Matches (Re2.create_exn "foo"))];
        productions =
          [AllParametersTaint { excludes = []; taint = [TaintAnnotation (source "Test")] }];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 0; name = "x"; positional_only = false }),
          source "Test" );
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 1; name = "y"; positional_only = false }),
          source "Test" );
      ];
  assert_applied_rules
    ~source:{|
      class C:
        def foo(x: int, y: str): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Matches (Re2.create_exn "foo"))];
        productions =
          [AllParametersTaint { excludes = ["x"]; taint = [TaintAnnotation (source "Test")] }];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 1; name = "y"; positional_only = false }),
          source "Test" );
      ];
  assert_applied_rules
    ~source:{|
      class C:
        def foo(x: int, y: str): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Matches (Re2.create_exn "foo"))];
        productions =
          [AllParametersTaint { excludes = ["y"]; taint = [TaintAnnotation (source "Test")] }];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 0; name = "x"; positional_only = false }),
          source "Test" );
      ];

  (* Parameter taint. *)
  assert_applied_rules
    ~source:{|
      class C:
        def foo(x: int, y: str): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Matches (Re2.create_exn "foo"))];
        productions =
          [
            ParameterTaint
              {
                where = [NameConstraint (Matches (Re2.create_exn "x"))];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 0; name = "x"; positional_only = false }),
          source "Test" );
      ];
  assert_applied_rules
    ~source:{|
      class C:
        def foo(x: int, y: str): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Matches (Re2.create_exn "foo"))];
        productions =
          [
            ParameterTaint
              {
                where = [Not (NameConstraint (Matches (Re2.create_exn "y")))];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 0; name = "x"; positional_only = false }),
          source "Test" );
      ];
  assert_applied_rules
    ~source:{|
      class C:
        def foo(x: int, y: str): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Matches (Re2.create_exn "foo"))];
        productions =
          [
            ParameterTaint
              {
                where =
                  [
                    ParameterConstraint.AnnotationConstraint
                      (AnnotationNameConstraint (Matches (Re2.create_exn "int")));
                  ];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 0; name = "x"; positional_only = false }),
          source "Test" );
      ];
  assert_applied_rules
    ~source:{|
      class C:
        def foo(x: int, y: str): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Matches (Re2.create_exn "foo"))];
        productions =
          [
            ParameterTaint
              {
                where =
                  [
                    Not
                      (ParameterConstraint.AnnotationConstraint
                         (AnnotationNameConstraint (Equals "int")));
                  ];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 1; name = "y"; positional_only = false }),
          source "Test" );
      ];
  assert_applied_rules
    ~source:
      {|
      from typing import Annotated
      class C:
        def foo(x: int, y: Annotated[str, "foo"]): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Matches (Re2.create_exn "foo"))];
        productions =
          [
            ParameterTaint
              {
                where = [ParameterConstraint.AnnotationConstraint IsAnnotatedTypeConstraint];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 1; name = "y"; positional_only = false }),
          source "Test" );
      ];
  assert_applied_rules
    ~source:{|
      def foo(x: int, y: str): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Matches (Re2.create_exn "foo"))];
        productions =
          [
            ParameterTaint
              {
                where = [NameConstraint (Matches (Re2.create_exn "x"))];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 0; name = "x"; positional_only = false }),
          source "Test" );
      ];
  assert_applied_rules
    ~source:{|
      def foo(x: int, y: str): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Matches (Re2.create_exn "foo"))];
        productions =
          [
            ParameterTaint
              {
                where = [Not (NameConstraint (Matches (Re2.create_exn "y")))];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 0; name = "x"; positional_only = false }),
          source "Test" );
      ];
  assert_applied_rules
    ~source:{|
      def foo(x: int, y: str): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Matches (Re2.create_exn "foo"))];
        productions =
          [
            ParameterTaint
              {
                where =
                  [
                    ParameterConstraint.AnnotationConstraint
                      (AnnotationNameConstraint (Matches (Re2.create_exn "int")));
                  ];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 0; name = "x"; positional_only = false }),
          source "Test" );
      ];
  assert_applied_rules
    ~source:{|
      def foo(x: int, y: str): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Matches (Re2.create_exn "foo"))];
        productions =
          [
            ParameterTaint
              {
                where =
                  [
                    Not
                      (ParameterConstraint.AnnotationConstraint
                         (AnnotationNameConstraint (Equals "int")));
                  ];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 1; name = "y"; positional_only = false }),
          source "Test" );
      ];
  assert_applied_rules
    ~source:
      {|
      from typing import Annotated
      def foo(x: int, y: Annotated[str, "foo"]): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Matches (Re2.create_exn "foo"))];
        productions =
          [
            ParameterTaint
              {
                where = [ParameterConstraint.AnnotationConstraint IsAnnotatedTypeConstraint];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 1; name = "y"; positional_only = false }),
          source "Test" );
      ];
  assert_applied_rules
    ~source:{|
      def foo(x, y): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Matches (Re2.create_exn "foo"))];
        productions =
          [
            ParameterTaint
              {
                where = [ParameterConstraint.IndexConstraint 0];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 0; name = "x"; positional_only = false }),
          source "Test" );
      ];
  assert_applied_rules
    ~source:{|
      def foo(x, y): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Matches (Re2.create_exn "foo"))];
        productions =
          [
            ParameterTaint
              {
                where = [ParameterConstraint.IndexConstraint 1];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 1; name = "y"; positional_only = false }),
          source "Test" );
      ];
  assert_applied_rules
    ~source:{|
      def foo(x, y): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [NameConstraint (Matches (Re2.create_exn "foo"))];
        productions = [ParameterTaint { where = []; taint = [TaintAnnotation (source "Test")] }];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 0; name = "x"; positional_only = false }),
          source "Test" );
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 1; name = "y"; positional_only = false }),
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
        location = Ast.Location.any;
        name = "get_foo";
        query = [ReturnConstraint IsAnnotatedTypeConstraint];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:{|
       def foo(x: int, y: str) -> int: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [ReturnConstraint IsAnnotatedTypeConstraint];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[];
  assert_applied_rules
    ~source:{|
       def foo(x: int, y: str): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [ReturnConstraint IsAnnotatedTypeConstraint];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[];
  assert_applied_rules
    ~source:{|
       def foo(x: typing.Annotated[int, "annotation"], y: str): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint)];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:{|
       def foo(a, b: typing.Annotated[int, "annotation"], c: str): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint)];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  (* Any of. *)
  assert_applied_rules
    ~source:{|
       def foo(a, b: typing.Annotated[int, "annotation"], c: str): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
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
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:{|
       def foo(a, b, c: str) -> typing.Annotated[int, "annotation"]: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
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
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:{|
       def foo(a, b: typing.Annotated[int, DynamicSource(A)], c: str): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint)];
        productions =
          [
            PositionalParameterTaint
              {
                index = 1;
                taint =
                  [
                    ParametricSourceFromAnnotation
                      { source_pattern = "DynamicSource"; kind = "Dynamic" };
                  ];
              };
          ];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 1; name = "b"; positional_only = false }),
          source ~subkind:"A" "Dynamic" );
      ];
  (* Case where we don't match. *)
  assert_applied_rules
    ~source:{|
       def foo(a, b: typing.Annotated[int, DynamicSource(A)], c: str): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint)];
        productions =
          [
            PositionalParameterTaint
              {
                index = 0;
                taint =
                  [
                    ParametricSourceFromAnnotation
                      { source_pattern = "DynamicSource"; kind = "Dynamic" };
                  ];
              };
          ];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[];
  (* All of. *)
  assert_applied_rules
    ~source:
      {|
       def foo(a: typing.Annotated[int, "annotation"])-> typing.Annotated[int, "annotation"]: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            AllOf
              [
                AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint);
                ReturnConstraint IsAnnotatedTypeConstraint;
              ];
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  (* Some cases where we don't match with "AllOf". *)
  assert_applied_rules
    ~source:{|
       def foo(a: typing.Annotated[int, "annotation"]): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            AllOf
              [
                AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint);
                ReturnConstraint IsAnnotatedTypeConstraint;
              ];
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[];
  assert_applied_rules
    ~source:{|
       def foo(a) -> typing.Annotated[int, "annotation"]): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            AllOf
              [
                AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint);
                ReturnConstraint IsAnnotatedTypeConstraint;
              ];
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[];
  (* Named parameters + parametric sources from annotation. *)
  assert_applied_rules
    ~source:{|
       def foo(a, b: typing.Annotated[int, DynamicSource(A)], c: str): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint)];
        productions =
          [
            NamedParameterTaint
              {
                name = "b";
                taint =
                  [
                    ParametricSourceFromAnnotation
                      { source_pattern = "DynamicSource"; kind = "Dynamic" };
                  ];
              };
          ];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 1; name = "b"; positional_only = false }),
          source ~subkind:"A" "Dynamic" );
      ];
  (* All parameters taint + parametric source from annotation. *)
  assert_applied_rules
    ~source:{|
       def foo(a, b: typing.Annotated[int, DynamicSource(A)], c: str): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint)];
        productions =
          [
            AllParametersTaint
              {
                excludes = [];
                taint =
                  [
                    ParametricSourceFromAnnotation
                      { source_pattern = "DynamicSource"; kind = "Dynamic" };
                  ];
              };
          ];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 1; name = "b"; positional_only = false }),
          source ~subkind:"A" "Dynamic" );
      ];
  (* Returned taint + parametric source from annotation. *)
  assert_applied_rules
    ~source:{|
       def foo(a) -> typing.Annotated[int, DynamicSource(B)]: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [ReturnConstraint IsAnnotatedTypeConstraint];
        productions =
          [
            ReturnTaint
              [
                ParametricSourceFromAnnotation { source_pattern = "DynamicSource"; kind = "Dynamic" };
              ];
          ];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source ~subkind:"B" "Dynamic"];
  (* Named parameters + parametric sinks from annotation. *)
  assert_applied_rules
    ~source:{|
       def foo(a, b: typing.Annotated[int, DynamicSink(BSink)], c: str): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint)];
        productions =
          [
            NamedParameterTaint
              {
                name = "b";
                taint =
                  [ParametricSinkFromAnnotation { sink_pattern = "DynamicSink"; kind = "Dynamic" }];
              };
          ];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 1; name = "b"; positional_only = false }),
          ModelParser.Sink
            {
              sink = Sinks.ParametricSink { sink_name = "Dynamic"; subkind = "BSink" };
              breadcrumbs = [];
              via_features = [];
              path = [];
              leaf_names = [];
              leaf_name_provided = false;
              trace_length = None;
            } );
      ];
  (* Type annotation constraint for callables *)
  assert_applied_rules
    ~source:{|
       def foo(a, b: typing.Annotated[str, "foo"], c: str, d: int): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint)];
        productions =
          [
            ParameterTaint
              {
                where = [AnnotationConstraint IsAnnotatedTypeConstraint];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 1; name = "b"; positional_only = false }),
          source "Test" );
      ];
  assert_applied_rules
    ~source:{|
       def foo(a, b: typing.Annotated[str, "foo"], c: str, d: int): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            AnyParameterConstraint
              (AnnotationConstraint (AnnotationNameConstraint (Matches (Re2.create_exn "str"))));
          ];
        productions =
          [
            ParameterTaint
              {
                where =
                  [AnnotationConstraint (AnnotationNameConstraint (Matches (Re2.create_exn "str")))];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 1; name = "b"; positional_only = false }),
          source "Test" );
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 2; name = "c"; positional_only = false }),
          source "Test" );
      ];
  assert_applied_rules
    ~source:{|
       def foo(a, b: typing.Annotated[str, "foo"], c: str, d: int): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [AnyParameterConstraint (AnnotationConstraint (AnnotationNameConstraint (Equals "int")))];
        productions =
          [
            ParameterTaint
              {
                where = [AnnotationConstraint (AnnotationNameConstraint (Equals "int"))];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ( ModelParser.ParameterAnnotation
            (AccessPath.Root.PositionalParameter
               { position = 3; name = "d"; positional_only = false }),
          source "Test" );
      ];
  assert_applied_rules
    ~source:{|
       def foo() -> int: ...
       def bar() -> str: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [ReturnConstraint (AnnotationNameConstraint (Equals "int"))];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:{|
       def foo() -> int: ...
       def bar() -> str: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [ReturnConstraint (AnnotationNameConstraint (Equals "int"))];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.bar"; kind = Normal })
    ~expected:[];
  assert_applied_rules
    ~source:{|
       def foo() -> str: ...
       def bar() -> List[str]: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [ReturnConstraint (AnnotationNameConstraint (Matches (Re2.create_exn "str")))];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:{|
       def foo() -> str: ...
       def bar() -> typing.List[str]: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [ReturnConstraint (AnnotationNameConstraint (Matches (Re2.create_exn "str")))];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.bar"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:{|
       def foo() -> typing.Annotated[str, "foo"]: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [ReturnConstraint IsAnnotatedTypeConstraint];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:{|
       def foo() -> typing.Annotated[str, "foo"]: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [ReturnConstraint (AnnotationNameConstraint (Matches (Re2.create_exn "foo")))];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];

  (* Decorator names. *)
  assert_applied_rules
    ~source:
      {|
       def d1(c): ...
       def d2(c): ...

       @d1
       def foo(a): ...
       @d2
       def bar(a): ...

       @d1
       @d2
       def baz(a): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            AnyDecoratorConstraint
              { name_constraint = Matches (Re2.create_exn "d1"); arguments_constraint = None };
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:
      {|
       def d1(c): ...
       def d2(c): ...

       @d1
       def foo(a): ...
       @d2
       def bar(a): ...

       @d1
       @d2
       def baz(a): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            AnyDecoratorConstraint
              { name_constraint = Matches (Re2.create_exn "d1"); arguments_constraint = None };
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.bar"; kind = Normal })
    ~expected:[];
  assert_applied_rules
    ~source:
      {|
       def d1(c): ...
       def d2(c): ...

       @d1
       def foo(a): ...
       @d2
       def bar(a): ...

       @d1
       @d2
       def baz(a): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            AnyDecoratorConstraint
              { name_constraint = Matches (Re2.create_exn "d1"); arguments_constraint = None };
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.baz"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:
      {|
       from flask import Flask
       app = Flask(__name__)
       @app.route('/')
       def foo(a): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            AnyDecoratorConstraint
              {
                name_constraint = Matches (Re2.create_exn "app.route");
                arguments_constraint = None;
              };
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:
      {|
       def d1(c): ...
       def d2(c): ...

       @d1
       def foo(a): ...
       @d2
       def bar(a): ...

       @d1
       @d2
       def baz(a): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            AnyDecoratorConstraint
              { name_constraint = Matches (Re2.create_exn "d1"); arguments_constraint = None };
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.baz"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:
      {|
       def d1(c): ...
       def d2(c): ...

       @d1
       def foo(a): ...
       @d2
       def bar(a): ...

       @d1
       @d2
       def baz(a): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            AnyDecoratorConstraint
              { name_constraint = Equals "test.d1"; arguments_constraint = None };
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.baz"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:
      {|
       def d1(c): ...
       def d2(c): ...

       @d1
       def foo(a): ...
       @d2
       def bar(a): ...

       @d1
       @d2
       def baz(a): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            AnyDecoratorConstraint
              {
                name_constraint = Equals "test.d1";
                arguments_constraint =
                  Some
                    (ArgumentsConstraint.Contains
                       [
                         {
                           Ast.Expression.Call.Argument.name = None;
                           value = +Ast.Expression.(Expression.Constant (Constant.Integer 1));
                         };
                       ]);
              };
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.baz"; kind = Normal })
    ~expected:[];
  assert_applied_rules
    ~source:
      {|
       def d1(c): ...
       def d2(c): ...

       @d1
       def foo(a): ...
       @d2
       def bar(a): ...

       @d1(1)
       @d2
       def baz(a): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            AnyDecoratorConstraint
              {
                name_constraint = Equals "test.d1";
                arguments_constraint =
                  Some
                    (ArgumentsConstraint.Contains
                       [
                         {
                           Ast.Expression.Call.Argument.name = None;
                           value = +Ast.Expression.(Expression.Constant (Constant.Integer 1));
                         };
                       ]);
              };
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.baz"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:
      {|
       def d1(c): ...
       def d2(c): ...

       @d1
       def foo(a): ...
       @d2
       def bar(a): ...

       @d1(1)
       @d2
       def baz(a): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            AnyDecoratorConstraint
              {
                name_constraint = Equals "test.d1";
                arguments_constraint =
                  Some
                    (ArgumentsConstraint.Contains
                       [
                         {
                           Ast.Expression.Call.Argument.name =
                             Some (Ast.Node.create_with_default_location "arg1");
                           value = +Ast.Expression.(Expression.Constant (Constant.Integer 1));
                         };
                       ]);
              };
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.baz"; kind = Normal })
    ~expected:[];
  assert_applied_rules
    ~source:
      {|
       def d1(c): ...
       def d2(c): ...

       @d1
       def foo(a): ...
       @d2
       def bar(a): ...

       @d1(arg1=1)
       @d2
       def baz(a): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            AnyDecoratorConstraint
              {
                name_constraint = Equals "test.d1";
                arguments_constraint =
                  Some
                    (ArgumentsConstraint.Contains
                       [
                         {
                           Ast.Expression.Call.Argument.name =
                             Some (Ast.Node.create_with_default_location "arg1");
                           value = +Ast.Expression.(Expression.Constant (Constant.Integer 1));
                         };
                       ]);
              };
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.baz"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:
      {|
       def d1(c): ...
       def d2(c): ...

       @d1
       def foo(a): ...
       @d2
       def bar(a): ...

       @d1(1, method="POST")
       @d2
       def baz(a): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            AnyDecoratorConstraint
              {
                name_constraint = Equals "test.d1";
                arguments_constraint =
                  Some
                    (ArgumentsConstraint.Contains
                       [
                         {
                           Ast.Expression.Call.Argument.name =
                             Some (Ast.Node.create_with_default_location "method");
                           value =
                             +Ast.Expression.(
                                Expression.Constant
                                  (Constant.String (Ast.Expression.StringLiteral.create "POST")));
                         };
                         {
                           Ast.Expression.Call.Argument.name = None;
                           value = +Ast.Expression.(Expression.Constant (Constant.Integer 1));
                         };
                       ]);
              };
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.baz"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:
      {|
       def d1(c): ...
       def d2(c): ...

       @d1(1)
       def foo(a): ...
       @d2
       def bar(a): ...

       @d1(1, method="POST")
       @d2
       def baz(a): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            AnyDecoratorConstraint
              {
                name_constraint = Equals "test.d1";
                arguments_constraint =
                  Some
                    (ArgumentsConstraint.Equals
                       [
                         {
                           Ast.Expression.Call.Argument.name =
                             Some (Ast.Node.create_with_default_location "method");
                           value =
                             +Ast.Expression.(
                                Expression.Constant
                                  (Constant.String (Ast.Expression.StringLiteral.create "POST")));
                         };
                         {
                           Ast.Expression.Call.Argument.name = None;
                           value = +Ast.Expression.(Expression.Constant (Constant.Integer 1));
                         };
                       ]);
              };
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[];
  assert_applied_rules
    ~source:
      {|
       def d1(c): ...
       def d2(c): ...

       @d1(1)
       def foo(a): ...
       @d2
       def bar(a): ...

       @d1(1, method="POST")
       @d2
       def baz(a): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            AnyDecoratorConstraint
              {
                name_constraint = Equals "test.d1";
                arguments_constraint =
                  Some
                    (ArgumentsConstraint.Equals
                       [
                         {
                           Ast.Expression.Call.Argument.name =
                             Some (Ast.Node.create_with_default_location "method");
                           value =
                             +Ast.Expression.(
                                Expression.Constant
                                  (Constant.String (Ast.Expression.StringLiteral.create "POST")));
                         };
                         {
                           Ast.Expression.Call.Argument.name = None;
                           value = +Ast.Expression.(Expression.Constant (Constant.Integer 1));
                         };
                       ]);
              };
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.baz"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];

  assert_applied_rules
    ~source:
      {|
      class C:
        def foo(): ...
      class D:
        def foo(): ...
      class DC:
        def foo(): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [ParentConstraint (NameSatisfies (Matches (Re2.create_exn "C")))];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:
      {|
      class C:
        def foo(): ...
      class D:
        def foo(): ...
      class DC:
        def foo(): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [ParentConstraint (NameSatisfies (Matches (Re2.create_exn "C")))];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.D"; method_name = "foo"; kind = Normal })
    ~expected:[];

  assert_applied_rules
    ~source:
      {|
      class C:
        def foo(): ...
      class D:
        def foo(): ...
      class DC:
        def foo(): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [ParentConstraint (NameSatisfies (Matches (Re2.create_exn "C")))];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.DC"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:
      {|
      @d1
      class A:
        def foo(): ...
      @d2
      class B:
        def foo(): ...
      @d3
      class C:
        def foo(): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            ParentConstraint
              (DecoratorSatisfies
                 { name_constraint = Matches (Re2.create_exn "d2"); arguments_constraint = None });
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:
      {|
      @d1
      class A:
        def foo(): ...
      @d2
      class B:
        def foo(): ...
      @d3
      class C:
        def foo(): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            ParentConstraint
              (DecoratorSatisfies
                 { name_constraint = Matches (Re2.create_exn "4"); arguments_constraint = None });
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_applied_rules
    ~source:
      {|
      @d1
      class A:
        def foo(): ...
      @d2
      class B:
        def foo(): ...
      @d1(1)
      @d3
      class C:
        def foo(): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            ParentConstraint
              (DecoratorSatisfies
                 {
                   name_constraint = Equals "test.d1";
                   arguments_constraint =
                     Some
                       (ArgumentsConstraint.Contains
                          [
                            {
                              Ast.Expression.Call.Argument.name = None;
                              value = +Ast.Expression.(Expression.Constant (Constant.Integer 1));
                            };
                          ]);
                 });
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_applied_rules
    ~source:
      {|
      @d1
      class A:
        def foo(): ...
      @d2
      class B:
        def foo(): ...
      @d1(1)
      @d3
      class C:
        def foo(): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            ParentConstraint
              (DecoratorSatisfies
                 {
                   name_constraint = Matches (Re2.create_exn "d1");
                   arguments_constraint =
                     Some
                       (ArgumentsConstraint.Contains
                          [
                            {
                              Ast.Expression.Call.Argument.name = None;
                              value = +Ast.Expression.(Expression.Constant (Constant.Integer 1));
                            };
                          ]);
                 });
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];

  (* Test attribute models. *)
  assert_applied_rules_for_attribute
    ~source:{|
      class C:
        x: ...
      class D(C):
        y: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [ParentConstraint (NameSatisfies (Matches (Re2.create_exn "C")))];
        productions = [AttributeTaint [TaintAnnotation (source "Test")]];
        rule_kind = AttributeModel;
      }
    ~name:"test.C.x"
    ~annotation:None
    ~expected:[source "Test"];
  assert_applied_rules_for_attribute
    ~source:{|
      class C:
        x: ...
      class D(C):
        y: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [ParentConstraint (NameSatisfies (Matches (Re2.create_exn "C")))];
        productions = [AttributeTaint [TaintAnnotation (sink "Test")]];
        rule_kind = AttributeModel;
      }
    ~name:"test.C.x"
    ~annotation:None
    ~expected:[sink "Test"];
  assert_applied_rules_for_attribute
    ~source:{|
      class C:
        x: ...
      class D(C):
        y: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [ParentConstraint (NameSatisfies (Matches (Re2.create_exn "C")))];
        productions = [AttributeTaint [TaintAnnotation (source "Test")]];
        rule_kind = AttributeModel;
      }
    ~name:"test.D.y"
    ~annotation:None
    ~expected:[];
  assert_applied_rules_for_attribute
    ~source:{|
      class C:
        x: ...
      class D(C):
        y: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [ParentConstraint (Extends { class_name = "test.C"; is_transitive = false })];
        productions = [AttributeTaint [TaintAnnotation (source "Test")]];
        rule_kind = AttributeModel;
      }
    ~name:"test.C.x"
    ~annotation:None
    ~expected:[source "Test"];
  ();
  assert_applied_rules_for_attribute
    ~source:{|
      class C:
        x: ...
      class D(C):
        y: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [ParentConstraint (Extends { class_name = "test.C"; is_transitive = false })];
        productions = [AttributeTaint [TaintAnnotation (source "Test")]];
        rule_kind = AttributeModel;
      }
    ~name:"test.D.y"
    ~annotation:None
    ~expected:[source "Test"];
  assert_applied_rules_for_attribute
    ~source:
      {|
      class C:
        x: ...
      class D(C):
        y: ...
      class E:
        z: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [ParentConstraint (Extends { class_name = "test.C"; is_transitive = false })];
        productions = [AttributeTaint [TaintAnnotation (source "Test")]];
        rule_kind = AttributeModel;
      }
    ~name:"test.E.z"
    ~annotation:None
    ~expected:[];
  assert_applied_rules_for_attribute
    ~source:{|
      class C:
        x: int
        y: str
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [AnnotationConstraint (AnnotationNameConstraint (Equals "int"))];
        productions = [AttributeTaint [TaintAnnotation (source "Test")]];
        rule_kind = AttributeModel;
      }
    ~name:"test.C.x"
    ~annotation:(Some "int")
    ~expected:[source "Test"];
  assert_applied_rules_for_attribute
    ~source:{|
      class C:
        x: int
        y: str
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [AnnotationConstraint (AnnotationNameConstraint (Equals "int"))];
        productions = [AttributeTaint [TaintAnnotation (source "Test")]];
        rule_kind = AttributeModel;
      }
    ~name:"test.C.y"
    ~annotation:(Some "str")
    ~expected:[];
  assert_applied_rules_for_attribute
    ~source:
      {|
      class Foo1:
        ...
      class Foo2:
        ...
      class Bar:
        ...
      class C:
        x: Foo1
        y: Foo2
        z: Bar
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [AnnotationConstraint (AnnotationNameConstraint (Matches (Re2.create_exn "Foo")))];
        productions = [AttributeTaint [TaintAnnotation (source "Test")]];
        rule_kind = AttributeModel;
      }
    ~name:"test.C.x"
    ~annotation:(Some "typing.Type[Foo1]")
    ~expected:[source "Test"];
  assert_applied_rules_for_attribute
    ~source:
      {|
      class Foo1:
        ...
      class Foo2:
        ...
      class Bar:
        ...
      class C:
        x: Foo1
        y: Foo2
        z: Bar
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [AnnotationConstraint (AnnotationNameConstraint (Matches (Re2.create_exn "Foo")))];
        productions = [AttributeTaint [TaintAnnotation (source "Test")]];
        rule_kind = AttributeModel;
      }
    ~name:"test.C.y"
    ~annotation:(Some "typing.Type[Foo2]")
    ~expected:[source "Test"];
  assert_applied_rules_for_attribute
    ~source:
      {|
      class Foo1:
        ...
      class Foo2:
        ...
      class Bar:
        ...
      class C:
        x: Foo1
        y: Foo2
        z: Bar
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [AnnotationConstraint (AnnotationNameConstraint (Matches (Re2.create_exn "Foo")))];
        productions = [AttributeTaint [TaintAnnotation (source "Test")]];
        rule_kind = AttributeModel;
      }
    ~name:"test.C.z"
    ~annotation:(Some "typing.Type[Bar]")
    ~expected:[];
  assert_applied_rules_for_attribute
    ~source:
      {|
      from typing import Annotated
      class C:
        x: int
        y: Annotated[str, "foo"]
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [AnnotationConstraint IsAnnotatedTypeConstraint];
        productions = [AttributeTaint [TaintAnnotation (source "Test")]];
        rule_kind = AttributeModel;
      }
    ~name:"test.C.x"
    ~annotation:(Some "int")
    ~expected:[];
  assert_applied_rules_for_attribute
    ~source:
      {|
      from typing import Annotated
      class C:
        x: int
        y: Annotated[str, "foo"]
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [AnnotationConstraint IsAnnotatedTypeConstraint];
        productions = [AttributeTaint [TaintAnnotation (source "Test")]];
        rule_kind = AttributeModel;
      }
    ~name:"test.C.y"
    ~annotation:(Some "typing.Annotated[str, \"foo\"]")
    ~expected:[source "Test"];

  (* Test 'Not' clause *)
  assert_applied_rules
    ~source:{|
      def foo(): ...
      def barfoo(): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            NameConstraint (Matches (Re2.create_exn "foo"));
            Not (NameConstraint (Matches (Re2.create_exn "bar")));
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:{|
      def foo(): ...
      def barfoo(): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            NameConstraint (Matches (Re2.create_exn "foo"));
            Not (NameConstraint (Matches (Re2.create_exn "bar")));
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.barfoo"; kind = Normal })
    ~expected:[];
  assert_applied_rules
    ~source:
      {|
       def foo(a) -> typing.Annotated[int, DynamicSource(B)]: ...
       def bar(b): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [Not (ReturnConstraint IsAnnotatedTypeConstraint)];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[];
  assert_applied_rules
    ~source:
      {|
      class C:
        def foo(): ...
      class D:
        def foo(): ...
      class DC:
        def foo(): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            ParentConstraint (NameSatisfies (Matches (Re2.create_exn "C")));
            Not (ParentConstraint (NameSatisfies (Matches (Re2.create_exn "D"))));
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules
    ~source:
      {|
      class C:
        def foo(): ...
      class D:
        def foo(): ...
      class DC:
        def foo(): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query =
          [
            ParentConstraint (NameSatisfies (Matches (Re2.create_exn "C")));
            Not (ParentConstraint (NameSatisfies (Matches (Re2.create_exn "D"))));
          ];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.DC"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_applied_rules
    ~source:
      {|
       def foo(a) -> typing.Annotated[int, DynamicSource(B)]: ...
       def bar(b): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [Not (ReturnConstraint IsAnnotatedTypeConstraint)];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = FunctionModel;
      }
    ~callable:(Target.Function { name = "test.bar"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  assert_applied_rules_for_attribute
    ~source:
      {|
      class C:
        x: ...
      class D(C):
        y: ...
      class E:
        z: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [Not (ParentConstraint (Extends { class_name = "test.C"; is_transitive = false }))];
        productions = [AttributeTaint [TaintAnnotation (source "Test")]];
        rule_kind = AttributeModel;
      }
    ~name:"test.C.x"
    ~annotation:None
    ~expected:[];
  assert_applied_rules_for_attribute
    ~source:
      {|
      class C:
        x: ...
      class D(C):
        y: ...
      class E:
        z: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [Not (ParentConstraint (Extends { class_name = "test.C"; is_transitive = false }))];
        productions = [AttributeTaint [TaintAnnotation (source "Test")]];
        rule_kind = AttributeModel;
      }
    ~name:"test.D.y"
    ~annotation:None
    ~expected:[];
  assert_applied_rules_for_attribute
    ~source:
      {|
      class C:
        x: ...
      class D(C):
        y: ...
      class E:
        z: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [Not (ParentConstraint (Extends { class_name = "test.C"; is_transitive = false }))];
        productions = [AttributeTaint [TaintAnnotation (source "Test")]];
        rule_kind = AttributeModel;
      }
    ~name:"test.E.z"
    ~annotation:None
    ~expected:[source "Test"];

  (* Test transitive extends *)
  assert_applied_rules_for_attribute
    ~source:
      {|
      class C:
        x: ...
      class D(C):
        y: ...
      class E(D):
        z: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [ParentConstraint (Extends { class_name = "test.C"; is_transitive = true })];
        productions = [AttributeTaint [TaintAnnotation (source "Test")]];
        rule_kind = AttributeModel;
      }
    ~name:"test.E.z"
    ~annotation:None
    ~expected:[source "Test"];
  assert_applied_rules_for_attribute
    ~source:
      {|
      class C:
        x: ...
      class D(C):
        y: ...
      class E(D):
        z: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [ParentConstraint (Extends { class_name = "test.C"; is_transitive = true })];
        productions = [AttributeTaint [TaintAnnotation (source "Test")]];
        rule_kind = AttributeModel;
      }
    ~name:"test.D.y"
    ~annotation:None
    ~expected:[source "Test"];
  assert_applied_rules_for_attribute
    ~source:
      {|
      class C:
        x: ...
      class D(C):
        y: ...
      class E(D):
        z: ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [ParentConstraint (Extends { class_name = "test.C"; is_transitive = true })];
        productions = [AttributeTaint [TaintAnnotation (source "Test")]];
        rule_kind = AttributeModel;
      }
    ~name:"test.C.x"
    ~annotation:None
    ~expected:[source "Test"];
  assert_applied_rules
    ~source:
      {|
      class A:
        def foo(): ...
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D:
        def foo(): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [Not (ParentConstraint (Extends { class_name = "test.A"; is_transitive = true }))];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_applied_rules
    ~source:
      {|
      class A:
        def foo(): ...
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D:
        def foo(): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [Not (ParentConstraint (Extends { class_name = "test.A"; is_transitive = true }))];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_applied_rules
    ~source:
      {|
      class A:
        def foo(): ...
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D:
        def foo(): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [Not (ParentConstraint (Extends { class_name = "test.A"; is_transitive = true }))];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_applied_rules
    ~source:
      {|
      class A:
        def foo(): ...
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D:
        def foo(): ...
     |}
    ~rule:
      {
        location = Ast.Location.any;
        name = "get_foo";
        query = [Not (ParentConstraint (Extends { class_name = "test.A"; is_transitive = true }))];
        productions = [ReturnTaint [TaintAnnotation (source "Test")]];
        rule_kind = MethodModel;
      }
    ~callable:(Target.Method { class_name = "test.D"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParser.ReturnAnnotation, source "Test"];
  ()


let () = "modelQuery" >::: ["apply_rule" >:: test_apply_rule] |> Test.run
