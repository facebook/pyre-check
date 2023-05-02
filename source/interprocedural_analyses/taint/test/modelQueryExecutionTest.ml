(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Core
open Test
open Taint
open Interprocedural
open TestHelper
module ModelQuery = ModelParseResult.ModelQuery

type query_element = ModelParseResult.ModelAnnotation.t [@@deriving show, equal]

let source ?subkind name =
  let source =
    match subkind with
    | None -> Sources.NamedSource name
    | Some subkind -> Sources.ParametricSource { source_name = name; subkind }
  in
  ModelParseResult.TaintAnnotation.from_source source


let sink name =
  let sink = Sinks.NamedSink name in
  ModelParseResult.TaintAnnotation.from_sink sink


let test_generated_annotations context =
  let assert_generated_annotations ~source ~query ~callable ~expected =
    let { ScratchProject.BuiltTypeEnvironment.type_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_type_environment
    in
    let global_resolution = Analysis.TypeEnvironment.ReadOnly.global_resolution type_environment in
    let class_hierarchy_graph =
      ClassHierarchyGraph.Heap.from_qualifiers
        ~scheduler:(mock_scheduler ())
        ~environment:type_environment
        ~qualifiers:[Ast.Reference.create "test"]
      |> ClassHierarchyGraph.SharedMemory.from_heap ~store_transitive_children_for:[]
    in
    let actual =
      ModelQueryExecution.CallableQueryExecutor.generate_annotations_from_query_on_target
        ~verbose:false
        ~resolution:global_resolution
        ~class_hierarchy_graph
        ~modelable:
          (ModelQueryExecution.CallableQueryExecutor.make_modelable
             ~resolution:global_resolution
             callable)
        query
    in
    assert_equal
      ~cmp:(List.equal equal_query_element)
      ~printer:(List.to_string ~f:show_query_element)
      expected
      actual
  in
  let assert_generated_annotations_for_attributes ~source ~query ~name ~expected =
    let { ScratchProject.BuiltTypeEnvironment.type_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_type_environment
    in
    let global_resolution = Analysis.TypeEnvironment.ReadOnly.global_resolution type_environment in
    let class_hierarchy_graph =
      ClassHierarchyGraph.Heap.from_qualifiers
        ~scheduler:(mock_scheduler ())
        ~environment:type_environment
        ~qualifiers:[Ast.Reference.create "test"]
      |> ClassHierarchyGraph.SharedMemory.from_heap ~store_transitive_children_for:[]
    in
    let target = name |> Ast.Reference.create |> Target.create_object in
    let actual =
      ModelQueryExecution.AttributeQueryExecutor.generate_annotations_from_query_on_target
        ~verbose:false
        ~resolution:global_resolution
        ~class_hierarchy_graph
        ~modelable:
          (ModelQueryExecution.AttributeQueryExecutor.make_modelable
             ~resolution:global_resolution
             target)
        query
    in
    assert_equal
      ~cmp:(List.equal ModelParseResult.TaintAnnotation.equal)
      ~printer:(List.to_string ~f:ModelParseResult.TaintAnnotation.show)
      expected
      actual
  in
  let assert_generated_annotations_for_globals ~source ~query ~name ~expected =
    let { ScratchProject.BuiltTypeEnvironment.type_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_type_environment
    in
    let global_resolution = Analysis.TypeEnvironment.ReadOnly.global_resolution type_environment in
    let class_hierarchy_graph =
      ClassHierarchyGraph.Heap.from_qualifiers
        ~scheduler:(mock_scheduler ())
        ~environment:type_environment
        ~qualifiers:[Ast.Reference.create "test"]
      |> ClassHierarchyGraph.SharedMemory.from_heap ~store_transitive_children_for:[]
    in
    let target = name |> Ast.Reference.create |> Target.create_object in
    let actual =
      ModelQueryExecution.GlobalVariableQueryExecutor.generate_annotations_from_query_on_target
        ~verbose:false
        ~resolution:global_resolution
        ~class_hierarchy_graph
        ~modelable:
          (ModelQueryExecution.GlobalVariableQueryExecutor.make_modelable
             ~resolution:global_resolution
             target)
        query
    in
    assert_equal
      ~cmp:(List.equal ModelParseResult.TaintAnnotation.equal)
      ~printer:(List.to_string ~f:ModelParseResult.TaintAnnotation.show)
      expected
      actual
  in
  assert_generated_annotations
    ~source:{|
      def foo(): ...
      |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [FullyQualifiedNameConstraint (Matches (Re2.create_exn "foo"))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:{|
      def foo(): ...
      |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [FullyQualifiedNameConstraint (Equals "foo")];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
    ~source:{|
      def foo(): ...
      |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [FullyQualifiedNameConstraint (Equals "test.foo")];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:{|
      def foo(): ...
      |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "foo"))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:{|
      def foo(): ...
      |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Equals "foo")];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:{|
      def foo(): ...
      |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Equals "test.foo")];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[];

  (* Test multiple constraints. *)
  assert_generated_annotations
    ~source:{|
      def foo(): ...
      def barfoo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            NameConstraint (Matches (Re2.create_exn "foo"));
            NameConstraint (Matches (Re2.create_exn "bar"));
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.barfoo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:{|
      def foo(): ...
      def barfoo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            NameConstraint (Matches (Re2.create_exn "foo"));
            NameConstraint (Matches (Re2.create_exn "bar"));
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[];

  (* Method vs. callable productions. *)
  assert_generated_annotations
    ~source:{|
      class C:
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "foo"))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[];

  assert_generated_annotations
    ~source:{|
      class C:
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "foo"))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];

  (* Multiple productions. *)
  assert_generated_annotations
    ~source:{|
      class C:
        def foo(x: int): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "foo"))];
        models =
          [
            Return [TaintAnnotation (source "Test")];
            NamedParameter { name = "x"; taint = [TaintAnnotation (source "Test")] };
          ];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test");
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 0; name = "x"; positional_only = false },
            source "Test" );
      ];
  (* All parameter taint. *)
  assert_generated_annotations
    ~source:{|
      class C:
        def foo(x: int, y: str): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "foo"))];
        models = [AllParameters { excludes = []; taint = [TaintAnnotation (source "Test")] }];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 0; name = "x"; positional_only = false },
            source "Test" );
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 1; name = "y"; positional_only = false },
            source "Test" );
      ];
  assert_generated_annotations
    ~source:{|
      class C:
        def foo(x: int, y: str): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "foo"))];
        models = [AllParameters { excludes = ["x"]; taint = [TaintAnnotation (source "Test")] }];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 1; name = "y"; positional_only = false },
            source "Test" );
      ];
  assert_generated_annotations
    ~source:{|
      class C:
        def foo(x: int, y: str): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "foo"))];
        models = [AllParameters { excludes = ["y"]; taint = [TaintAnnotation (source "Test")] }];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 0; name = "x"; positional_only = false },
            source "Test" );
      ];

  (* Parameter taint. *)
  assert_generated_annotations
    ~source:{|
      class C:
        def foo(x: int, y: str): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "foo"))];
        models =
          [
            Parameter
              {
                where = [NameConstraint (Matches (Re2.create_exn "x"))];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 0; name = "x"; positional_only = false },
            source "Test" );
      ];
  assert_generated_annotations
    ~source:{|
      class C:
        def foo(x: int, y: str): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "foo"))];
        models =
          [
            Parameter
              {
                where = [Not (NameConstraint (Matches (Re2.create_exn "y")))];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 0; name = "x"; positional_only = false },
            source "Test" );
      ];
  assert_generated_annotations
    ~source:{|
      class C:
        def foo(x: int, y: str): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "foo"))];
        models =
          [
            Parameter
              {
                where =
                  [
                    ModelQuery.ParameterConstraint.AnnotationConstraint
                      (NameConstraint (Matches (Re2.create_exn "int")));
                  ];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 0; name = "x"; positional_only = false },
            source "Test" );
      ];
  assert_generated_annotations
    ~source:{|
      class C:
        def foo(x: int, y: str): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "foo"))];
        models =
          [
            Parameter
              {
                where =
                  [
                    Not
                      (ModelQuery.ParameterConstraint.AnnotationConstraint
                         (NameConstraint (Equals "int")));
                  ];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 1; name = "y"; positional_only = false },
            source "Test" );
      ];
  assert_generated_annotations
    ~source:
      {|
      from typing import Annotated
      class C:
        def foo(x: int, y: Annotated[str, "foo"]): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "foo"))];
        models =
          [
            Parameter
              {
                where =
                  [ModelQuery.ParameterConstraint.AnnotationConstraint IsAnnotatedTypeConstraint];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 1; name = "y"; positional_only = false },
            source "Test" );
      ];
  assert_generated_annotations
    ~source:{|
      def foo(x: int, y: str): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "foo"))];
        models =
          [
            Parameter
              {
                where = [NameConstraint (Matches (Re2.create_exn "x"))];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 0; name = "x"; positional_only = false },
            source "Test" );
      ];
  assert_generated_annotations
    ~source:{|
      def foo(x: int, y: str): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "foo"))];
        models =
          [
            Parameter
              {
                where = [Not (NameConstraint (Matches (Re2.create_exn "y")))];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 0; name = "x"; positional_only = false },
            source "Test" );
      ];
  assert_generated_annotations
    ~source:{|
      def foo(x: int, y: str): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "foo"))];
        models =
          [
            Parameter
              {
                where =
                  [
                    ModelQuery.ParameterConstraint.AnnotationConstraint
                      (NameConstraint (Matches (Re2.create_exn "int")));
                  ];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 0; name = "x"; positional_only = false },
            source "Test" );
      ];
  assert_generated_annotations
    ~source:{|
      def foo(x: int, y: str): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "foo"))];
        models =
          [
            Parameter
              {
                where =
                  [
                    Not
                      (ModelQuery.ParameterConstraint.AnnotationConstraint
                         (NameConstraint (Equals "int")));
                  ];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 1; name = "y"; positional_only = false },
            source "Test" );
      ];
  assert_generated_annotations
    ~source:
      {|
      from typing import Annotated
      def foo(x: int, y: Annotated[str, "foo"]): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "foo"))];
        models =
          [
            Parameter
              {
                where =
                  [ModelQuery.ParameterConstraint.AnnotationConstraint IsAnnotatedTypeConstraint];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 1; name = "y"; positional_only = false },
            source "Test" );
      ];
  assert_generated_annotations
    ~source:{|
      def foo(x, y): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "foo"))];
        models =
          [
            Parameter
              {
                where = [ModelQuery.ParameterConstraint.IndexConstraint 0];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 0; name = "x"; positional_only = false },
            source "Test" );
      ];
  assert_generated_annotations
    ~source:{|
      def foo(x, y): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "foo"))];
        models =
          [
            Parameter
              {
                where = [ModelQuery.ParameterConstraint.IndexConstraint 1];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 1; name = "y"; positional_only = false },
            source "Test" );
      ];
  assert_generated_annotations
    ~source:{|
      def foo(x, y): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "foo"))];
        models = [Parameter { where = []; taint = [TaintAnnotation (source "Test")] }];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 0; name = "x"; positional_only = false },
            source "Test" );
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 1; name = "y"; positional_only = false },
            source "Test" );
      ];

  (* Annotated returns. *)
  assert_generated_annotations
    ~source:
      {|
       from typing import Annotated
       def foo(x: int, y: str) -> Annotated[int, "annotation"]: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [ReturnConstraint IsAnnotatedTypeConstraint];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:{|
       def foo(x: int, y: str) -> int: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [ReturnConstraint IsAnnotatedTypeConstraint];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
    ~source:{|
       def foo(x: int, y: str): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [ReturnConstraint IsAnnotatedTypeConstraint];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
    ~source:{|
       def foo(x: typing.Annotated[int, "annotation"], y: str): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint)];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:{|
       def foo(a, b: typing.Annotated[int, "annotation"], c: str): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint)];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  (* Return annotation extends. *)
  assert_generated_annotations
    ~source:
      {|
      class A:
        pass

      class B(A):
        pass

      class C(B):
        pass

      class Test:
        def test1() -> A: ...
        def test2() -> B: ...
        def test3() -> C: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "test";
        logging_group_name = None;
        path = None;
        where =
          [
            ReturnConstraint
              (AnnotationClassExtends
                 { class_name = "test.A"; is_transitive = true; includes_self = false });
          ];
        models = [Return [TaintAnnotation (sink "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.Test"; method_name = "test1"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
    ~source:
      {|
      class A:
        pass

      class B(A):
        pass

      class C(B):
        pass

      class Test:
        def test1() -> A: ...
        def test2() -> B: ...
        def test3() -> C: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "test";
        logging_group_name = None;
        path = None;
        where =
          [
            ReturnConstraint
              (AnnotationClassExtends
                 { class_name = "test.A"; is_transitive = true; includes_self = false });
          ];
        models = [Return [TaintAnnotation (sink "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.Test"; method_name = "test2"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")];
  assert_generated_annotations
    ~source:
      {|
      class A:
        pass

      class B(A):
        pass

      class C(B):
        pass

      class Test:
        def test1() -> A: ...
        def test2() -> B: ...
        def test3() -> C: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "test";
        logging_group_name = None;
        path = None;
        where =
          [
            ReturnConstraint
              (AnnotationClassExtends
                 { class_name = "test.A"; is_transitive = true; includes_self = false });
          ];
        models = [Return [TaintAnnotation (sink "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.Test"; method_name = "test3"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")];
  assert_generated_annotations
    ~source:
      {|
      class A:
        pass

      class B(A):
        pass

      class C(B):
        pass

      def test1() -> typing.Optional[A]: ...
      def test2() -> B: ...
      def test3() -> typing.Optional[C]: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "test";
        logging_group_name = None;
        path = None;
        where =
          [
            ReturnConstraint
              (AnnotationClassExtends
                 { class_name = "test.A"; is_transitive = true; includes_self = true });
          ];
        models = [Return [TaintAnnotation (sink "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.test1"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")];
  assert_generated_annotations
    ~source:
      {|
      class A:
        pass

      class B(A):
        pass

      class C(B):
        pass

      def test1() -> typing.Optional[A]: ...
      def test2() -> B: ...
      def test3() -> typing.Optional[C]: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "test";
        logging_group_name = None;
        path = None;
        where =
          [
            ReturnConstraint
              (AnnotationClassExtends
                 { class_name = "test.A"; is_transitive = true; includes_self = true });
          ];
        models = [Return [TaintAnnotation (sink "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.test2"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")];
  assert_generated_annotations
    ~source:
      {|
      class A:
        pass

      class B(A):
        pass

      class C(B):
        pass

      def test1() -> typing.Optional[A]: ...
      def test2() -> B: ...
      def test3() -> typing.Optional[C]: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "test";
        logging_group_name = None;
        path = None;
        where =
          [
            ReturnConstraint
              (AnnotationClassExtends
                 { class_name = "test.A"; is_transitive = true; includes_self = true });
          ];
        models = [Return [TaintAnnotation (sink "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.test3"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")];
  assert_generated_annotations
    ~source:
      {|
      from typing import Union

      class A:
        pass

      class B:
        pass

      def test1() -> Union[A, B]: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "test";
        logging_group_name = None;
        path = None;
        where =
          [
            ReturnConstraint
              (AnnotationClassExtends
                 { class_name = "test.A"; is_transitive = false; includes_self = true });
          ];
        models = [Return [TaintAnnotation (sink "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.test1"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
    ~source:{|
      from typing import List

      def test1() -> List[int]: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "test";
        logging_group_name = None;
        path = None;
        where =
          [
            ReturnConstraint
              (AnnotationClassExtends
                 { class_name = "list"; is_transitive = false; includes_self = true });
          ];
        models = [Return [TaintAnnotation (sink "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.test1"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")];
  assert_generated_annotations
    ~source:{|
      from typing import List

      def test1() -> List: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "test";
        logging_group_name = None;
        path = None;
        where =
          [
            ReturnConstraint
              (AnnotationClassExtends
                 { class_name = "list"; is_transitive = false; includes_self = true });
          ];
        models = [Return [TaintAnnotation (sink "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.test1"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")];
  assert_generated_annotations
    ~source:
      {|
      from typing import List

      class MyList(List):
        pass

      def test1() -> MyList: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "test";
        logging_group_name = None;
        path = None;
        where =
          [
            ReturnConstraint
              (AnnotationClassExtends
                 { class_name = "list"; is_transitive = false; includes_self = true });
          ];
        models = [Return [TaintAnnotation (sink "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.test1"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")];
  assert_generated_annotations
    ~source:{|
      from typing import Tuple

      def test1() -> Tuple[int, str]: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "test";
        logging_group_name = None;
        path = None;
        where =
          [
            ReturnConstraint
              (AnnotationClassExtends
                 { class_name = "tuple"; is_transitive = false; includes_self = true });
          ];
        models = [Return [TaintAnnotation (sink "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.test1"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")];
  assert_generated_annotations
    ~source:
      {|
      from typing import Tuple

      class MyTuple(Tuple):
        pass

      def test1() -> MyTuple: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "test";
        logging_group_name = None;
        path = None;
        where =
          [
            ReturnConstraint
              (AnnotationClassExtends
                 { class_name = "tuple"; is_transitive = false; includes_self = true });
          ];
        models = [Return [TaintAnnotation (sink "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.test1"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")];
  assert_generated_annotations
    ~source:
      {|
      from pyre_extensions import ReadOnly
      from typing import Optional

      class A:
        pass

      class B(A):
        pass

      class C(B):
        pass

      def test1() -> ReadOnly[Optional[ReadOnly[C]]]: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "test";
        logging_group_name = None;
        path = None;
        where =
          [
            ReturnConstraint
              (AnnotationClassExtends
                 { class_name = "test.A"; is_transitive = true; includes_self = true });
          ];
        models = [Return [TaintAnnotation (sink "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.test1"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")];
  assert_generated_annotations
    ~source:{|
      class A:
        pass

      def test1(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "test";
        logging_group_name = None;
        path = None;
        where =
          [
            ReturnConstraint
              (AnnotationClassExtends
                 { class_name = "test.A"; is_transitive = true; includes_self = true });
          ];
        models = [Return [TaintAnnotation (sink "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.test1"; kind = Normal })
    ~expected:[];
  (* Any of. *)
  assert_generated_annotations
    ~source:{|
       def foo(a, b: typing.Annotated[int, "annotation"], c: str): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            AnyOf
              [
                AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint);
                ReturnConstraint IsAnnotatedTypeConstraint;
              ];
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:{|
       def foo(a, b, c: str) -> typing.Annotated[int, "annotation"]: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            AnyOf
              [
                AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint);
                ReturnConstraint IsAnnotatedTypeConstraint;
              ];
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:{|
       def foo(a, b: typing.Annotated[int, DynamicSource(A)], c: str): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint)];
        models =
          [
            PositionalParameter
              {
                index = 1;
                taint =
                  [
                    ParametricSourceFromAnnotation
                      { source_pattern = "DynamicSource"; kind = "Dynamic" };
                  ];
              };
          ];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 1; name = "b"; positional_only = false },
            source ~subkind:"A" "Dynamic" );
      ];
  (* Case where we don't match. *)
  assert_generated_annotations
    ~source:{|
       def foo(a, b: typing.Annotated[int, DynamicSource(A)], c: str): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint)];
        models =
          [
            PositionalParameter
              {
                index = 0;
                taint =
                  [
                    ParametricSourceFromAnnotation
                      { source_pattern = "DynamicSource"; kind = "Dynamic" };
                  ];
              };
          ];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[];
  (* All of. *)
  assert_generated_annotations
    ~source:
      {|
       def foo(a: typing.Annotated[int, "annotation"])-> typing.Annotated[int, "annotation"]: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            AllOf
              [
                AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint);
                ReturnConstraint IsAnnotatedTypeConstraint;
              ];
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  (* Some cases where we don't match with "AllOf". *)
  assert_generated_annotations
    ~source:{|
       def foo(a: typing.Annotated[int, "annotation"]): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            AllOf
              [
                AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint);
                ReturnConstraint IsAnnotatedTypeConstraint;
              ];
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
    ~source:{|
       def foo(a) -> typing.Annotated[int, "annotation"]: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            AllOf
              [
                AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint);
                ReturnConstraint IsAnnotatedTypeConstraint;
              ];
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[];
  (* Named parameters + parametric sources from annotation. *)
  assert_generated_annotations
    ~source:{|
       def foo(a, b: typing.Annotated[int, DynamicSource(A)], c: str): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint)];
        models =
          [
            NamedParameter
              {
                name = "b";
                taint =
                  [
                    ParametricSourceFromAnnotation
                      { source_pattern = "DynamicSource"; kind = "Dynamic" };
                  ];
              };
          ];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 1; name = "b"; positional_only = false },
            source ~subkind:"A" "Dynamic" );
      ];
  (* All parameters taint + parametric source from annotation. *)
  assert_generated_annotations
    ~source:{|
       def foo(a, b: typing.Annotated[int, DynamicSource(A)], c: str): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint)];
        models =
          [
            AllParameters
              {
                excludes = [];
                taint =
                  [
                    ParametricSourceFromAnnotation
                      { source_pattern = "DynamicSource"; kind = "Dynamic" };
                  ];
              };
          ];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 1; name = "b"; positional_only = false },
            source ~subkind:"A" "Dynamic" );
      ];
  (* Returned taint + parametric source from annotation. *)
  assert_generated_annotations
    ~source:{|
       def foo(a) -> typing.Annotated[int, DynamicSource(B)]: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [ReturnConstraint IsAnnotatedTypeConstraint];
        models =
          [
            Return
              [
                ParametricSourceFromAnnotation { source_pattern = "DynamicSource"; kind = "Dynamic" };
              ];
          ];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source ~subkind:"B" "Dynamic")];
  (* Named parameters + parametric sinks from annotation. *)
  assert_generated_annotations
    ~source:{|
       def foo(a, b: typing.Annotated[int, DynamicSink(BSink)], c: str): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint)];
        models =
          [
            NamedParameter
              {
                name = "b";
                taint =
                  [ParametricSinkFromAnnotation { sink_pattern = "DynamicSink"; kind = "Dynamic" }];
              };
          ];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 1; name = "b"; positional_only = false },
            ModelParseResult.TaintAnnotation.from_sink
              (Sinks.ParametricSink { sink_name = "Dynamic"; subkind = "BSink" }) );
      ];
  (* Type annotation constraint for callables *)
  assert_generated_annotations
    ~source:{|
       def foo(a, b: typing.Annotated[str, "foo"], c: str, d: int): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyParameterConstraint (AnnotationConstraint IsAnnotatedTypeConstraint)];
        models =
          [
            Parameter
              {
                where = [AnnotationConstraint IsAnnotatedTypeConstraint];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 1; name = "b"; positional_only = false },
            source "Test" );
      ];
  assert_generated_annotations
    ~source:{|
       def foo(a, b: typing.Annotated[str, "foo"], c: str, d: int): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            AnyParameterConstraint
              (AnnotationConstraint (NameConstraint (Matches (Re2.create_exn "str"))));
          ];
        models =
          [
            Parameter
              {
                where = [AnnotationConstraint (NameConstraint (Matches (Re2.create_exn "str")))];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 1; name = "b"; positional_only = false },
            source "Test" );
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 2; name = "c"; positional_only = false },
            source "Test" );
      ];
  assert_generated_annotations
    ~source:{|
       def foo(a, b: typing.Annotated[str, "foo"], c: str, d: int): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyParameterConstraint (AnnotationConstraint (NameConstraint (Equals "int")))];
        models =
          [
            Parameter
              {
                where = [AnnotationConstraint (NameConstraint (Equals "int"))];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          ( AccessPath.Root.PositionalParameter { position = 3; name = "d"; positional_only = false },
            source "Test" );
      ];
  assert_generated_annotations
    ~source:{|
       def foo() -> int: ...
       def bar() -> str: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [ReturnConstraint (NameConstraint (Equals "int"))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:{|
       def foo() -> int: ...
       def bar() -> str: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [ReturnConstraint (NameConstraint (Equals "int"))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.bar"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
    ~source:{|
       def foo() -> str: ...
       def bar() -> List[str]: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [ReturnConstraint (NameConstraint (Matches (Re2.create_exn "str")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:{|
       def foo() -> str: ...
       def bar() -> typing.List[str]: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [ReturnConstraint (NameConstraint (Matches (Re2.create_exn "str")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.bar"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:{|
       def foo() -> typing.Annotated[str, "foo"]: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [ReturnConstraint IsAnnotatedTypeConstraint];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:{|
       def foo() -> typing.Annotated[str, "foo"]: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [ReturnConstraint (NameConstraint (Matches (Re2.create_exn "foo")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];

  (* Decorator names. *)
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [AnyDecoratorConstraint (FullyQualifiedNameConstraint (Matches (Re2.create_exn "d1")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyDecoratorConstraint (NameConstraint (Matches (Re2.create_exn "d1")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [AnyDecoratorConstraint (FullyQualifiedNameConstraint (Matches (Re2.create_exn "d1")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.bar"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [AnyDecoratorConstraint (FullyQualifiedNameConstraint (Matches (Re2.create_exn "d1")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.baz"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:
      {|
       from flask import Flask
       app = Flask(__name__)
       @app.route('/')
       def foo(a): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            AnyDecoratorConstraint
              (FullyQualifiedNameConstraint (Matches (Re2.create_exn "app.route")));
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:
      {|
       from flask import Flask
       app = Flask(__name__)
       @app.route('/')
       def foo(a): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyDecoratorConstraint (NameConstraint (Matches (Re2.create_exn "app.route")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [AnyDecoratorConstraint (FullyQualifiedNameConstraint (Matches (Re2.create_exn "d1")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.baz"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyDecoratorConstraint (FullyQualifiedNameConstraint (Equals "test.d1"))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.baz"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            AnyDecoratorConstraint
              (AllOf
                 [
                   FullyQualifiedNameConstraint (Equals "test.d1");
                   ArgumentsConstraint
                     (ModelQuery.ArgumentsConstraint.Contains
                        [
                          {
                            Ast.Expression.Call.Argument.name = None;
                            value = +Ast.Expression.(Expression.Constant (Constant.Integer 1));
                          };
                        ]);
                 ]);
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.baz"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            AnyDecoratorConstraint
              (AllOf
                 [
                   FullyQualifiedNameConstraint (Equals "test.d1");
                   ArgumentsConstraint
                     (Contains
                        [
                          {
                            Ast.Expression.Call.Argument.name = None;
                            value = +Ast.Expression.(Expression.Constant (Constant.Integer 1));
                          };
                        ]);
                 ]);
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.baz"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            AnyDecoratorConstraint
              (AllOf
                 [
                   FullyQualifiedNameConstraint (Equals "test.d1");
                   ArgumentsConstraint
                     (Contains
                        [
                          {
                            Ast.Expression.Call.Argument.name =
                              Some (Ast.Node.create_with_default_location "arg1");
                            value = +Ast.Expression.(Expression.Constant (Constant.Integer 1));
                          };
                        ]);
                 ]);
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.baz"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            AnyDecoratorConstraint
              (AllOf
                 [
                   FullyQualifiedNameConstraint (Equals "test.d1");
                   ArgumentsConstraint
                     (Contains
                        [
                          {
                            Ast.Expression.Call.Argument.name =
                              Some (Ast.Node.create_with_default_location "arg1");
                            value = +Ast.Expression.(Expression.Constant (Constant.Integer 1));
                          };
                        ]);
                 ]);
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.baz"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            AnyDecoratorConstraint
              (AllOf
                 [
                   FullyQualifiedNameConstraint (Equals "test.d1");
                   ArgumentsConstraint
                     (Contains
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
                 ]);
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.baz"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            AnyDecoratorConstraint
              (AllOf
                 [
                   FullyQualifiedNameConstraint (Equals "test.d1");
                   ArgumentsConstraint
                     (Equals
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
                 ]);
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            AnyDecoratorConstraint
              (AllOf
                 [
                   FullyQualifiedNameConstraint (Equals "test.d1");
                   ArgumentsConstraint
                     (Equals
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
                 ]);
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.baz"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];

  assert_generated_annotations
    ~source:
      {|
      class C:
        def foo(): ...
      class D:
        def foo(): ...
      class DC:
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [ClassConstraint (NameConstraint (Matches (Re2.create_exn "C")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:
      {|
      class C:
        def foo(): ...
      class D:
        def foo(): ...
      class DC:
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [ClassConstraint (NameConstraint (Matches (Re2.create_exn "C")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.D"; method_name = "foo"; kind = Normal })
    ~expected:[];

  assert_generated_annotations
    ~source:
      {|
      class C:
        def foo(): ...
      class D:
        def foo(): ...
      class DC:
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [ClassConstraint (NameConstraint (Matches (Re2.create_exn "C")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.DC"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [ClassConstraint (DecoratorConstraint (NameConstraint (Matches (Re2.create_exn "d2"))))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [ClassConstraint (DecoratorConstraint (NameConstraint (Matches (Re2.create_exn "4"))))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (DecoratorConstraint
                 (AllOf
                    [
                      NameConstraint (Equals "test.d1");
                      ArgumentsConstraint
                        (Contains
                           [
                             {
                               Ast.Expression.Call.Argument.name = None;
                               value = +Ast.Expression.(Expression.Constant (Constant.Integer 1));
                             };
                           ]);
                    ]));
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (DecoratorConstraint
                 (AllOf
                    [
                      NameConstraint (Matches (Re2.create_exn "d1"));
                      ArgumentsConstraint
                        (Contains
                           [
                             {
                               Ast.Expression.Call.Argument.name = None;
                               value = +Ast.Expression.(Expression.Constant (Constant.Integer 1));
                             };
                           ]);
                    ]));
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];

  (* Test attribute models. *)
  assert_generated_annotations_for_attributes
    ~source:{|
      class C:
        x: ...
      class D(C):
        y: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [ClassConstraint (NameConstraint (Matches (Re2.create_exn "C")))];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.C.x"
    ~expected:[source "Test"];
  assert_generated_annotations_for_attributes
    ~source:{|
      class C:
        x: ...
      class D(C):
        y: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [ClassConstraint (NameConstraint (Matches (Re2.create_exn "C")))];
        models = [Attribute [TaintAnnotation (sink "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.C.x"
    ~expected:[sink "Test"];
  assert_generated_annotations_for_attributes
    ~source:{|
      class C:
        x: ...
      class D(C):
        y: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [ClassConstraint (NameConstraint (Matches (Re2.create_exn "C")))];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.D.y"
    ~expected:[];
  assert_generated_annotations_for_attributes
    ~source:{|
      class C:
        x: ...
      class D(C):
        y: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (Extends { class_name = "test.C"; is_transitive = false; includes_self = true });
          ];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.C.x"
    ~expected:[source "Test"];
  ();
  assert_generated_annotations_for_attributes
    ~source:{|
      class C:
        x: ...
      class D(C):
        y: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (Extends { class_name = "test.C"; is_transitive = false; includes_self = true });
          ];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.D.y"
    ~expected:[source "Test"];
  assert_generated_annotations_for_attributes
    ~source:
      {|
      class C:
        x: ...
      class D(C):
        y: ...
      class E:
        z: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (Extends { class_name = "test.C"; is_transitive = false; includes_self = true });
          ];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.E.z"
    ~expected:[];
  assert_generated_annotations_for_attributes
    ~source:{|
      class C:
        x: int
        y: str
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnnotationConstraint (NameConstraint (Equals "int"))];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.C.x"
    ~expected:[source "Test"];
  assert_generated_annotations_for_attributes
    ~source:{|
      class C:
        x: int
        y: str
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnnotationConstraint (NameConstraint (Equals "int"))];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.C.y"
    ~expected:[];
  assert_generated_annotations_for_attributes
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnnotationConstraint (NameConstraint (Matches (Re2.create_exn "Foo")))];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.C.x"
    ~expected:[source "Test"];
  assert_generated_annotations_for_attributes
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnnotationConstraint (NameConstraint (Matches (Re2.create_exn "Foo")))];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.C.y"
    ~expected:[source "Test"];
  assert_generated_annotations_for_attributes
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnnotationConstraint (NameConstraint (Matches (Re2.create_exn "Foo")))];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.C.z"
    ~expected:[];
  assert_generated_annotations_for_attributes
    ~source:
      {|
      from typing import Annotated
      class C:
        x: int
        y: Annotated[str, "foo"]
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnnotationConstraint IsAnnotatedTypeConstraint];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.C.x"
    ~expected:[];
  assert_generated_annotations_for_attributes
    ~source:
      {|
      from typing import Annotated
      class C:
        x: int
        y: Annotated[str, "foo"]
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnnotationConstraint IsAnnotatedTypeConstraint];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.C.y"
    ~expected:[source "Test"];

  (* Test 'Not' clause *)
  assert_generated_annotations
    ~source:{|
      def foo(): ...
      def barfoo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            NameConstraint (Matches (Re2.create_exn "foo"));
            Not (NameConstraint (Matches (Re2.create_exn "bar")));
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:{|
      def foo(): ...
      def barfoo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            NameConstraint (Matches (Re2.create_exn "foo"));
            Not (NameConstraint (Matches (Re2.create_exn "bar")));
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.barfoo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
    ~source:
      {|
       def foo(a) -> typing.Annotated[int, DynamicSource(B)]: ...
       def bar(b): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [Not (ReturnConstraint IsAnnotatedTypeConstraint)];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
    ~source:
      {|
      class C:
        def foo(): ...
      class D:
        def foo(): ...
      class DC:
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint (NameConstraint (Matches (Re2.create_exn "C")));
            Not (ClassConstraint (NameConstraint (Matches (Re2.create_exn "D"))));
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:
      {|
      class C:
        def foo(): ...
      class D:
        def foo(): ...
      class DC:
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint (NameConstraint (Matches (Re2.create_exn "C")));
            Not (ClassConstraint (NameConstraint (Matches (Re2.create_exn "D"))));
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.DC"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
    ~source:
      {|
       def foo(a) -> typing.Annotated[int, DynamicSource(B)]: ...
       def bar(b): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [Not (ReturnConstraint IsAnnotatedTypeConstraint)];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Function { name = "test.bar"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations_for_attributes
    ~source:
      {|
      class C:
        x: ...
      class D(C):
        y: ...
      class E:
        z: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            Not
              (ClassConstraint
                 (Extends { class_name = "test.C"; is_transitive = false; includes_self = true }));
          ];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.C.x"
    ~expected:[];
  assert_generated_annotations_for_attributes
    ~source:
      {|
      class C:
        x: ...
      class D(C):
        y: ...
      class E:
        z: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            Not
              (ClassConstraint
                 (Extends { class_name = "test.C"; is_transitive = false; includes_self = true }));
          ];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.D.y"
    ~expected:[];
  assert_generated_annotations_for_attributes
    ~source:
      {|
      class C:
        x: ...
      class D(C):
        y: ...
      class E:
        z: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            Not
              (ClassConstraint
                 (Extends { class_name = "test.C"; is_transitive = false; includes_self = true }));
          ];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.E.z"
    ~expected:[source "Test"];

  (* Test transitive extends *)
  assert_generated_annotations_for_attributes
    ~source:
      {|
      class C:
        x: ...
      class D(C):
        y: ...
      class E(D):
        z: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (Extends { class_name = "test.C"; is_transitive = true; includes_self = true });
          ];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.E.z"
    ~expected:[source "Test"];
  assert_generated_annotations_for_attributes
    ~source:
      {|
      class C:
        x: ...
      class D(C):
        y: ...
      class E(D):
        z: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (Extends { class_name = "test.C"; is_transitive = true; includes_self = true });
          ];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.D.y"
    ~expected:[source "Test"];
  assert_generated_annotations_for_attributes
    ~source:
      {|
      class C:
        x: ...
      class D(C):
        y: ...
      class E(D):
        z: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (Extends { class_name = "test.C"; is_transitive = true; includes_self = true });
          ];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.C.x"
    ~expected:[source "Test"];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            Not
              (ClassConstraint
                 (Extends { class_name = "test.A"; is_transitive = true; includes_self = true }));
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            Not
              (ClassConstraint
                 (Extends { class_name = "test.A"; is_transitive = true; includes_self = true }));
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            Not
              (ClassConstraint
                 (Extends { class_name = "test.A"; is_transitive = true; includes_self = true }));
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            Not
              (ClassConstraint
                 (Extends { class_name = "test.A"; is_transitive = true; includes_self = true }));
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.D"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];

  (* Test includes_self=False *)
  assert_generated_annotations_for_attributes
    ~source:
      {|
      class C:
        x: ...
      class D(C):
        y: ...
      class E(D):
        z: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (Extends { class_name = "test.C"; is_transitive = false; includes_self = false });
          ];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.C.x"
    ~expected:[];
  assert_generated_annotations_for_attributes
    ~source:
      {|
      class C:
        x: ...
      class D(C):
        y: ...
      class E(D):
        z: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (Extends { class_name = "test.C"; is_transitive = false; includes_self = false });
          ];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.D.y"
    ~expected:[source "Test"];
  assert_generated_annotations_for_attributes
    ~source:
      {|
      class C:
        x: ...
      class D(C):
        y: ...
      class E(D):
        z: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (Extends { class_name = "test.C"; is_transitive = false; includes_self = false });
          ];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.E.z"
    ~expected:[];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (Extends { class_name = "test.A"; is_transitive = true; includes_self = false });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (Extends { class_name = "test.A"; is_transitive = true; includes_self = false });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (Extends { class_name = "test.A"; is_transitive = true; includes_self = false });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (Extends { class_name = "test.B"; is_transitive = true; includes_self = false });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (Extends { class_name = "test.B"; is_transitive = true; includes_self = false });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
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
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (Extends { class_name = "test.B"; is_transitive = true; includes_self = false });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];

  (* Test cls.any_child *)
  assert_generated_annotations
    ~source:
      {|
      @decorator
      class A:
        def foo(): ...
      @decorator
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D:
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (AnyChildConstraint
                 {
                   class_constraint = DecoratorConstraint (NameConstraint (Equals "decorator"));
                   is_transitive = false;
                   includes_self = true;
                 });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:
      {|
      @decorator
      class A:
        def foo(): ...
      @decorator
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D:
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (AnyChildConstraint
                 {
                   class_constraint = DecoratorConstraint (NameConstraint (Equals "decorator"));
                   is_transitive = false;
                   includes_self = true;
                 });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:
      {|
      @decorator
      class A:
        def foo(): ...
      @decorator
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D:
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (AnyChildConstraint
                 {
                   class_constraint = DecoratorConstraint (NameConstraint (Equals "decorator"));
                   is_transitive = false;
                   includes_self = true;
                 });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
    ~source:
      {|
      @decorator
      class A:
        def foo(): ...
      @decorator
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D:
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (AnyChildConstraint
                 {
                   class_constraint = DecoratorConstraint (NameConstraint (Equals "decorator"));
                   is_transitive = false;
                   includes_self = false;
                 });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:
      {|
      @decorator
      class A:
        def foo(): ...
      @decorator
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D:
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (AnyChildConstraint
                 {
                   class_constraint = DecoratorConstraint (NameConstraint (Equals "decorator"));
                   is_transitive = false;
                   includes_self = false;
                 });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
    ~source:
      {|
      @decorator
      class A:
        def foo(): ...
      @decorator
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D:
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (AnyChildConstraint
                 {
                   class_constraint = DecoratorConstraint (NameConstraint (Equals "decorator"));
                   is_transitive = false;
                   includes_self = false;
                 });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
    ~source:
      {|
      @decorator
      class A:
        def foo(): ...
      @decorator
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D:
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (AnyChildConstraint
                 {
                   class_constraint = DecoratorConstraint (NameConstraint (Equals "decorator"));
                   is_transitive = true;
                   includes_self = false;
                 });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:
      {|
      @decorator
      class A:
        def foo(): ...
      @decorator
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D:
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (AnyChildConstraint
                 {
                   class_constraint = DecoratorConstraint (NameConstraint (Equals "decorator"));
                   is_transitive = true;
                   includes_self = false;
                 });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
    ~source:
      {|
      @decorator
      class A:
        def foo(): ...
      @decorator
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D:
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (AnyChildConstraint
                 {
                   class_constraint = DecoratorConstraint (NameConstraint (Equals "decorator"));
                   is_transitive = true;
                   includes_self = false;
                 });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
    ~source:
      {|
      @decorator
      class A:
        def foo(): ...
      class B(A):
        def foo(): ...
      @decorator
      class C(B):
        def foo(): ...
      class D:
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (AnyChildConstraint
                 {
                   class_constraint = DecoratorConstraint (NameConstraint (Equals "decorator"));
                   is_transitive = true;
                   includes_self = false;
                 });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];

  (* Test cls.any_parent *)
  assert_generated_annotations
    ~source:
      {|
      class A:
        def foo(): ...
      @decorator
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D(C):
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (AnyParentConstraint
                 {
                   class_constraint = DecoratorConstraint (NameConstraint (Equals "decorator"));
                   is_transitive = false;
                   includes_self = true;
                 });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
    ~source:
      {|
      class A:
        def foo(): ...
      @decorator
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D(C):
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (AnyParentConstraint
                 {
                   class_constraint = DecoratorConstraint (NameConstraint (Equals "decorator"));
                   is_transitive = false;
                   includes_self = true;
                 });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:
      {|
      class A:
        def foo(): ...
      @decorator
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D(C):
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (AnyParentConstraint
                 {
                   class_constraint = DecoratorConstraint (NameConstraint (Equals "decorator"));
                   is_transitive = false;
                   includes_self = true;
                 });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:
      {|
      class A:
        def foo(): ...
      @decorator
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D(C):
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (AnyParentConstraint
                 {
                   class_constraint = DecoratorConstraint (NameConstraint (Equals "decorator"));
                   is_transitive = false;
                   includes_self = false;
                 });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
    ~source:
      {|
      class A:
        def foo(): ...
      @decorator
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D(C):
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (AnyParentConstraint
                 {
                   class_constraint = DecoratorConstraint (NameConstraint (Equals "decorator"));
                   is_transitive = false;
                   includes_self = false;
                 });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
    ~source:
      {|
      class A:
        def foo(): ...
      @decorator
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D(C):
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (AnyParentConstraint
                 {
                   class_constraint = DecoratorConstraint (NameConstraint (Equals "decorator"));
                   is_transitive = false;
                   includes_self = false;
                 });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:
      {|
      class A:
        def foo(): ...
      @decorator
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D(C):
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (AnyParentConstraint
                 {
                   class_constraint = DecoratorConstraint (NameConstraint (Equals "decorator"));
                   is_transitive = true;
                   includes_self = false;
                 });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
    ~source:
      {|
      class A:
        def foo(): ...
      @decorator
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D(C):
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (AnyParentConstraint
                 {
                   class_constraint = DecoratorConstraint (NameConstraint (Equals "decorator"));
                   is_transitive = true;
                   includes_self = false;
                 });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[];
  assert_generated_annotations
    ~source:
      {|
      class A:
        def foo(): ...
      @decorator
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D(C):
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (AnyParentConstraint
                 {
                   class_constraint = DecoratorConstraint (NameConstraint (Equals "decorator"));
                   is_transitive = true;
                   includes_self = false;
                 });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];
  assert_generated_annotations
    ~source:
      {|
      class A:
        def foo(): ...
      @decorator
      class B(A):
        def foo(): ...
      class C(B):
        def foo(): ...
      class D(C):
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            ClassConstraint
              (AnyParentConstraint
                 {
                   class_constraint = DecoratorConstraint (NameConstraint (Equals "decorator"));
                   is_transitive = true;
                   includes_self = false;
                 });
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Method { class_name = "test.D"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")];

  assert_generated_annotations_for_globals
    ~source:{|
      foo = []
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "foo"))];
        models = [Global [TaintAnnotation (source "Test")]];
        find = Global;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.foo"
    ~expected:[source "Test"];
  assert_generated_annotations_for_globals
    ~source:{|
      foo, bar = [], {}
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_bar";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "bar"))];
        models = [Global [TaintAnnotation (source "Test")]];
        find = Global;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.bar"
    ~expected:[source "Test"];
  assert_generated_annotations_for_globals
    ~source:{|
      foo = []
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_baz";
        logging_group_name = None;
        path = None;
        where = [NameConstraint (Matches (Re2.create_exn "baz"))];
        models = [Global [TaintAnnotation (source "Test")]];
        find = Global;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.foo"
    ~expected:[];
  ()


let test_partition_cache_queries _ =
  let assert_partition ~queries ~expected () =
    let partition = ModelQueryExecution.PartitionCacheQueries.partition queries in
    assert_equal
      ~cmp:ModelQueryExecution.PartitionCacheQueries.equal
      ~printer:ModelQueryExecution.PartitionCacheQueries.show
      expected
      partition
  in
  let empty_query =
    {
      ModelQuery.location = { start = { line = 0; column = 0 }; stop = { line = 0; column = 0 } };
      name = "empty";
      logging_group_name = None;
      path = None;
      where = [];
      find = Method;
      models = [];
      expected_models = [];
      unexpected_models = [];
    }
  in
  let read_from_cache =
    {
      empty_query with
      name = "read_from_cache";
      where = [ReadFromCache { kind = "thrift"; name = "cache:name" }];
      models =
        [
          Return
            [
              TaintAnnotation
                (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
            ];
        ];
    }
  in
  let write_to_cache =
    {
      empty_query with
      name = "write_to_cache";
      where = [NameConstraint (Matches (Re2.create_exn "foo"))];
      models =
        [
          WriteToCache
            {
              ModelQuery.WriteToCache.kind = "thrift";
              name =
                [
                  ModelQuery.WriteToCache.Substring.ClassName;
                  ModelQuery.WriteToCache.Substring.Literal ":";
                  ModelQuery.WriteToCache.Substring.MethodName;
                ];
            };
        ];
    }
  in
  let regular =
    {
      empty_query with
      name = "regular";
      where = [NameConstraint (Matches (Re2.create_exn "foo"))];
      models =
        [
          Return
            [
              TaintAnnotation
                (ModelParseResult.TaintAnnotation.from_source (Sources.NamedSource "Test"));
            ];
        ];
    }
  in
  assert_partition
    ~queries:[regular; read_from_cache; write_to_cache]
    ~expected:
      {
        ModelQueryExecution.PartitionCacheQueries.write_to_cache = [write_to_cache];
        read_from_cache = [read_from_cache];
        others = [regular];
      }
    ();
  ()


let test_generated_cache context =
  let assert_generated_cache ~source ~queries ~callables ~expected =
    let { ScratchProject.BuiltTypeEnvironment.type_environment; _ } =
      ScratchProject.setup ~context ["test.py", source] |> ScratchProject.build_type_environment
    in
    let global_resolution = Analysis.TypeEnvironment.ReadOnly.global_resolution type_environment in
    let class_hierarchy_graph =
      ClassHierarchyGraph.Heap.from_qualifiers
        ~scheduler:(mock_scheduler ())
        ~environment:type_environment
        ~qualifiers:[Ast.Reference.create "test"]
      |> ClassHierarchyGraph.SharedMemory.from_heap ~store_transitive_children_for:[]
    in
    let actual =
      ModelQueryExecution.CallableQueryExecutor.generate_cache_from_queries_on_targets
        ~verbose:false
        ~resolution:global_resolution
        ~class_hierarchy_graph
        ~targets:callables
        queries
    in
    let expected =
      List.fold
        ~init:ModelQueryExecution.ReadWriteCache.empty
        ~f:(fun cache (kind, name, target) ->
          ModelQueryExecution.ReadWriteCache.write cache ~kind ~name ~target)
        expected
    in
    assert_equal
      ~cmp:ModelQueryExecution.ReadWriteCache.equal
      ~printer:ModelQueryExecution.ReadWriteCache.show
      expected
      actual
  in
  assert_generated_cache
    ~source:{|
      def foo(): ...
      def no_match(): ...
      |}
    ~queries:
      [
        {
          location = Ast.Location.any;
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          models =
            [
              WriteToCache
                {
                  ModelQuery.WriteToCache.kind = "thrift";
                  name = [ModelQuery.WriteToCache.Substring.FunctionName];
                };
            ];
          find = Function;
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ~callables:
      [
        Target.Function { name = "test.foo"; kind = Normal };
        Target.Function { name = "test.no_match"; kind = Normal };
      ]
    ~expected:["thrift", "foo", Target.Function { name = "test.foo"; kind = Normal }];
  assert_generated_cache
    ~source:
      {|
      class C:
        def foo(self): ...
      class D:
        def foo(self): ...
      |}
    ~queries:
      [
        {
          location = Ast.Location.any;
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          models =
            [
              WriteToCache
                {
                  ModelQuery.WriteToCache.kind = "thrift";
                  name =
                    [
                      ModelQuery.WriteToCache.Substring.ClassName;
                      ModelQuery.WriteToCache.Substring.Literal ":";
                      ModelQuery.WriteToCache.Substring.MethodName;
                    ];
                };
            ];
          find = Method;
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ~callables:
      [
        Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal };
        Target.Method { class_name = "test.D"; method_name = "foo"; kind = Normal };
      ]
    ~expected:
      [
        ( "thrift",
          "C:foo",
          Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal } );
        ( "thrift",
          "D:foo",
          Target.Method { class_name = "test.D"; method_name = "foo"; kind = Normal } );
      ];
  (* We can have multiple targets for the same kind+name *)
  assert_generated_cache
    ~source:
      {|
      class C:
        def foo(self): ...
      class D:
        def foo(self): ...
      |}
    ~queries:
      [
        {
          location = Ast.Location.any;
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [NameConstraint (Matches (Re2.create_exn "foo"))];
          models =
            [
              WriteToCache
                {
                  ModelQuery.WriteToCache.kind = "thrift";
                  name = [ModelQuery.WriteToCache.Substring.MethodName];
                };
            ];
          find = Method;
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ~callables:
      [
        Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal };
        Target.Method { class_name = "test.D"; method_name = "foo"; kind = Normal };
      ]
    ~expected:
      [
        "thrift", "foo", Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal };
        "thrift", "foo", Target.Method { class_name = "test.D"; method_name = "foo"; kind = Normal };
      ];
  (* Multiple WriteToCache in the same query. *)
  assert_generated_cache
    ~source:
      {|
      class C:
        def foo(self): ...
      class D:
        def foo(self): ...
      |}
    ~queries:
      [
        {
          location = Ast.Location.any;
          name = "get_foo";
          logging_group_name = None;
          path = None;
          where = [FullyQualifiedNameConstraint (Matches (Re2.create_exn "C.foo"))];
          models =
            [
              WriteToCache
                {
                  ModelQuery.WriteToCache.kind = "a";
                  name = [ModelQuery.WriteToCache.Substring.MethodName];
                };
              WriteToCache
                {
                  ModelQuery.WriteToCache.kind = "b";
                  name = [ModelQuery.WriteToCache.Substring.MethodName];
                };
            ];
          find = Method;
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ~callables:
      [
        Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal };
        Target.Method { class_name = "test.D"; method_name = "foo"; kind = Normal };
      ]
    ~expected:
      [
        "a", "foo", Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal };
        "b", "foo", Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal };
      ];
  ()


let test_read_from_cache_constraints _ =
  let assert_target_candidates ~cache ~constraints ~expected =
    let cache =
      List.fold
        ~init:ModelQueryExecution.ReadWriteCache.empty
        ~f:(fun cache (kind, name, target) ->
          ModelQueryExecution.ReadWriteCache.write cache ~kind ~name ~target)
        cache
    in
    let actual =
      ModelQueryExecution.CandidateTargetsFromCache.from_constraint cache (AllOf constraints)
    in
    assert_equal
      ~cmp:ModelQueryExecution.CandidateTargetsFromCache.equal
      ~printer:ModelQueryExecution.CandidateTargetsFromCache.show
      expected
      actual
  in
  let cache =
    [
      "thrift", "A:foo", Target.Method { class_name = "test.A"; method_name = "foo"; kind = Normal };
      "thrift", "B:foo", Target.Method { class_name = "test.B"; method_name = "foo"; kind = Normal };
      "thrift", "C:foo", Target.Method { class_name = "test.C"; method_name = "foo"; kind = Normal };
    ]
  in
  assert_target_candidates
    ~cache
    ~constraints:[ReadFromCache { kind = "thrift"; name = "A:foo" }]
    ~expected:
      (Set
         (Target.Set.of_list
            [Target.Method { class_name = "test.A"; method_name = "foo"; kind = Normal }]));
  assert_target_candidates
    ~cache
    ~constraints:[NameConstraint (Matches (Re2.create_exn "bar"))]
    ~expected:Top;
  assert_target_candidates
    ~cache
    ~constraints:
      [
        ReadFromCache { kind = "thrift"; name = "A:foo" };
        NameConstraint (Matches (Re2.create_exn "bar"));
      ]
    ~expected:
      (Set
         (Target.Set.of_list
            [Target.Method { class_name = "test.A"; method_name = "foo"; kind = Normal }]));
  assert_target_candidates
    ~cache
    ~constraints:
      [
        ReadFromCache { kind = "thrift"; name = "A:foo" };
        ReadFromCache { kind = "thrift"; name = "B:foo" };
      ]
    ~expected:(Set Target.Set.empty);
  assert_target_candidates
    ~cache
    ~constraints:
      [
        AnyOf
          [
            ReadFromCache { kind = "thrift"; name = "A:foo" };
            ReadFromCache { kind = "thrift"; name = "B:foo" };
          ];
      ]
    ~expected:
      (Set
         (Target.Set.of_list
            [
              Target.Method { class_name = "test.A"; method_name = "foo"; kind = Normal };
              Target.Method { class_name = "test.B"; method_name = "foo"; kind = Normal };
            ]));
  assert_target_candidates
    ~cache
    ~constraints:
      [
        AnyOf
          [
            ReadFromCache { kind = "thrift"; name = "A:foo" };
            ReadFromCache { kind = "thrift"; name = "B:foo" };
            NameConstraint (Matches (Re2.create_exn "bar"));
          ];
      ]
    ~expected:Top;
  assert_target_candidates
    ~cache
    ~constraints:
      [
        AllOf
          [
            AnyOf
              [
                ReadFromCache { kind = "thrift"; name = "A:foo" };
                ReadFromCache { kind = "thrift"; name = "B:foo" };
                ReadFromCache { kind = "thrift"; name = "C:foo" };
              ];
            AnyOf
              [
                ReadFromCache { kind = "thrift"; name = "A:foo" };
                ReadFromCache { kind = "thrift"; name = "B:foo" };
              ];
            AnyOf
              [
                ReadFromCache { kind = "thrift"; name = "A:foo" };
                ReadFromCache { kind = "thrift"; name = "C:foo" };
              ];
          ];
      ]
    ~expected:
      (Set
         (Target.Set.of_list
            [Target.Method { class_name = "test.A"; method_name = "foo"; kind = Normal }]));
  ()


let test_model_query_error context =
  let assert_json_error ~expected ~source ~queries () =
    let errors =
      try
        let _ =
          initialize
            ~models_source:queries
            ~context
            ~taint_configuration:TaintConfiguration.Heap.default
            ~model_path:(PyrePath.create_absolute "/a/b.pysa")
            source
        in
        []
      with
      | ModelVerificationError.ModelVerificationErrors errors -> errors
    in
    let errors = List.map ~f:ModelVerificationError.to_json errors |> fun errors -> `List errors in
    assert_equal
      ~printer:Yojson.Safe.pretty_to_string
      ~cmp:Yojson.Safe.equal
      (Yojson.Safe.from_string expected)
      errors
  in
  assert_json_error
    ~queries:
      {|
        ModelQuery(
          name = "invalid_model_query",
          find = "functions",
          where = Decorator(arguments.contains("1"), name.matches("d")),
          model = Returns(TaintSource[Test])
        )
      |}
    ~source:{|
        def foo(x):
            ...
      |}
    ~expected:
      {|
        [
          {
            "description": "Model Query `invalid_model_query` output no models.",
            "line": 2,
            "column": 0,
            "stop_line": 7,
            "stop_column": 1,
            "path": "/a/b.pysa",
            "code": 41
          }
        ]
      |}
    ();
  assert_json_error
    ~queries:
      {|
        ModelQuery(
          name = "invalid_model_query_1",
          find = "functions",
          where = Decorator(arguments.contains("1"), name.matches("d")),
          model = Returns(TaintSource[Test])
        )
        ModelQuery(
          name = "invalid_model_query_2",
          find = "functions",
          where = Decorator(arguments.contains("1"), name.matches("d")),
          model = Returns(TaintSource[Test])
        )
      |}
    ~source:{|
        def foo(x):
            ...
      |}
    ~expected:
      {|
        [
          {
            "description": "Model Query `invalid_model_query_1` output no models.",
            "line": 2,
            "column": 0,
            "stop_line": 7,
            "stop_column": 1,
            "path": "/a/b.pysa",
            "code": 41
          },
          {
            "description": "Model Query `invalid_model_query_2` output no models.",
            "line": 8,
            "column": 0,
            "stop_line": 13,
            "stop_column": 1,
            "path": "/a/b.pysa",
            "code": 41
          }
        ]
      |}
    ();
  assert_json_error
    ~queries:
      {|
        ModelQuery(
          name = "invalid_model_query_1",
          logging_group_name = "query_group",
          find = "functions",
          where = Decorator(arguments.contains("1"), name.matches("d")),
          model = Returns(TaintSource[Test])
        )
        ModelQuery(
          name = "invalid_model_query_2",
          logging_group_name = "query_group",
          find = "functions",
          where = Decorator(arguments.contains("1"), name.matches("d")),
          model = Returns(TaintSource[Test])
        )
      |}
    ~source:{|
        def foo(x):
            ...
      |}
    ~expected:
      {|
        [
          {
            "description": "Model Query group `query_group` output no models.",
            "line": 9,
            "column": 0,
            "stop_line": 15,
            "stop_column": 1,
            "path": "/a/b.pysa",
            "code": 67
          }
        ]
      |}
    ();
  (* No error on WriteToCache queries. *)
  assert_json_error
    ~queries:
      {|
        ModelQuery(
          name = "invalid_model_query",
          find = "functions",
          where = Decorator(arguments.contains("1"), name.matches("d")),
          model = WriteToCache(kind="cache", name=f"name")
        )
      |}
    ~source:{|
        def foo(x):
            ...
      |}
    ~expected:"[]"
    ();
  ()


let () =
  "modelQuery"
  >::: [
         "generated_annotations" >:: test_generated_annotations;
         "partition_cache_queries" >:: test_partition_cache_queries;
         "generated_cache" >:: test_generated_cache;
         "read_from_cache_constraints" >:: test_read_from_cache_constraints;
         "model_query_error" >:: test_model_query_error;
       ]
  |> Test.run
