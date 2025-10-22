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


let assert_generated_annotations
    context
    ?(skip_for_pyrefly = false)
    ?pyrefly_expected
    ~source
    ~query
    ~callable
    ~expected
    ()
  =
  let project =
    Test.ScratchPyrePysaProject.setup
      ~context
      ~force_pyre1:skip_for_pyrefly
      ~requires_type_of_expressions:false
      ["test.py", source]
  in
  let pyre_api = Test.ScratchPyrePysaProject.read_only_api project in
  let configuration = Test.ScratchPyrePysaProject.configuration_of project in
  let initial_callables =
    FetchCallables.from_qualifier ~configuration ~pyre_api ~qualifier:!&"test"
  in
  let scheduler = Test.mock_scheduler () in
  let scheduler_policy = Scheduler.Policy.legacy_fixed_chunk_count () in
  let definitions_and_stubs =
    Interprocedural.FetchCallables.get initial_callables ~definitions:true ~stubs:true
  in
  let callables_to_definitions_map =
    Interprocedural.CallablesSharedMemory.ReadWrite.from_callables
      ~scheduler
      ~scheduler_policy
      ~pyre_api
      definitions_and_stubs
  in
  let class_hierarchy_graph =
    ClassHierarchyGraph.Heap.from_qualifiers
      ~scheduler:(mock_scheduler ())
      ~scheduler_policies:Configuration.SchedulerPolicies.empty
      ~pyre_api
      ~qualifiers:[Ast.Reference.create "test"]
    |> ClassHierarchyGraph.SharedMemory.from_heap ~store_transitive_children_for:[]
  in
  let expected =
    match pyrefly_expected with
    | Some pyrefly_expected when PyrePysaApi.ReadOnly.is_pyrefly pyre_api -> pyrefly_expected
    | _ -> expected
  in
  let actual =
    ModelQueryExecution.CallableQueryExecutor.generate_annotations_from_query_on_target
      ~verbose:false
      ~pyre_api
      ~callables_to_definitions_map:
        (Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
      ~class_hierarchy_graph
      ~modelable:
        (ModelQueryExecution.CallableQueryExecutor.make_modelable
           ~pyre_api
           ~callables_to_definitions_map:
             (Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
           (Target.from_regular callable))
      query
  in
  Interprocedural.CallablesSharedMemory.ReadWrite.cleanup callables_to_definitions_map;
  assert_equal
    ~cmp:(List.equal equal_query_element)
    ~printer:(List.to_string ~f:show_query_element)
    expected
    actual


let assert_generated_annotations_for_attributes
    context
    ?(skip_for_pyrefly = false)
    ~source
    ~query
    ~name
    ~expected
    ()
  =
  let project =
    Test.ScratchPyrePysaProject.setup
      ~context
      ~force_pyre1:skip_for_pyrefly
      ~requires_type_of_expressions:false
      ["test.py", source]
  in
  let pyre_api = Test.ScratchPyrePysaProject.read_only_api project in
  let configuration = Test.ScratchPyrePysaProject.configuration_of project in
  let initial_callables =
    FetchCallables.from_qualifier ~configuration ~pyre_api ~qualifier:!&"test"
  in
  let scheduler = Test.mock_scheduler () in
  let scheduler_policy = Scheduler.Policy.legacy_fixed_chunk_count () in
  let definitions_and_stubs =
    Interprocedural.FetchCallables.get initial_callables ~definitions:true ~stubs:true
  in
  let callables_to_definitions_map =
    Interprocedural.CallablesSharedMemory.ReadWrite.from_callables
      ~scheduler
      ~scheduler_policy
      ~pyre_api
      definitions_and_stubs
  in
  let class_hierarchy_graph =
    ClassHierarchyGraph.Heap.from_qualifiers
      ~scheduler:(mock_scheduler ())
      ~scheduler_policies:Configuration.SchedulerPolicies.empty
      ~pyre_api
      ~qualifiers:[Ast.Reference.create "test"]
    |> ClassHierarchyGraph.SharedMemory.from_heap ~store_transitive_children_for:[]
  in
  let target = name |> Ast.Reference.create |> Target.create_object in
  let actual =
    ModelQueryExecution.AttributeQueryExecutor.generate_annotations_from_query_on_target
      ~verbose:false
      ~pyre_api
      ~callables_to_definitions_map:
        (Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
      ~class_hierarchy_graph
      ~modelable:
        (ModelQueryExecution.AttributeQueryExecutor.make_modelable
           ~pyre_api
           ~callables_to_definitions_map:
             (Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
           target)
      query
  in
  Interprocedural.CallablesSharedMemory.ReadWrite.cleanup callables_to_definitions_map;
  assert_equal
    ~cmp:(List.equal ModelParseResult.TaintAnnotation.equal)
    ~printer:(List.to_string ~f:ModelParseResult.TaintAnnotation.show)
    expected
    actual


let assert_generated_annotations_for_globals
    context
    ?(skip_for_pyrefly = false)
    ~source
    ~query
    ~name
    ~expected
    ()
  =
  let project =
    Test.ScratchPyrePysaProject.setup
      ~context
      ~force_pyre1:skip_for_pyrefly
      ~requires_type_of_expressions:false
      ["test.py", source]
  in
  let pyre_api = Test.ScratchPyrePysaProject.read_only_api project in
  let configuration = Test.ScratchPyrePysaProject.configuration_of project in
  let initial_callables =
    FetchCallables.from_qualifier ~configuration ~pyre_api ~qualifier:!&"test"
  in
  let definitions_and_stubs = FetchCallables.get ~definitions:true ~stubs:true initial_callables in
  let scheduler = Test.mock_scheduler () in
  let scheduler_policy = Scheduler.Policy.legacy_fixed_chunk_count () in
  let callables_to_definitions_map =
    Interprocedural.CallablesSharedMemory.ReadWrite.from_callables
      ~scheduler
      ~scheduler_policy
      ~pyre_api
      definitions_and_stubs
  in
  let class_hierarchy_graph =
    ClassHierarchyGraph.Heap.from_qualifiers
      ~scheduler:(mock_scheduler ())
      ~scheduler_policies:Configuration.SchedulerPolicies.empty
      ~pyre_api
      ~qualifiers:[Ast.Reference.create "test"]
    |> ClassHierarchyGraph.SharedMemory.from_heap ~store_transitive_children_for:[]
  in
  let target = name |> Ast.Reference.create |> Target.create_object in
  let actual =
    ModelQueryExecution.GlobalVariableQueryExecutor.generate_annotations_from_query_on_target
      ~verbose:false
      ~pyre_api
      ~callables_to_definitions_map:
        (Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
      ~class_hierarchy_graph
      ~modelable:
        (ModelQueryExecution.GlobalVariableQueryExecutor.make_modelable
           ~pyre_api
           ~callables_to_definitions_map:
             (Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
           target)
      query
  in
  Interprocedural.CallablesSharedMemory.ReadWrite.cleanup callables_to_definitions_map;
  assert_equal
    ~cmp:(List.equal ModelParseResult.TaintAnnotation.equal)
    ~printer:(List.to_string ~f:ModelParseResult.TaintAnnotation.show)
    expected
    actual


let test_generated_annotations_function_name context =
  let assert_generated_annotations = assert_generated_annotations context in
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
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[]
    ();
  (* Multiple definitions *)
  assert_generated_annotations
    ~source:{|
      def foo(): ...
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
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  assert_generated_annotations
    ~source:{|
      def foo(): ...
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
    ~callable:(Target.Regular.Function { name = "test.foo$2"; kind = Normal })
    ~expected:[]
    ~pyrefly_expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  assert_generated_annotations
    ~source:
      {|
      class Foo:
        @property
        def bar(self):
          return 0
        @bar.setter
        def bar(self, value):
          pass
      |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [FullyQualifiedNameConstraint (Equals "test.Foo.bar")];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:
      (Target.Regular.Method { class_name = "test.Foo"; method_name = "bar"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  assert_generated_annotations
    ~source:
      {|
      class Foo:
        @property
        def bar(self):
          return 0
        @bar.setter
        def bar(self, value):
          pass
      |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [FullyQualifiedNameConstraint (Equals "test.Foo.bar")];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:
      (Target.Regular.Method
         { class_name = "test.Foo"; method_name = "bar@setter"; kind = PyreflyPropertySetter })
    ~expected:[]
    ~pyrefly_expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  ()


(* Test multiple constraints. *)
let test_generated_annotations_and context =
  let assert_generated_annotations = assert_generated_annotations context in
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
    ~callable:(Target.Regular.Function { name = "test.barfoo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[]
    ();

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
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();

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
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  ()


(* Multiple productions. *)
let test_generated_annotations_multiple_productions context =
  let assert_generated_annotations = assert_generated_annotations context in
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
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test");
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          {
            root =
              AccessPath.Root.PositionalParameter
                { position = 0; name = "x"; positional_only = false };
            annotation = source "Test";
            generation_if_source = false;
          };
      ]
    ();

  ()


let test_generated_annotations_constants context =
  let assert_generated_annotations = assert_generated_annotations context in
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
        where = [Constant true];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
        where = [Constant false];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[]
    ();
  ()


(* All parameter taint. *)
let test_generated_annotations_all_parameters context =
  let assert_generated_annotations = assert_generated_annotations context in
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
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          {
            root =
              AccessPath.Root.PositionalParameter
                { position = 0; name = "x"; positional_only = false };
            annotation = source "Test";
            generation_if_source = false;
          };
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          {
            root =
              AccessPath.Root.PositionalParameter
                { position = 1; name = "y"; positional_only = false };
            annotation = source "Test";
            generation_if_source = false;
          };
      ]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          {
            root =
              AccessPath.Root.PositionalParameter
                { position = 1; name = "y"; positional_only = false };
            annotation = source "Test";
            generation_if_source = false;
          };
      ]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          {
            root =
              AccessPath.Root.PositionalParameter
                { position = 0; name = "x"; positional_only = false };
            annotation = source "Test";
            generation_if_source = false;
          };
      ]
    ();
  ()


(* Parameter taint. *)
let test_generated_annotations_parameter_taint context =
  let assert_generated_annotations = assert_generated_annotations context in
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
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          {
            root =
              AccessPath.Root.PositionalParameter
                { position = 0; name = "x"; positional_only = false };
            annotation = source "Test";
            generation_if_source = false;
          };
      ]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          {
            root =
              AccessPath.Root.PositionalParameter
                { position = 0; name = "x"; positional_only = false };
            annotation = source "Test";
            generation_if_source = false;
          };
      ]
    ();
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
                      (FullyQualifiedConstraint (Matches (Re2.create_exn "int")));
                  ];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          {
            root =
              AccessPath.Root.PositionalParameter
                { position = 0; name = "x"; positional_only = false };
            annotation = source "Test";
            generation_if_source = false;
          };
      ]
    ();
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
                         (FullyQualifiedConstraint (Equals "int")));
                  ];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          {
            root =
              AccessPath.Root.PositionalParameter
                { position = 1; name = "y"; positional_only = false };
            annotation = source "Test";
            generation_if_source = false;
          };
      ]
    ();
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
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          {
            root =
              AccessPath.Root.PositionalParameter
                { position = 0; name = "x"; positional_only = false };
            annotation = source "Test";
            generation_if_source = false;
          };
      ]
    ();
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
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          {
            root =
              AccessPath.Root.PositionalParameter
                { position = 0; name = "x"; positional_only = false };
            annotation = source "Test";
            generation_if_source = false;
          };
      ]
    ();
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
                      (FullyQualifiedConstraint (Matches (Re2.create_exn "int")));
                  ];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          {
            root =
              AccessPath.Root.PositionalParameter
                { position = 0; name = "x"; positional_only = false };
            annotation = source "Test";
            generation_if_source = false;
          };
      ]
    ();
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
                         (FullyQualifiedConstraint (Equals "int")));
                  ];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          {
            root =
              AccessPath.Root.PositionalParameter
                { position = 1; name = "y"; positional_only = false };
            annotation = source "Test";
            generation_if_source = false;
          };
      ]
    ();
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
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          {
            root =
              AccessPath.Root.PositionalParameter
                { position = 0; name = "x"; positional_only = false };
            annotation = source "Test";
            generation_if_source = false;
          };
      ]
    ();
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
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          {
            root =
              AccessPath.Root.PositionalParameter
                { position = 1; name = "y"; positional_only = false };
            annotation = source "Test";
            generation_if_source = false;
          };
      ]
    ();
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
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          {
            root =
              AccessPath.Root.PositionalParameter
                { position = 0; name = "x"; positional_only = false };
            annotation = source "Test";
            generation_if_source = false;
          };
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          {
            root =
              AccessPath.Root.PositionalParameter
                { position = 1; name = "y"; positional_only = false };
            annotation = source "Test";
            generation_if_source = false;
          };
      ]
    ();
  ()


let test_generated_annotations_typing_annotated context =
  let assert_generated_annotations = assert_generated_annotations context in
  (* typing.Annotated is lost during parsing, this is expected behavior. *)
  assert_generated_annotations
    ~source:{|
       import typing
       def foo() -> typing.Annotated[str, "foo"]: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [ReturnConstraint (FullyQualifiedConstraint (Matches (Re2.create_exn "foo")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[]
    ();
  assert_generated_annotations
    ~source:
      {|
       import typing
       def foo(x: typing.Annotated[int, "foo"], y: str): ...
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
              (AnnotationConstraint (FullyQualifiedConstraint (Matches (Re2.create_exn "foo"))));
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[]
    ();
  ()


(* Return annotation extends. *)
let test_generated_annotations_return_extends context =
  let assert_generated_annotations = assert_generated_annotations context in
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
    ~callable:
      (Target.Regular.Method { class_name = "test.Test"; method_name = "test1"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:
      (Target.Regular.Method { class_name = "test.Test"; method_name = "test2"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")]
    ();
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
    ~callable:
      (Target.Regular.Method { class_name = "test.Test"; method_name = "test3"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")]
    ();
  assert_generated_annotations
    ~source:
      {|
      import typing

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
    ~callable:(Target.Regular.Function { name = "test.test1"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")]
    ();
  assert_generated_annotations
    ~source:
      {|
      import typing

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
    ~callable:(Target.Regular.Function { name = "test.test2"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")]
    ();
  assert_generated_annotations
    ~source:
      {|
      import typing

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
    ~callable:(Target.Regular.Function { name = "test.test3"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")]
    ();
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
    ~callable:(Target.Regular.Function { name = "test.test1"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Function { name = "test.test1"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")]
    ();
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
    ~callable:(Target.Regular.Function { name = "test.test1"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")]
    ();
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
    ~callable:(Target.Regular.Function { name = "test.test1"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")]
    ();
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
    ~callable:(Target.Regular.Function { name = "test.test1"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")]
    ();
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
    ~callable:(Target.Regular.Function { name = "test.test1"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")]
    ();
  assert_generated_annotations
    ~source:
      {|
      from pyre_extensions import PyreReadOnly
      from typing import Optional

      class A:
        pass

      class B(A):
        pass

      class C(B):
        pass

      def test1() -> PyreReadOnly[Optional[PyreReadOnly[C]]]: ...
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
    ~callable:(Target.Regular.Function { name = "test.test1"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (sink "Test")]
    ();
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
    ~callable:(Target.Regular.Function { name = "test.test1"; kind = Normal })
    ~expected:[]
    ();
  ()


(* Any of. *)
let test_generated_annotations_any_of context =
  let assert_generated_annotations = assert_generated_annotations context in
  assert_generated_annotations
    ~source:{|
       def foo(a, b: int, c: str): ...
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
                AnyParameterConstraint
                  (AnnotationConstraint (FullyQualifiedConstraint (Equals "int")));
                ReturnConstraint (FullyQualifiedConstraint (Equals "int"));
              ];
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  assert_generated_annotations
    ~source:{|
       def foo(a, b, c: str) -> int: ...
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
                AnyParameterConstraint
                  (AnnotationConstraint (FullyQualifiedConstraint (Equals "int")));
                ReturnConstraint (FullyQualifiedConstraint (Equals "int"));
              ];
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  ()


(* All of. *)
let test_generated_annotations_all_of context =
  let assert_generated_annotations = assert_generated_annotations context in
  assert_generated_annotations
    ~source:{|
       def foo(a: int) -> int: ...
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
                AnyParameterConstraint
                  (AnnotationConstraint (FullyQualifiedConstraint (Equals "int")));
                ReturnConstraint (FullyQualifiedConstraint (Equals "int"));
              ];
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  (* Some cases where we don't match with "AllOf". *)
  assert_generated_annotations
    ~source:{|
       def foo(a: int): ...
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
                AnyParameterConstraint
                  (AnnotationConstraint (FullyQualifiedConstraint (Equals "int")));
                ReturnConstraint (FullyQualifiedConstraint (Equals "int"));
              ];
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[]
    ();
  assert_generated_annotations
    ~source:{|
       def foo(a) -> int: ...
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
                AnyParameterConstraint
                  (AnnotationConstraint (FullyQualifiedConstraint (Equals "int")));
                ReturnConstraint (FullyQualifiedConstraint (Equals "int"));
              ];
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[]
    ();
  ()


let test_generated_annotations_any_parameter context =
  let assert_generated_annotations = assert_generated_annotations context in
  (* Type annotation constraint for callables *)
  assert_generated_annotations
    ~source:{|
       def foo(a, b: str, c: str, d: int): ...
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
              (AnnotationConstraint (FullyQualifiedConstraint (Matches (Re2.create_exn "str"))));
          ];
        models =
          [
            Parameter
              {
                where =
                  [AnnotationConstraint (FullyQualifiedConstraint (Matches (Re2.create_exn "str")))];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          {
            root =
              AccessPath.Root.PositionalParameter
                { position = 1; name = "b"; positional_only = false };
            annotation = source "Test";
            generation_if_source = false;
          };
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          {
            root =
              AccessPath.Root.PositionalParameter
                { position = 2; name = "c"; positional_only = false };
            annotation = source "Test";
            generation_if_source = false;
          };
      ]
    ();
  assert_generated_annotations
    ~source:{|
       def foo(a, b: str, c: str, d: int): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [AnyParameterConstraint (AnnotationConstraint (FullyQualifiedConstraint (Equals "int")))];
        models =
          [
            Parameter
              {
                where = [AnnotationConstraint (FullyQualifiedConstraint (Equals "int"))];
                taint = [TaintAnnotation (source "Test")];
              };
          ];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:
      [
        ModelParseResult.ModelAnnotation.ParameterAnnotation
          {
            root =
              AccessPath.Root.PositionalParameter
                { position = 3; name = "d"; positional_only = false };
            annotation = source "Test";
            generation_if_source = false;
          };
      ]
    ();
  ()


let test_generated_annotations_return_annotation context =
  let assert_generated_annotations = assert_generated_annotations context in
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
        where = [ReturnConstraint (FullyQualifiedConstraint (Equals "int"))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
        where = [ReturnConstraint (FullyQualifiedConstraint (Equals "int"))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.bar"; kind = Normal })
    ~expected:[]
    ();
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
        where = [ReturnConstraint (FullyQualifiedConstraint (Matches (Re2.create_exn "str")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  assert_generated_annotations
    ~source:
      {|
       import typing
       def foo() -> str: ...
       def bar() -> typing.List[str]: ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [ReturnConstraint (FullyQualifiedConstraint (Matches (Re2.create_exn "str")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.bar"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  ()


let test_generated_annotations_any_decorator context =
  let assert_generated_annotations = assert_generated_annotations context in
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
        where = [AnyDecoratorConstraint (FullyQualifiedCallee (Matches (Re2.create_exn "d1")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  (* Basic test case for `FullyQualifiedCallee`. *)
  assert_generated_annotations
    ~source:
      {|
      class Flask:
        def route(self):
          pass
      application = Flask()
      @application.route
      def my_view():
        pass
      |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_flask_route";
        logging_group_name = None;
        path = None;
        where =
          [
            AnyDecoratorConstraint
              (FullyQualifiedCallee (Matches (Re2.create_exn "test.Flask.route")));
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.my_view"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  (* Test `FullyQualifiedCallee` when the callee is unresolvable. *)
  assert_generated_annotations
    ~source:{|
      @application.route
      def my_view():
        pass
      |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_flask_route_unresolved_callee";
        logging_group_name = None;
        path = None;
        where =
          [
            AnyDecoratorConstraint
              (FullyQualifiedCallee (Matches (Re2.create_exn "test.Flask.route")));
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.my_view"; kind = Normal })
    ~expected:[]
    ();
  (* Test `FullyQualifiedCallee` on a decorator factory. *)
  assert_generated_annotations
    ~source:
      {|
      class Flask:
        def route(a, b):
          pass
      application = Flask()
      @application.route(1, 2)
      def my_view():
        pass
      |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_flask_route_decorator_factory";
        logging_group_name = None;
        path = None;
        where =
          [
            AnyDecoratorConstraint
              (FullyQualifiedCallee (Matches (Re2.create_exn "test.Flask.route")));
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.my_view"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  (* Test `FullyQualifiedCallee` when there are multiple decorators. We consider it as being matched
     if one of the decorators match. *)
  assert_generated_annotations
    ~source:
      {|
      class Flask:
        def route(self):
          pass
        def other():
          pass
      application = Flask()
      @application.route
      @application.other
      def my_view():
        pass
      |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_flask_route_multiple_decorators";
        logging_group_name = None;
        path = None;
        where =
          [
            AnyDecoratorConstraint
              (FullyQualifiedCallee (Matches (Re2.create_exn "test.Flask.route")));
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.my_view"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  (* Test `FullyQualifiedCallee` when the decorator is overriden. *)
  assert_generated_annotations
    ~source:
      {|
      class Flask:
        def route(self):
          pass
      class OverrideFlask(Flask):
        def route(self):
          pass
      application = OverrideFlask()
      @application.route
      def my_view():
        pass
      |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_flask_route_base_method";
        logging_group_name = None;
        path = None;
        where =
          [
            AnyDecoratorConstraint
              (FullyQualifiedCallee (Matches (Re2.create_exn "test.OverrideFlask.route")));
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.my_view"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  (* Test `FullyQualifiedCallee` when the decorator is a base method. *)
  assert_generated_annotations
    ~source:
      {|
      class Flask:
        def route(self):
          pass
      class OverrideFlask(Flask):
        pass
      application = OverrideFlask()
      @application.route
      def my_view():
        pass
      |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_flask_route_override";
        logging_group_name = None;
        path = None;
        where =
          [
            AnyDecoratorConstraint
              (FullyQualifiedCallee (Matches (Re2.create_exn "test.Flask.route")));
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.my_view"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  (* Test `FullyQualifiedCallee` for class decorators. *)
  assert_generated_annotations
    ~source:
      {|
      class registered:
        def __init__(self, arg):
          self.arg = arg
        def __call__(self, func):
          pass
      @registered(arg=1)
      def my_view():
        pass
      |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_flask_route";
        logging_group_name = None;
        path = None;
        where =
          [
            AnyDecoratorConstraint
              (AllOf
                 (* We can choose to write either of the following constraint. *)
                 [
                   FullyQualifiedCallee (Matches (Re2.create_exn "test.registered"));
                   FullyQualifiedCallee (Equals "test.registered.__init__");
                 ]);
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.my_view"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ~skip_for_pyrefly:true (* TODO(T225700656): Improve call graph building. *)
    ();
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
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
        where = [AnyDecoratorConstraint (FullyQualifiedCallee (Matches (Re2.create_exn "d1")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.bar"; kind = Normal })
    ~expected:[]
    ();
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
        where = [AnyDecoratorConstraint (FullyQualifiedCallee (Matches (Re2.create_exn "d1")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.baz"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  assert_generated_annotations
    ~source:
      {|
       class Flask:
         def route(self):
          pass
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
              (FullyQualifiedCallee (Matches (Re2.create_exn "test.Flask.route")));
          ];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[]
    ();
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
        where = [AnyDecoratorConstraint (FullyQualifiedCallee (Matches (Re2.create_exn "d1")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.baz"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
        where = [AnyDecoratorConstraint (FullyQualifiedCallee (Equals "test.d1"))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.baz"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
                   FullyQualifiedCallee (Equals "test.d1");
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
    ~callable:(Target.Regular.Function { name = "test.baz"; kind = Normal })
    ~expected:[]
    ();
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
                   FullyQualifiedCallee (Equals "test.d1");
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
    ~callable:(Target.Regular.Function { name = "test.baz"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
                   FullyQualifiedCallee (Equals "test.d1");
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
    ~callable:(Target.Regular.Function { name = "test.baz"; kind = Normal })
    ~expected:[]
    ();
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
                   FullyQualifiedCallee (Equals "test.d1");
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
    ~callable:(Target.Regular.Function { name = "test.baz"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
                   FullyQualifiedCallee (Equals "test.d1");
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
    ~callable:(Target.Regular.Function { name = "test.baz"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
                   FullyQualifiedCallee (Equals "test.d1");
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
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[]
    ();
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
                   FullyQualifiedCallee (Equals "test.d1");
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
    ~callable:(Target.Regular.Function { name = "test.baz"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  ()


let test_generated_annotations_class_constraint context =
  let assert_generated_annotations = assert_generated_annotations context in
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
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.D"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();

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
    ~callable:(Target.Regular.Method { class_name = "test.DC"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  ()


let test_generated_annotations_class_decorator context =
  let assert_generated_annotations = assert_generated_annotations context in
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
    ~callable:(Target.Regular.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  ()


(* Test attribute models. *)
let test_generated_annotations_for_attributes context =
  let assert_generated_annotations_for_attributes =
    assert_generated_annotations_for_attributes context
  in
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
    ~expected:[source "Test"]
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
        where = [ClassConstraint (NameConstraint (Matches (Re2.create_exn "C")))];
        models = [Attribute [TaintAnnotation (sink "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.C.x"
    ~expected:[sink "Test"]
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
        where = [ClassConstraint (NameConstraint (Matches (Re2.create_exn "C")))];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.D.y"
    ~expected:[]
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
    ~name:"test.C.x"
    ~expected:[source "Test"]
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
    ~expected:[source "Test"]
    ();
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
    ~expected:[]
    ();
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
        where = [AnnotationConstraint (OriginalAnnotationConstraint (Equals "int"))];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.C.x"
    ~expected:[source "Test"]
    ();
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
        where = [AnnotationConstraint (OriginalAnnotationConstraint (Equals "int"))];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.C.y"
    ~expected:[]
    ();
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
        where =
          [AnnotationConstraint (OriginalAnnotationConstraint (Matches (Re2.create_exn "Foo")))];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.C.x"
    ~expected:[source "Test"]
    ();
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
        where = [AnnotationConstraint (FullyQualifiedConstraint (Matches (Re2.create_exn "Foo")))];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.C.x"
    ~expected:[source "Test"]
    ();
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
        where =
          [AnnotationConstraint (OriginalAnnotationConstraint (Matches (Re2.create_exn "Foo")))];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.C.y"
    ~expected:[source "Test"]
    ();
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
        where =
          [AnnotationConstraint (OriginalAnnotationConstraint (Matches (Re2.create_exn "Foo")))];
        models = [Attribute [TaintAnnotation (source "Test")]];
        find = Attribute;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.C.z"
    ~expected:[]
    ();
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
    ~expected:[]
    ();
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
    ~expected:[source "Test"]
    ();

  ()


(* Test 'Not' clause *)
let test_generated_annotations_not context =
  let assert_generated_annotations = assert_generated_annotations context in
  let assert_generated_annotations_for_attributes =
    assert_generated_annotations_for_attributes context
  in
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
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
    ~callable:(Target.Regular.Function { name = "test.barfoo"; kind = Normal })
    ~expected:[]
    ();
  assert_generated_annotations
    ~source:{|
       def foo(a) -> int: ...
       def bar(b): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [Not (ReturnConstraint (FullyQualifiedConstraint (Equals "int")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Function;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Function { name = "test.foo"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.DC"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
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
    ~expected:[]
    ();
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
    ~expected:[]
    ();
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
    ~expected:[source "Test"]
    ();
  ()


(* Test transitive extends *)
let test_generated_annotations_class_constraints context =
  let assert_generated_annotations = assert_generated_annotations context in
  let assert_generated_annotations_for_attributes =
    assert_generated_annotations_for_attributes context
  in
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
    ~expected:[source "Test"]
    ();
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
    ~expected:[source "Test"]
    ();
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
    ~expected:[source "Test"]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.D"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();

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
    ~expected:[]
    ();
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
    ~expected:[source "Test"]
    ();
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
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();

  ()


(* Test cls.any_child *)
let test_generated_annotations_class_any_child context =
  let assert_generated_annotations = assert_generated_annotations context in
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
    ~callable:(Target.Regular.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  ()


(* Test cls.any_parent *)
let test_generated_annotations_class_any_parent context =
  let assert_generated_annotations = assert_generated_annotations context in
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
    ~callable:(Target.Regular.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
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
    ~callable:(Target.Regular.Method { class_name = "test.D"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  ()


(* Test any_overriden_method *)
let test_generated_annotations_class_any_overriden_method context =
  let assert_generated_annotations = assert_generated_annotations context in
  assert_generated_annotations
    ~source:
      {|
      class A:
        def foo(self): ...
      class B(A):
        def foo(self): ...
      class C(B):
        def foo(self): ...
        def bar(self): ...
      class D(C):
        def foo(self): ...
        def bar(self): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyOverridenMethod (Constant true)];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Method { class_name = "test.A"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
  assert_generated_annotations
    ~source:
      {|
      class A:
        def foo(self): ...
      class B(A):
        def foo(self): ...
      class C(B):
        def foo(self): ...
        def bar(self): ...
      class D(C):
        def foo(self): ...
        def bar(self): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyOverridenMethod (Constant true)];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  assert_generated_annotations
    ~source:
      {|
      class A:
        def foo(self): ...
      class B(A):
        def foo(self): ...
      class C(B):
        def foo(self): ...
        def bar(self): ...
      class D(C):
        def foo(self): ...
        def bar(self): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyOverridenMethod (Constant true)];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  assert_generated_annotations
    ~source:
      {|
      class A:
        def foo(self): ...
      class B(A):
        def foo(self): ...
      class C(B):
        def foo(self): ...
        def bar(self): ...
      class D(C):
        def foo(self): ...
        def bar(self): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyOverridenMethod (Constant true)];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "bar"; kind = Normal })
    ~expected:[]
    ();
  assert_generated_annotations
    ~source:
      {|
      class A:
        def foo(self): ...
      class B(A):
        def foo(self): ...
      class C(B):
        def foo(self): ...
        def bar(self): ...
      class D(C):
        def foo(self): ...
        def bar(self): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyOverridenMethod (Constant true)];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Method { class_name = "test.D"; method_name = "bar"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  (* Only consider instance methods. *)
  assert_generated_annotations
    ~source:
      {|
      class A:
        @classmethod
        def foo(cls): ...
      class B(A):
        @classmethod
        def foo(cls): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyOverridenMethod (Constant true)];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
  assert_generated_annotations
    ~source:
      {|
      class A:
        @staticmethod
        def foo(): ...
      class B(A):
        @staticmethod
        def foo(): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyOverridenMethod (Constant true)];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
  assert_generated_annotations
    ~source:
      {|
      class A:
        def foo(self): ...
      class B(A):
        def foo(self): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyOverridenMethod (ClassConstraint (NameConstraint (Equals "A")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  assert_generated_annotations
    ~source:
      {|
      class A:
        def foo(self): ...
      class B(A):
        def foo(self): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyOverridenMethod (ClassConstraint (NameConstraint (Equals "B")))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
  assert_generated_annotations
    ~source:
      {|
      class A:
        def foo(self): ...
      class B(A):
        def foo(self): ...
      class C(B):
        def foo(self): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyOverridenMethod (AnyOverridenMethod (Constant true))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Method { class_name = "test.B"; method_name = "foo"; kind = Normal })
    ~expected:[]
    ();
  assert_generated_annotations
    ~source:
      {|
      class A:
        def foo(self): ...
      class B(A):
        def foo(self): ...
      class C(B):
        def foo(self): ...
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where = [AnyOverridenMethod (AnyOverridenMethod (Constant true))];
        models = [Return [TaintAnnotation (source "Test")]];
        find = Method;
        expected_models = [];
        unexpected_models = [];
      }
    ~callable:(Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal })
    ~expected:[ModelParseResult.ModelAnnotation.ReturnAnnotation (source "Test")]
    ();
  ()


let test_generated_annotations_for_globals context =
  let assert_generated_annotations_for_globals = assert_generated_annotations_for_globals context in
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
    ~expected:[source "Test"]
    ();
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
    ~expected:[source "Test"]
    ();
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
    ~expected:[]
    ();
  assert_generated_annotations_for_globals
    ~source:{|
      foo: list[int] = []
     |}
    ~query:
      {
        location = Ast.Location.any;
        name = "get_foo";
        logging_group_name = None;
        path = None;
        where =
          [
            AnnotationConstraint
              (FullyQualifiedConstraint (Matches (Re2.create_exn "^(list|typing.List)\\[int\\]$")));
          ];
        models = [Global [TaintAnnotation (source "Test")]];
        find = Global;
        expected_models = [];
        unexpected_models = [];
      }
    ~name:"test.foo"
    ~expected:[source "Test"]
    ();
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
                  ModelQuery.FormatString.Substring.ClassName;
                  ModelQuery.FormatString.Substring.Literal ":";
                  ModelQuery.FormatString.Substring.MethodName;
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
  let assert_generated_cache ~source ~queries ~regular_callables ~expected =
    let project =
      Test.ScratchPyrePysaProject.setup
        ~context
        ~requires_type_of_expressions:false
        ["test.py", source]
    in
    let pyre_api = Test.ScratchPyrePysaProject.read_only_api project in
    let configuration = Test.ScratchPyrePysaProject.configuration_of project in
    let initial_callables =
      FetchCallables.from_qualifier ~configuration ~pyre_api ~qualifier:!&"test"
    in
    let scheduler = Test.mock_scheduler () in
    let scheduler_policy = Scheduler.Policy.legacy_fixed_chunk_count () in
    let definitions_and_stubs =
      Interprocedural.FetchCallables.get initial_callables ~definitions:true ~stubs:true
    in
    let callables_to_definitions_map =
      Interprocedural.CallablesSharedMemory.ReadWrite.from_callables
        ~scheduler
        ~scheduler_policy
        ~pyre_api
        definitions_and_stubs
    in
    let class_hierarchy_graph =
      ClassHierarchyGraph.Heap.from_qualifiers
        ~scheduler:(mock_scheduler ())
        ~scheduler_policies:Configuration.SchedulerPolicies.empty
        ~pyre_api
        ~qualifiers:[Ast.Reference.create "test"]
      |> ClassHierarchyGraph.SharedMemory.from_heap ~store_transitive_children_for:[]
    in
    let actual =
      ModelQueryExecution.CallableQueryExecutor.generate_cache_from_queries_on_targets
        ~verbose:false
        ~pyre_api
        ~callables_to_definitions_map:
          (Interprocedural.CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
        ~class_hierarchy_graph
        ~targets:(List.map regular_callables ~f:Target.from_regular)
        queries
    in
    let expected =
      List.fold
        ~init:ModelQueryExecution.ReadWriteCache.empty
        ~f:(fun cache (kind, name, target) ->
          ModelQueryExecution.ReadWriteCache.write
            cache
            ~kind
            ~name
            ~target:(Target.from_regular target))
        expected
    in
    Interprocedural.CallablesSharedMemory.ReadWrite.cleanup callables_to_definitions_map;
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
                  name = [ModelQuery.FormatString.Substring.FunctionName];
                };
            ];
          find = Function;
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ~regular_callables:
      [
        Target.Regular.Function { name = "test.foo"; kind = Normal };
        Target.Regular.Function { name = "test.no_match"; kind = Normal };
      ]
    ~expected:["thrift", "foo", Target.Regular.Function { name = "test.foo"; kind = Normal }];
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
                      ModelQuery.FormatString.Substring.ClassName;
                      ModelQuery.FormatString.Substring.Literal ":";
                      ModelQuery.FormatString.Substring.MethodName;
                    ];
                };
            ];
          find = Method;
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ~regular_callables:
      [
        Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal };
        Target.Regular.Method { class_name = "test.D"; method_name = "foo"; kind = Normal };
      ]
    ~expected:
      [
        ( "thrift",
          "C:foo",
          Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal } );
        ( "thrift",
          "D:foo",
          Target.Regular.Method { class_name = "test.D"; method_name = "foo"; kind = Normal } );
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
                  name = [ModelQuery.FormatString.Substring.MethodName];
                };
            ];
          find = Method;
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ~regular_callables:
      [
        Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal };
        Target.Regular.Method { class_name = "test.D"; method_name = "foo"; kind = Normal };
      ]
    ~expected:
      [
        ( "thrift",
          "foo",
          Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal } );
        ( "thrift",
          "foo",
          Target.Regular.Method { class_name = "test.D"; method_name = "foo"; kind = Normal } );
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
                  name = [ModelQuery.FormatString.Substring.MethodName];
                };
              WriteToCache
                {
                  ModelQuery.WriteToCache.kind = "b";
                  name = [ModelQuery.FormatString.Substring.MethodName];
                };
            ];
          find = Method;
          expected_models = [];
          unexpected_models = [];
        };
      ]
    ~regular_callables:
      [
        Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal };
        Target.Regular.Method { class_name = "test.D"; method_name = "foo"; kind = Normal };
      ]
    ~expected:
      [
        ( "a",
          "foo",
          Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal } );
        ( "b",
          "foo",
          Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal } );
      ];
  ()


let test_read_from_cache_constraints _ =
  let assert_target_candidates ~cache ~constraints ~expected =
    let cache =
      List.fold
        ~init:ModelQueryExecution.ReadWriteCache.empty
        ~f:(fun cache (kind, name, regular_target) ->
          ModelQueryExecution.ReadWriteCache.write
            cache
            ~kind
            ~name
            ~target:(Target.from_regular regular_target))
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
      ( "thrift",
        "A:foo",
        Target.Regular.Method { class_name = "test.A"; method_name = "foo"; kind = Normal } );
      ( "thrift",
        "B:foo",
        Target.Regular.Method { class_name = "test.B"; method_name = "foo"; kind = Normal } );
      ( "thrift",
        "C:foo",
        Target.Regular.Method { class_name = "test.C"; method_name = "foo"; kind = Normal } );
    ]
  in
  assert_target_candidates
    ~cache
    ~constraints:[ReadFromCache { kind = "thrift"; name = "A:foo" }]
    ~expected:
      (Set
         (Target.Set.of_list
            [
              Target.Regular.Method { class_name = "test.A"; method_name = "foo"; kind = Normal }
              |> Target.from_regular;
            ]));
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
            [
              Target.Regular.Method { class_name = "test.A"; method_name = "foo"; kind = Normal }
              |> Target.from_regular;
            ]));
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
              Target.Regular.Method { class_name = "test.A"; method_name = "foo"; kind = Normal }
              |> Target.from_regular;
              Target.Regular.Method { class_name = "test.B"; method_name = "foo"; kind = Normal }
              |> Target.from_regular;
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
            [
              Target.Regular.Method { class_name = "test.A"; method_name = "foo"; kind = Normal }
              |> Target.from_regular;
            ]));
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
         "generated_annotations_function_name" >:: test_generated_annotations_function_name;
         "generated_annotations_and" >:: test_generated_annotations_and;
         "generated_annotations_multiple_productions"
         >:: test_generated_annotations_multiple_productions;
         "generated_annotations_constants" >:: test_generated_annotations_constants;
         "generated_annotations_all_parameters" >:: test_generated_annotations_all_parameters;
         "generated_annotations_parameter_taint" >:: test_generated_annotations_parameter_taint;
         "generated_annotations_typing_annotated" >:: test_generated_annotations_typing_annotated;
         "generated_annotations_return_extends" >:: test_generated_annotations_return_extends;
         "generated_annotations_any_of" >:: test_generated_annotations_any_of;
         "generated_annotations_all_of" >:: test_generated_annotations_all_of;
         "generated_annotations_any_parameter" >:: test_generated_annotations_any_parameter;
         "generated_annotations_return_annotation" >:: test_generated_annotations_return_annotation;
         "generated_annotations_any_decorator" >:: test_generated_annotations_any_decorator;
         "generated_annotations_class_constraint" >:: test_generated_annotations_class_constraint;
         "generated_annotations_class_decorator" >:: test_generated_annotations_class_decorator;
         "generated_annotations_for_attributes" >:: test_generated_annotations_for_attributes;
         "generated_annotations_not" >:: test_generated_annotations_not;
         "generated_annotations_class_constraints" >:: test_generated_annotations_class_constraints;
         "generated_annotations_class_any_child" >:: test_generated_annotations_class_any_child;
         "generated_annotations_class_any_parent" >:: test_generated_annotations_class_any_parent;
         "generated_annotations_class_any_overriden_method"
         >:: test_generated_annotations_class_any_overriden_method;
         "generated_annotations_for_globals" >:: test_generated_annotations_for_globals;
         "partition_cache_queries" >:: test_partition_cache_queries;
         "generated_cache" >:: test_generated_cache;
         "read_from_cache_constraints" >:: test_read_from_cache_constraints;
         "model_query_error" >:: test_model_query_error;
       ]
  |> Test.run
