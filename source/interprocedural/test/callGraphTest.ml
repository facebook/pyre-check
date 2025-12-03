(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Analysis
open Test
open Interprocedural
open CallGraph
open Shims

let find_define_exn ~define_name ~module_name source =
  let find_define define =
    String.equal
      (FunctionDefinition.qualified_name_of_define ~module_name (Node.value define)
      |> Reference.show)
      define_name
  in
  List.find_exn
    (Preprocessing.defines ~include_nested:true ~include_toplevels:true source)
    ~f:find_define


let compute_define_call_graph
    ~callable
    ~module_name
    ~pyre_api
    ~configuration
    ~object_targets
    ~maximum_target_depth
  =
  let static_analysis_configuration =
    Configuration.StaticAnalysis.create ~maximum_target_depth configuration ()
  in
  let override_graph_heap =
    OverrideGraph.Heap.from_qualifier
      ~pyre_api
      ~skip_overrides_targets:Reference.SerializableSet.empty
      module_name
  in
  let override_graph_shared_memory = OverrideGraph.SharedMemory.from_heap override_graph_heap in
  let initial_callables =
    FetchCallables.from_qualifier ~configuration ~pyre_api ~qualifier:module_name
  in
  let definitions = FetchCallables.get_definitions initial_callables in
  let scheduler = Test.mock_scheduler () in
  let scheduler_policy = Scheduler.Policy.legacy_fixed_chunk_count () in
  let definitions_and_stubs =
    Interprocedural.FetchCallables.get initial_callables ~definitions:true ~stubs:true
  in
  let callables_to_definitions_map =
    CallablesSharedMemory.ReadWrite.from_callables
      ~scheduler
      ~scheduler_policy
      ~pyre_api
      definitions_and_stubs
  in
  let type_of_expression_shared_memory =
    Interprocedural.TypeOfExpressionSharedMemory.create
      ~pyre_api
      ~callables_to_definitions_map:
        (CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
      ()
  in
  let callables_to_decorators_map =
    CallableToDecoratorsMap.SharedMemory.create
      ~callables_to_definitions_map:
        (CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
      ~scheduler
      ~scheduler_policy
      ~is_pyrefly:(PyrePysaApi.ReadOnly.is_pyrefly pyre_api)
      definitions
  in
  let call_graph =
    match pyre_api with
    | PyrePysaApi.ReadOnly.Pyre1 _ ->
        let source = TestHelper.source_from_qualifier ~pyre_api module_name in
        let define =
          find_define_exn
            ~define_name:(Target.define_name_exn callable |> Reference.show)
            ~module_name
            source
        in
        CallGraphBuilder.call_graph_of_define
          ~static_analysis_configuration
          ~pyre_api
          ~override_graph:
            (Some
               (Interprocedural.OverrideGraph.SharedMemory.read_only override_graph_shared_memory))
          ~attribute_targets:
            (object_targets |> List.map ~f:Target.from_regular |> Target.HashSet.of_list)
          ~callables_to_definitions_map:
            (CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
          ~callables_to_decorators_map:
            (CallableToDecoratorsMap.SharedMemory.read_only callables_to_decorators_map)
          ~type_of_expression_shared_memory
          ~check_invariants:true
          ~qualifier:module_name
          ~callable
          ~define
    | PyrePysaApi.ReadOnly.Pyrefly _ ->
        let { CallGraph.SharedMemory.define_call_graphs; _ } =
          CallGraphBuilder.build_whole_program_call_graph
            ~scheduler
            ~static_analysis_configuration
            ~pyre_api
            ~resolve_module_path:None
            ~callables_to_definitions_map:
              (CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
            ~callables_to_decorators_map:
              (CallableToDecoratorsMap.SharedMemory.read_only callables_to_decorators_map)
            ~type_of_expression_shared_memory
            ~override_graph:
              (Some
                 (Interprocedural.OverrideGraph.SharedMemory.read_only override_graph_shared_memory))
            ~store_shared_memory:true
            ~attribute_targets:
              (object_targets |> List.map ~f:Target.from_regular |> Target.Set.of_list)
            ~skip_analysis_targets:(Target.HashSet.create ())
            ~check_invariants:true
            ~definitions:[callable]
            ~create_dependency_for:CallGraph.AllTargetsUseCase.Everything
        in
        let strip_builtins_from_string reference =
          reference
          |> Reference.create
          |> Interprocedural.PyreflyApi.strip_builtins_prefix
          |> Reference.show
        in
        let strip_builtins_from_target = function
          | Target.Regular (Target.Regular.Method { class_name; method_name; kind }) ->
              Target.Regular
                (Target.Regular.Method
                   { class_name = strip_builtins_from_string class_name; method_name; kind })
          | target -> target
        in
        CallGraph.SharedMemory.ReadOnly.get
          (CallGraph.SharedMemory.read_only define_call_graphs)
          ~cache:false
          ~callable
        |> Option.value_exn
        |> CallGraph.DefineCallGraph.map_target
             ~f:strip_builtins_from_target
             ~map_call_if:(fun _ -> true)
             ~map_return_if:(fun _ -> true)
        |> CallGraph.DefineCallGraph.map_receiver_class
             ~f:strip_builtins_from_string
             ~map_call_if:(fun _ -> true)
             ~map_return_if:(fun _ -> true)
  in
  OverrideGraph.SharedMemory.cleanup override_graph_shared_memory;
  call_graph, callables_to_definitions_map, type_of_expression_shared_memory


let assert_call_graph_of_define
    ?(object_targets = [])
    ?(maximum_target_depth = Configuration.StaticAnalysis.default_maximum_target_depth)
    ?((* Whether to run this test with PyreflyApi (i.e import call graphs from pyrefly) *)
      skip_for_pyrefly = true)
    ?pyrefly_expected
    ~source
    ~define_name
    ~expected
    ~(* Whether this test has been duplicated in the call graph building in Pyrefly *)
     _migrated_to_pyrefly
    ?(cmp = DefineCallGraphForTest.equal)
    ()
    context
  =
  let module_name, pyre_api, configuration =
    TestHelper.setup_single_py_file
      ~force_pyre1:skip_for_pyrefly
      ~requires_type_of_expressions:false
      ~file_name:"test.py"
      ~context
      ~source
      ()
  in
  let callable =
    match pyre_api with
    | PyrePysaApi.ReadOnly.Pyre1 _ ->
        let source = TestHelper.source_from_qualifier ~pyre_api module_name in
        let define = find_define_exn ~define_name ~module_name source in
        Target.from_define ~define_name:(Reference.create define_name) ~define:(Node.value define)
    | PyrePysaApi.ReadOnly.Pyrefly pyrefly_api ->
        Interprocedural.PyreflyApi.ReadOnly.target_from_define_name
          pyrefly_api
          ~override:false
          (Reference.create define_name)
  in
  let expected =
    match pyrefly_expected, pyre_api with
    | Some pyrefly_expected, PyrePysaApi.ReadOnly.Pyrefly _ -> pyrefly_expected
    | _ -> expected
  in
  let expected = DefineCallGraphForTest.from_expected expected in
  let actual, callables_to_definitions_map, _ =
    compute_define_call_graph
      ~callable
      ~module_name
      ~pyre_api
      ~configuration
      ~object_targets
      ~maximum_target_depth
  in
  CallablesSharedMemory.ReadWrite.cleanup callables_to_definitions_map;
  assert_equal
    ~cmp
    ~printer:DefineCallGraphForTest.show
    ~pp_diff:(Test.diff ~print:DefineCallGraphForTest.pp)
    expected
    (DefineCallGraph.for_test actual)


let assert_higher_order_call_graph_of_define
    ?(object_targets = [])
    ?(initial_state = CallGraphBuilder.HigherOrderCallGraph.State.empty)
    ?(called_when_parameter = Target.HashSet.create ())
    ?(maximum_parameterized_targets_at_call_site =
      Configuration.StaticAnalysis.default_maximum_parameterized_targets_at_call_site)
    ?callable
    ~source
    ~define_name
    ~expected_call_graph
    ~expected_returned_callables
    ?(cmp = CallGraphTestHelper.HigherOrderCallGraphForTest.equal)
    ()
    context
  =
  let open CallGraphTestHelper in
  let expected =
    HigherOrderCallGraphForTest.from_expected
      {
        HigherOrderCallGraphForTest.Expected.returned_callables = expected_returned_callables;
        call_graph = expected_call_graph;
      }
  in
  let module_name, pyre_api, configuration =
    TestHelper.setup_single_py_file
      ~force_pyre1:true
      ~requires_type_of_expressions:false
      ~file_name:"test.py"
      ~context
      ~source
      ()
  in
  let override_graph_shared_memory =
    module_name
    |> OverrideGraph.Heap.from_qualifier
         ~pyre_api
         ~skip_overrides_targets:Reference.SerializableSet.empty
    |> OverrideGraph.SharedMemory.from_heap
  in
  let () = OverrideGraph.SharedMemory.cleanup override_graph_shared_memory in
  let source = TestHelper.source_from_qualifier ~pyre_api module_name in
  let define = find_define_exn ~define_name ~module_name source in
  let callable =
    match callable with
    | Some callable -> callable
    | None ->
        Target.from_define ~define_name:(Reference.create define_name) ~define:(Node.value define)
  in
  let maximum_target_depth = Configuration.StaticAnalysis.default_maximum_target_depth in
  let define_call_graph, callables_to_definitions_map, type_of_expression_shared_memory =
    compute_define_call_graph
      ~callable
      ~module_name
      ~pyre_api
      ~configuration
      ~object_targets
      ~maximum_target_depth
  in
  let actual =
    CallGraphBuilder.higher_order_call_graph_of_define
      ~define_call_graph
      ~pyre_api
      ~callables_to_definitions_map:
        (CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
      ~type_of_expression_shared_memory
      ~skip_analysis_targets:(Target.HashSet.create ())
      ~called_when_parameter
      ~callable
      ~qualifier:module_name
      ~define
      ~initial_state
      ~get_callee_model:(fun _ -> None)
      ~profiler:CallGraphProfiler.disabled
      ~maximum_target_depth
      ~maximum_parameterized_targets_at_call_site:(Some maximum_parameterized_targets_at_call_site)
    |> HigherOrderCallGraphForTest.from_actual
  in
  CallablesSharedMemory.ReadWrite.cleanup callables_to_definitions_map;
  assert_equal
    ~cmp
    ~printer:HigherOrderCallGraphForTest.show
    ~pp_diff:(Test.diff ~print:HigherOrderCallGraphForTest.pp)
    expected
    actual


let test_call_graph_of_define =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:{|
     def foo():
         bar()

     def bar():
         pass
  |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "3:4-3:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.bar"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:
             {|
     def foo(c: C):
         c.m()

     class C:
       def m(self):
         pass
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "3:4-3:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create
                            ~implicit_receiver:true
                            ~receiver_class:"test.C"
                            (Target.create_method !&"test.C" "m");
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
     def foo():
       if 1 > 2:
         f = bar
       else:
         f = baz
       f()
     def baz() -> int: ...
     def bar() -> bool: ...
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "7:2-7:5",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.bool)
                            (Target.Regular.Function { name = "test.bar"; kind = Normal });
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.integer)
                            (Target.Regular.Function { name = "test.baz"; kind = Normal });
                        ]
                      ()) );
               ( "4:8-4:11|artificial-attribute-access|qualification:test.bar",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 ~return_type:(Some ReturnType.bool)
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
                             ]
                           ())
                      ()) );
               ( "6:8-6:11|artificial-attribute-access|qualification:test.baz",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 ~return_type:(Some ReturnType.integer)
                                 (Target.Regular.Function { name = "test.baz"; kind = Normal });
                             ]
                           ())
                      ()) );
               ( "3:5-3:10|artificial-call|comparison",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"int"
                            ~return_type:(Some ReturnType.bool)
                            (Target.Regular.Method
                               { class_name = "int"; method_name = "__gt__"; kind = Normal });
                        ]
                      ()) );
               ( "3:5-3:10|artificial-call|normalize-not-comparison>comparison",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"int"
                            ~return_type:(Some ReturnType.bool)
                            (Target.Regular.Method
                               { class_name = "int"; method_name = "__le__"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
     def foo():
       if 1 > 2:
         f = bar
       else:
         f = None
       f()
     def bar(): ...
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "3:5-3:10|artificial-call|comparison",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"int"
                            ~return_type:(Some ReturnType.bool)
                            (Target.Regular.Method
                               { class_name = "int"; method_name = "__gt__"; kind = Normal });
                        ]
                      ()) );
               ( "3:5-3:10|artificial-call|normalize-not-comparison>comparison",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"int"
                            ~return_type:(Some ReturnType.bool)
                            (Target.Regular.Method
                               { class_name = "int"; method_name = "__le__"; kind = Normal });
                        ]
                      ()) );
               ( "4:8-4:11|artificial-attribute-access|qualification:test.bar",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
                             ]
                           ())
                      ()) );
               ( "7:2-7:5",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.bar"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:
             {|
     from typing import Optional

     def foo(c: Optional[C]):
       c.m()
     class C:
       def m():
         ...

      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "5:2-5:7",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create
                            ~implicit_receiver:true
                            ~receiver_class:"test.C"
                            (Target.create_method !&"test.C" "m");
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:
             {|
     from typing import Optional

     def foo(c: C):
       c.m()
     class C:
       def m():
         ...
     class D(C):
       def m():
         ...
     class E(D):
       def m():
         ...
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "5:2-5:7",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create
                            ~implicit_receiver:true
                            ~receiver_class:"test.C"
                            (Target.create_override !&"test.C" "m");
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:
             {|
     from typing import Optional

     def foo(d: D):
       d.m()
     class C:
       def m():
         ...
     class D(C):
       pass
     class E(D):
       def m():
         ...
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "5:2-5:7",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create
                            ~implicit_receiver:true
                            ~receiver_class:"test.D"
                            (Target.create_method !&"test.C" "m");
                          CallTarget.create
                            ~implicit_receiver:true
                            ~receiver_class:"test.D"
                            (Target.create_method !&"test.E" "m");
                        ]
                      ()) );
             ]
           ~pyrefly_expected:
             [
               ( "5:2-5:7",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create
                            ~implicit_receiver:true
                            ~receiver_class:"test.D"
                            (Target.create_override !&"test.C" "m");
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:
             {|
    class C:
      def __call__(self, a: int): ...
    def foo(c: C):
       c(1)
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "5:3-5:7",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create
                            ~implicit_receiver:true
                            ~implicit_dunder_call:true
                            ~receiver_class:"test.C"
                            (Target.create_method !&"test.C" "__call__");
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:
             {|
    class C:
      @staticmethod
      def __call__(a: int) -> bool: ...
    def foo(c: C):
       c(1)
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "6:3-6:7",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create
                            ~implicit_dunder_call:true
                            ~return_type:(Some ReturnType.bool)
                            ~is_static_method:true
                            (Target.create_method !&"test.C" "__call__");
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:
             {|
    class C:
      def __call__(self, a: int) -> bool: ...
    def foo(c: C):
       c.__call__(1)
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "5:3-5:16",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.bool)
                            ~receiver_class:"test.C"
                            (Target.create_method !&"test.C" "__call__");
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:
             {|
    from typing import Protocol
    class C(Protocol):
      def __call__(self, a: int) -> bool: ...
    def foo(c: C):
       c(1)
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "6:3-6:7",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create
                            ~implicit_receiver:true
                            ~implicit_dunder_call:true
                            ~return_type:(Some ReturnType.bool)
                            ~receiver_class:"test.C"
                            (Target.create_method !&"test.C" "__call__");
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:true (* difference: receiver class on __init__ *)
           ~_migrated_to_pyrefly:true
           ~source:
             {|
       class C:
         def __init__(self, a): ...
       def foo():
         C()
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "5:2-5:5",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.unknown)
                            (Target.Regular.Method
                               { class_name = "test.C"; method_name = "__init__"; kind = Normal });
                        ]
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.unknown)
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__new__"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:{|
       def foo(x: str) -> int:
         return int(x)
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "3:9-3:15",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__init__"; kind = Normal });
                        ]
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.integer)
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "int"; method_name = "__new__"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:
             {|
       class C:
         def __new__(cls, a): ...
       def foo():
         C()
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "5:2-5:5",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.unknown)
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__init__"; kind = Normal });
                        ]
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.unknown)
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "test.C"; method_name = "__new__"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:true (* difference: receiver_class on __init__ *)
           ~_migrated_to_pyrefly:true
           ~source:
             {|
       from unknown import A
       class B(A):
         def __init__(self, a): ...
       def foo():
         B()
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "6:2-6:5",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.unknown)
                            (Target.Regular.Method
                               { class_name = "test.B"; method_name = "__init__"; kind = Normal });
                        ]
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.unknown)
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__new__"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:
             {|
       from unknown import A
       class B(A):
         def __new__(cls, a): ...
       def foo():
         B()
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "6:2-6:5",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.unknown)
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__init__"; kind = Normal });
                        ]
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.unknown)
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "test.B"; method_name = "__new__"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
        class C:
          @property
          def p(self) -> int: ...
          @p.setter
          def p(self, v: int) -> None: ...
        def foo(c: C):
          c.p = c.p
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "8:2-8:5",
                 ExpressionCallees.from_attribute_access
                   {
                     AttributeAccessCallees.property_targets =
                       [
                         CallTarget.create_regular
                           ~implicit_receiver:true
                           ~return_type:(Some ReturnType.none)
                           (Target.Regular.Method
                              {
                                class_name = "test.C";
                                method_name = "p";
                                kind = Pyre1PropertySetter;
                              });
                       ];
                     global_targets = [];
                     is_attribute = false;
                     if_called = CallCallees.empty;
                   } );
               ( "8:8-8:11",
                 ExpressionCallees.from_attribute_access
                   {
                     AttributeAccessCallees.property_targets =
                       [
                         CallTarget.create_regular
                           ~implicit_receiver:true
                           ~return_type:(Some ReturnType.integer)
                           (Target.Regular.Method
                              { class_name = "test.C"; method_name = "p"; kind = Normal });
                       ];
                     global_targets = [];
                     is_attribute = false;
                     if_called = CallCallees.empty;
                   } );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
        class C:
          @staticmethod
          def f(a: int) -> int: ...
        def foo():
          C.f()
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "6:2-6:7",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.integer)
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "test.C"; method_name = "f"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:
             {|
        class C:
          @classmethod
          def f(cls, a: int) -> int: ...
        def foo(c: C):
          C.f()
          c.f()
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "6:2-6:7",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~is_class_method:true
                            ~receiver_class:"test.C"
                            (Target.Regular.Method
                               { class_name = "test.C"; method_name = "f"; kind = Normal });
                        ]
                      ()) );
               ( "7:2-7:7",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~is_class_method:true
                            ~index:1
                            ~receiver_class:"test.C"
                            (Target.Regular.Method
                               { class_name = "test.C"; method_name = "f"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
    # Original code: https://fburl.com/code/k6hypgar
    class A:
      def foo(self):
        raise NotImplementedError
    class B(A):
      # The type of B.foo would be different without the if-else here.
      if 1 == 1:
        @classmethod
        def foo(cls) -> None:
          pass
      else:
        @classmethod
        def foo(cls) -> None:
          pass

    def bar():
      B.foo()
  |}
           ~define_name:"test.bar"
           ~expected:
             [
               ( "18:2-18:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular (* TODO: should resolve to test.B *)
                            ~implicit_receiver:false
                            ~is_class_method:false
                            (Target.Regular.Method
                               { class_name = "test.A"; method_name = "foo"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      from abc import abstractmethod
      class A:
        def f(self):
            return self.g()

        @abstractmethod
        def g(self):
            pass

      class B(A):
        def g(self):
            pass
      |}
           ~define_name:"test.A.f"
           ~expected:
             [
               ( "5:13-5:21",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"test.A"
                            (Target.Regular.Override
                               { class_name = "test.A"; method_name = "g"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~skip_for_pyrefly:false
           ~source:{|
        def foo():
          1 > 2
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "3:2-3:7|artificial-call|comparison",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.bool)
                            ~receiver_class:"int"
                            (Target.Regular.Method
                               { class_name = "int"; method_name = "__gt__"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~skip_for_pyrefly:false
           ~source:{|
        def foo():
          1 > 2 > 3
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "3:2-3:7|artificial-call|comparison",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.bool)
                            ~receiver_class:"int"
                            (Target.Regular.Method
                               { class_name = "int"; method_name = "__gt__"; kind = Normal });
                        ]
                      ()) );
               ( "3:6-3:11|artificial-call|comparison",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.bool)
                            ~receiver_class:"int"
                            ~index:1
                            (Target.Regular.Method
                               { class_name = "int"; method_name = "__gt__"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      class C:
        def __repr__(self) -> str: ...

      def foo(c: C):
        repr(c)
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "6:2-6:9|artificial-call|repr-call",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"test.C"
                            (Target.Regular.Method
                               { class_name = "test.C"; method_name = "__repr__"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
      from functools import partial
      def f(a, b):
        ...

      def foo():
        partial(f, 1)
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "7:10-7:11|artificial-attribute-access|qualification:test.f",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.f"; kind = Normal });
                             ]
                           ())
                      ()) );
               ( "7:2-7:15",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               {
                                 class_name = "functools.partial";
                                 method_name = "__init__";
                                 kind = Normal;
                               });
                        ]
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__new__"; kind = Normal });
                        ]
                      ~shim_target:
                        (Some
                           {
                             ShimTarget.call_targets =
                               [
                                 CallTarget.create_regular
                                   (Target.Regular.Function { name = "test.f"; kind = Normal });
                               ];
                             decorated_targets = [];
                             argument_mapping =
                               {
                                 ShimArgumentMapping.identifier = "functools.partial";
                                 callee = ShimArgumentMapping.Target.Argument { index = 0 };
                                 arguments =
                                   [
                                     {
                                       ShimArgumentMapping.Argument.name = None;
                                       value = ShimArgumentMapping.Target.Argument { index = 1 };
                                     };
                                   ];
                               };
                           })
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
      from builtins import to_callable_target

      @to_callable_target
      def callable_target(arg):
        pass

      def foo():
        callable_target.async_schedule(1)
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "9:2-9:17|artificial-attribute-access|qualification:test.callable_target",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 ~return_type:(Some ReturnType.integer)
                                 (Target.Regular.Function
                                    { name = "test.callable_target"; kind = Decorated });
                             ]
                           ())
                      ()) );
               ( "9:2-9:35",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~shim_target:
                        (Some
                           {
                             ShimTarget.call_targets =
                               [
                                 CallTarget.create_regular
                                   (Target.Regular.Function
                                      { name = "test.callable_target"; kind = Decorated });
                               ];
                             decorated_targets = [];
                             argument_mapping =
                               {
                                 ShimArgumentMapping.identifier = "async_task";
                                 callee =
                                   ShimArgumentMapping.Target.GetAttributeBase
                                     {
                                       inner = ShimArgumentMapping.Target.Callee;
                                       attribute = "async_schedule";
                                     };
                                 arguments =
                                   [
                                     {
                                       ShimArgumentMapping.Argument.name = None;
                                       value = ShimArgumentMapping.Target.Argument { index = 0 };
                                     };
                                   ];
                               };
                           })
                      ~unresolved:
                        (Unresolved.True
                           (Unresolved.BypassingDecorators Unresolved.CannotResolveExports))
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
      from builtins import to_callable_target

      class Foo:
        @to_callable_target
        def callable_target(arg):
          pass

      def bar(foo: Foo):
        foo.callable_target(1)
      |}
           ~define_name:"test.bar"
           ~expected:
             [
               ( "10:2-10:24",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~implicit_dunder_call:true
                            ~return_type:(Some ReturnType.integer)
                            ~receiver_class:"TestCallableTarget"
                            (Target.Regular.Method
                               {
                                 class_name = "TestCallableTarget";
                                 method_name = "__call__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:{|
      def foo(x=bar()):
        pass

      def bar():
        pass
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "2:10-2:15",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.bar"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:true (* difference: receiver class on super.__init__ *)
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      class C:
        def f(self, x: int) -> int:
          return x

      class D(C):
        def f(self, x: int) -> int:
          return x

        def g(self) -> None:
          super().f(1)
      |}
           ~define_name:"test.D.g"
           ~expected:
             [
               ( "11:4-11:11",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__new__"; kind = Normal });
                        ]
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               { class_name = "super"; method_name = "__init__"; kind = Normal });
                        ]
                      ()) );
               ( "11:4-11:16",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~receiver_class:"test.C"
                            (Target.Regular.Method
                               { class_name = "test.C"; method_name = "f"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      class C:
        def f(self, x: int) -> int:
          return x

      class D(C):
        def f(self, x: int) -> int:
          return x

      def foo(c: C):
        C.f(c, 1)
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "11:2-11:11",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.integer)
                            (Target.Regular.Method
                               { class_name = "test.C"; method_name = "f"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      class C:
        @classmethod
        def f(cls, x: int) -> int:
          return x
        @classmethod
        def g(cls):
          pass

      class D(C):
        @classmethod
        def f(cls, x: int) -> int:
          return x

      def foo(c: C):
        C.f(c, 1)
        D.f()
        D.g()
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "16:2-16:11",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~is_class_method:true
                            ~receiver_class:"test.C"
                            (Target.Regular.Method
                               { class_name = "test.C"; method_name = "f"; kind = Normal });
                        ]
                      ()) );
               ( "17:2-17:7",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~is_class_method:true
                            ~receiver_class:"test.D"
                            (Target.Regular.Method
                               { class_name = "test.D"; method_name = "f"; kind = Normal });
                        ]
                      ()) );
               ( "18:2-18:7",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~is_class_method:true
                            ~receiver_class:"test.D"
                            (Target.Regular.Method
                               { class_name = "test.C"; method_name = "g"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      def hof(f, arg) -> bool:
        f(arg)

      def bar(x) -> int:
        pass

      def foo():
        hof(bar, 1)
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "9:2-9:13",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.bool)
                            (Target.Regular.Function { name = "test.hof"; kind = Normal });
                        ]
                      ~higher_order_parameters:
                        (HigherOrderParameterMap.from_list
                           [
                             {
                               index = 0;
                               call_targets =
                                 [
                                   CallTarget.create_regular
                                     ~return_type:(Some ReturnType.integer)
                                     (Target.Regular.Function { name = "test.bar"; kind = Normal });
                                 ];
                               unresolved = CallGraph.Unresolved.False;
                             };
                           ])
                      ()) );
               ( "9:6-9:9|artificial-attribute-access|qualification:test.bar",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 ~return_type:(Some ReturnType.integer)
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ~pyrefly_expected:
             [
               ( "9:2-9:13",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.bool)
                            (Target.Regular.Function { name = "test.hof"; kind = Normal });
                        ]
                      ~higher_order_parameters:
                        (HigherOrderParameterMap.from_list
                           [
                             {
                               index = 0;
                               call_targets =
                                 [
                                   CallTarget.create_regular
                                     ~return_type:(Some ReturnType.integer)
                                     (Target.Regular.Function { name = "test.bar"; kind = Normal });
                                 ];
                               unresolved = CallGraph.Unresolved.False;
                             };
                           ])
                      ()) );
               ( "9:6-9:9",
                 ExpressionCallees.from_identifier
                   (IdentifierCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 ~return_type:(Some ReturnType.integer)
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      def hof(f, g, arg) -> bool:
        f(arg)
        g(arg)

      def foo(x) -> int:
        pass

      def bar(x) -> int:
        pass

      def test():
        hof(foo, bar, 1)
      |}
           ~define_name:"test.test"
           ~expected:
             [
               ( "13:2-13:18",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.bool)
                            (Target.Regular.Function { name = "test.hof"; kind = Normal });
                        ]
                      ~higher_order_parameters:
                        (HigherOrderParameterMap.from_list
                           [
                             {
                               index = 0;
                               call_targets =
                                 [
                                   CallTarget.create_regular
                                     ~return_type:(Some ReturnType.integer)
                                     (Target.Regular.Function { name = "test.foo"; kind = Normal });
                                 ];
                               unresolved = CallGraph.Unresolved.False;
                             };
                             {
                               index = 1;
                               call_targets =
                                 [
                                   CallTarget.create_regular
                                     ~return_type:(Some ReturnType.integer)
                                     (Target.Regular.Function { name = "test.bar"; kind = Normal });
                                 ];
                               unresolved = CallGraph.Unresolved.False;
                             };
                           ])
                      ()) );
               ( "13:6-13:9|artificial-attribute-access|qualification:test.foo",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 ~return_type:(Some ReturnType.integer)
                                 (Target.Regular.Function { name = "test.foo"; kind = Normal });
                             ]
                           ())
                      ()) );
               ( "13:11-13:14|artificial-attribute-access|qualification:test.bar",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 ~return_type:(Some ReturnType.integer)
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ~pyrefly_expected:
             [
               ( "13:2-13:18",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.bool)
                            (Target.Regular.Function { name = "test.hof"; kind = Normal });
                        ]
                      ~higher_order_parameters:
                        (HigherOrderParameterMap.from_list
                           [
                             {
                               index = 0;
                               call_targets =
                                 [
                                   CallTarget.create_regular
                                     ~return_type:(Some ReturnType.integer)
                                     (Target.Regular.Function { name = "test.foo"; kind = Normal });
                                 ];
                               unresolved = CallGraph.Unresolved.False;
                             };
                             {
                               index = 1;
                               call_targets =
                                 [
                                   CallTarget.create_regular
                                     ~return_type:(Some ReturnType.integer)
                                     (Target.Regular.Function { name = "test.bar"; kind = Normal });
                                 ];
                               unresolved = CallGraph.Unresolved.False;
                             };
                           ])
                      ()) );
               ( "13:6-13:9",
                 ExpressionCallees.from_identifier
                   (IdentifierCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 ~return_type:(Some ReturnType.integer)
                                 (Target.Regular.Function { name = "test.foo"; kind = Normal });
                             ]
                           ())
                      ()) );
               ( "13:11-13:14",
                 ExpressionCallees.from_identifier
                   (IdentifierCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 ~return_type:(Some ReturnType.integer)
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      _magic_enum = property

      class Enum:
        @_magic_enum
        def value(self): ...

      class Permission(Enum):
        @property
        def action_name(self) -> bool:
          if len(self.value):
              return True
          return False
      |}
           ~define_name:"test.Permission.action_name"
           ~expected:
             [
               ( "11:7-11:22",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.integer)
                            (Target.Regular.Function { name = "len"; kind = Normal });
                        ]
                      ~higher_order_parameters:
                        (HigherOrderParameterMap.from_list
                           [
                             {
                               index = 0;
                               call_targets =
                                 [
                                   CallTarget.create_regular
                                     ~implicit_receiver:true
                                     (Target.Regular.Method
                                        {
                                          class_name = "test.Enum";
                                          method_name = "value";
                                          kind = Normal;
                                        });
                                 ];
                               unresolved = CallGraph.Unresolved.False;
                             };
                           ])
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:{|
      def test():
        return map(lambda x: x, [0])
      |}
           ~define_name:"test.test"
           ~expected:
             [
               ( "3:9-3:30",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__new__"; kind = Normal });
                        ]
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               { class_name = "map"; method_name = "__init__"; kind = Normal });
                        ]
                      ~higher_order_parameters:
                        (HigherOrderParameterMap.from_list
                           [
                             {
                               index = 0;
                               call_targets = [];
                               unresolved = CallGraph.Unresolved.True LambdaArgument;
                             };
                           ])
                      ()) );
             ]
           ~pyrefly_expected:
             [
               ( "3:9-3:30",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "map"; method_name = "__new__"; kind = Normal });
                        ]
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__init__"; kind = Normal });
                        ]
                      ~higher_order_parameters:
                        (HigherOrderParameterMap.from_list
                           [
                             {
                               index = 0;
                               call_targets = [];
                               unresolved = CallGraph.Unresolved.True LambdaArgument;
                             };
                           ])
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
        class Builder:
            def __init__(self) -> None:
                self._saved: Optional[str] = None
                self._not_saved: Optional[str] = None

            def set_saved(self, saved: str) -> "Builder":
                self._saved = saved
                return self

            def set_not_saved(self, not_saved: str) -> "Builder":
                self._not_saved = not_saved
                return self

        def foo():
            builder = Builder()
            builder.set_not_saved("true").set_saved("false")
   |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "16:14-16:23",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__new__"; kind = Normal });
                        ]
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               {
                                 class_name = "test.Builder";
                                 method_name = "__init__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "17:4-17:33",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"test.Builder"
                            (Target.Regular.Method
                               {
                                 class_name = "test.Builder";
                                 method_name = "set_not_saved";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "17:4-17:52",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"test.Builder"
                            (Target.Regular.Method
                               {
                                 class_name = "test.Builder";
                                 method_name = "set_saved";
                                 kind = Normal;
                               });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
      from functools import lru_cache
      @lru_cache()
      def f() -> int:
        return 0

      def foo():
        f()
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "8:2-8:5",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.integer)
                            (Target.Regular.Function { name = "test.f"; kind = Normal });
                        ]
                      ~recognized_call:CallGraph.CallCallees.RecognizedCall.True
                      ()) );
             ]
           ();
      (* Imprecise call graph due to `@lru_cache` and inner functions. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
      from functools import lru_cache
      class C:
        @lru_cache()
        def m(self, x: int) -> int:
          return x

      def foo(c: C):
        c.m(1)
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "9:2-9:8",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~receiver_class:"test.C"
                            (Target.Regular.Method
                               { class_name = "test.C"; method_name = "m"; kind = Normal });
                        ]
                      ~recognized_call:CallGraph.CallCallees.RecognizedCall.True
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:false
           ~source:
             {|
      from functools import lru_cache

      def foo():
        @lru_cache()
        def inner() -> int:
          return 0

        inner()
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "5:2-7:12",
                 ExpressionCallees.from_define
                   (DefineCallees.create
                      ~define_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.foo.inner"; kind = Normal });
                        ]
                      ()) );
               ( "9:2-9:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.integer)
                            ~implicit_receiver:true
                            ~implicit_dunder_call:true
                            ~receiver_class:"functools._lru_cache_wrapper"
                            (Target.Regular.Method
                               {
                                 class_name = "functools._lru_cache_wrapper";
                                 method_name = "__call__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
             ]
           ~pyrefly_expected:
             [
               ( "5:2-7:12",
                 ExpressionCallees.from_define
                   (DefineCallees.create
                      ~define_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.none)
                            (Target.Regular.Function { name = "test.foo.inner"; kind = Decorated });
                        ]
                      ()) );
               ( "9:2-9:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.integer)
                            (Target.Regular.Function { name = "test.foo.inner"; kind = Decorated });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
    class C:
        def run(self) -> str:
            return ""

    def foo() -> None:
        cs: List[C] = [C()]
        result = [c.run() for c in cs]
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "7:19-7:22",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__new__"; kind = Normal });
                        ]
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__init__"; kind = Normal });
                        ]
                      ()) );
               ( "8:14-8:21",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"test.C"
                            (Target.Regular.Method
                               { class_name = "test.C"; method_name = "run"; kind = Normal });
                        ]
                      ()) );
               ( "8:31-8:33|artificial-call|generator-iter",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"list"
                            (Target.Regular.Method
                               { class_name = "list"; method_name = "__iter__"; kind = Normal });
                        ]
                      ()) );
               ( "8:31-8:33|artificial-call|generator-next",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"typing.Iterator"
                            (Target.Regular.Method
                               {
                                 class_name = "typing.Iterator";
                                 method_name = "__next__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
             ]
           ();
      (* Ensure we don't infinite loop when resolving callable classes. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
    from typing import Any, Target
    def to_c(callable: Target[..., Any]) -> C:
      ...

    class C:
      @to_c
      def __call__(self) -> "C":
        return self

    def foo(c: C) -> None:
      c()
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "12:2-12:5",
                 ExpressionCallees.from_call
                   (CallCallees.unresolved
                      ~reason:(CallGraph.Unresolved.BypassingDecorators UnknownIdentifierCallee)
                      ~message:(lazy "")
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~cmp:DefineCallGraphForTest.equal_ignoring_types
           ~source:
             {|
        from contextlib import ContextManager
        def foo():
          with to_cm() as my_int:
            pass
        def to_cm() -> ContextManager[int]: ...
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "4:7-4:14|artificial-call|with-enter",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~receiver_class:"contextlib.ContextManager"
                            (Target.Regular.Method
                               {
                                 class_name = "contextlib.ContextManager";
                                 method_name = "__enter__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "4:7-4:14",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.to_cm"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      (* Only the last attribute is a setter for chained property setter calls. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
        class C:
          @property
          def p(self) -> "C":
            ...
          @p.setter
          def p(self, new_value: "C") -> None:
            ...

        def foo(c: C):
          c.p.p = c
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "11:2-11:5",
                 ExpressionCallees.from_attribute_access
                   {
                     AttributeAccessCallees.property_targets =
                       [
                         CallTarget.create_regular
                           ~implicit_receiver:true
                           ~return_type:(Some ReturnType.unknown)
                           (Target.Regular.Method
                              { class_name = "test.C"; method_name = "p"; kind = Normal });
                       ];
                     global_targets = [];
                     is_attribute = false;
                     if_called = CallCallees.empty;
                   } );
               ( "11:2-11:7",
                 ExpressionCallees.from_attribute_access
                   {
                     AttributeAccessCallees.property_targets =
                       [
                         CallTarget.create_regular
                           ~implicit_receiver:true
                           ~return_type:(Some ReturnType.none)
                           (Target.Regular.Method
                              {
                                class_name = "test.C";
                                method_name = "p";
                                kind = Pyre1PropertySetter;
                              });
                       ];
                     global_targets = [];
                     is_attribute = false;
                     if_called = CallCallees.empty;
                   } );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
        from typing import Protocol
        class C(Protocol):
          def f(self) -> int: ...

        def foo(c: C):
          c.f()
          C.f(c)
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "7:2-7:7",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~receiver_class:"test.C"
                            (Target.Regular.Method
                               { class_name = "test.C"; method_name = "f"; kind = Normal });
                        ]
                      ()) );
               ( "8:2-8:8",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~index:1
                            ~return_type:(Some ReturnType.integer)
                            (Target.Regular.Method
                               { class_name = "test.C"; method_name = "f"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      class C:
        @property
        def foo(self) -> int:
          ...

      class D:
        @property
        def foo(self) -> bool:
          ...

      class E:
        foo: int = 1

      def uses_foo(c_or_d: C | D, c_or_e: C | E):
        x = c_or_d.foo
        y = c_or_e.foo
    |}
           ~define_name:"test.uses_foo"
           ~expected:
             [
               ( "16:6-16:16",
                 ExpressionCallees.from_attribute_access
                   {
                     AttributeAccessCallees.property_targets =
                       [
                         CallTarget.create_regular
                           ~implicit_receiver:true
                           ~return_type:(Some ReturnType.integer)
                           (Target.Regular.Method
                              { class_name = "test.C"; method_name = "foo"; kind = Normal });
                         CallTarget.create_regular
                           ~implicit_receiver:true
                           ~return_type:(Some ReturnType.bool)
                           (Target.Regular.Method
                              { class_name = "test.D"; method_name = "foo"; kind = Normal });
                       ];
                     global_targets = [];
                     is_attribute = false;
                     if_called = CallCallees.empty;
                   } );
               ( "17:6-17:16",
                 ExpressionCallees.from_attribute_access
                   {
                     AttributeAccessCallees.property_targets =
                       [
                         CallTarget.create_regular
                           ~implicit_receiver:true
                           ~index:1
                           ~return_type:(Some ReturnType.integer)
                           (Target.Regular.Method
                              { class_name = "test.C"; method_name = "foo"; kind = Normal });
                       ];
                     global_targets = [];
                     is_attribute = true;
                     if_called = CallCallees.empty;
                   } );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      from typing import TypeVar
      class C:
        @property
        def foo(self) -> int:
          ...

      class D:
        @property
        def foo(self) -> int:
          ...

      TCOrD = TypeVar("TCOrD", C, D)
      def uses_foo(c_or_d: TCOrD):
        x = c_or_d.foo
    |}
           ~define_name:"test.uses_foo"
           ~expected:
             [
               ( "15:6-15:16",
                 ExpressionCallees.from_attribute_access
                   {
                     AttributeAccessCallees.property_targets =
                       [
                         CallTarget.create_regular
                           ~implicit_receiver:true
                           ~return_type:(Some ReturnType.integer)
                           (Target.Regular.Method
                              { class_name = "test.C"; method_name = "foo"; kind = Normal });
                         CallTarget.create_regular
                           ~implicit_receiver:true
                           ~return_type:(Some ReturnType.integer)
                           (Target.Regular.Method
                              { class_name = "test.D"; method_name = "foo"; kind = Normal });
                       ];
                     global_targets = [];
                     is_attribute = false;
                     if_called = CallCallees.empty;
                   } );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
        class C:
          @classmethod
          def foo(cls):
            pass
        d = {
          "a": C,
          "b": C,
        }
        def calls_d_method(s: str):
          d[s].foo()
      |}
           ~define_name:"test.calls_d_method"
           ~expected:
             [
               ( "11:2-11:3|identifier|$local_test$d",
                 ExpressionCallees.from_identifier
                   (IdentifierCallees.create
                      ~global_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:None
                            (Target.Regular.Object "test.d");
                        ]
                      ()) );
               ( "11:2-11:6|artificial-call|subscript-get-item",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"dict"
                            (Target.Regular.Method
                               { class_name = "dict"; method_name = "__getitem__"; kind = Normal });
                        ]
                      ()) );
               ( "11:2-11:12",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~is_class_method:true
                            ~receiver_class:"test.C"
                            (Target.Regular.Method
                               { class_name = "test.C"; method_name = "foo"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
       import typing
       def foo() -> typing.Dict[str, int]:
         return {"a": 0}
       def bar():
         return 1
       def baz():
         return "b"
       def fun(d: typing.Dict[str, int], e: typing.Dict[str, typing.Dict[str, int]]):
         foo()["a"] = bar()
         d[baz()] = bar()
         e["a"]["b"] = 0
      |}
           ~define_name:"test.fun"
           ~expected:
             [
               ( "10:2-10:7",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.foo"; kind = Normal });
                        ]
                      ()) );
               ( "10:2-10:20|artificial-call|subscript-set-item",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"dict"
                            (Target.Regular.Method
                               { class_name = "dict"; method_name = "__setitem__"; kind = Normal });
                        ]
                      ()) );
               ( "10:15-10:20",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.bar"; kind = Normal });
                        ]
                      ()) );
               ( "11:2-11:18|artificial-call|subscript-set-item",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"dict"
                            ~index:1
                            (Target.Regular.Method
                               { class_name = "dict"; method_name = "__setitem__"; kind = Normal });
                        ]
                      ()) );
               ( "11:4-11:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.baz"; kind = Normal });
                        ]
                      ()) );
               ( "11:13-11:18",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~index:1
                            (Target.Regular.Function { name = "test.bar"; kind = Normal });
                        ]
                      ()) );
               ( "12:2-12:8|artificial-call|subscript-get-item",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"dict"
                            (Target.Regular.Method
                               { class_name = "dict"; method_name = "__getitem__"; kind = Normal });
                        ]
                      ()) );
               ( "12:2-12:17|artificial-call|subscript-set-item",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"dict"
                            ~index:2
                            (Target.Regular.Method
                               { class_name = "dict"; method_name = "__setitem__"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      def outer(x: int) -> None:
        def inner(x: int) -> None:
          print(x)

        inner(x)
  |}
           ~define_name:"test.outer"
           ~expected:
             [
               ( "3:2-4:12",
                 ExpressionCallees.from_define
                   (DefineCallees.create
                      ~define_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.outer.inner"; kind = Normal });
                        ]
                      ()) );
               ( "6:2-6:10",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.outer.inner"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~skip_for_pyrefly:false
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      class Foo:
        def outer(self, x: int) -> None:
          def inner(x: int) -> None:
            pass

          inner(x)
  |}
           ~define_name:"test.Foo.outer"
           ~expected:
             [
               ( "4:4-5:10",
                 ExpressionCallees.from_define
                   (DefineCallees.create
                      ~define_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function
                               { name = "test.Foo.outer.inner"; kind = Normal });
                        ]
                      ()) );
               ( "7:4-7:12",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function
                               { name = "test.Foo.outer.inner"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
     class C:
       def m(self) -> str:
         return "world"

     def foo(c: C) -> str:
       return f"hello {c.m()}"
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "7:9-7:25|format-string-artificial",
                 ExpressionCallees.from_format_string_artificial
                   (FormatStringArtificialCallees.from_f_string_targets
                      [CallTarget.create ~return_type:None Target.ArtificialTargets.format_string])
               );
               ( "7:18-7:23|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~receiver_class:"str"
                          (Target.Regular.Method
                             { class_name = "str"; method_name = "__str__"; kind = Normal });
                      ]) );
               ( "7:18-7:23",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"test.C"
                            (Target.Regular.Method
                               { class_name = "test.C"; method_name = "m"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
     class C:
       @property
       def attribute(self) -> Callable[[], int]:
         return lambda: 0

     def foo(c: C) -> str:
       return c.attribute()
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "8:9-8:22",
                 ExpressionCallees.from_call
                   (CallCallees.unresolved
                      ~reason:(CallGraph.Unresolved.BypassingDecorators CannotFindParentClass)
                      ~message:(lazy "")
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
     def foo() -> None:
       pass
     def bar() -> None:
       pass
     def test(x) -> str:
       try:
         return foo()
       finally:
         bar(x)
      |}
           ~define_name:"test.test"
           ~expected:
             [
               ( "8:11-8:16",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.foo"; kind = Normal });
                        ]
                      ()) );
               ( "10:4-10:10",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.bar"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
     def foo() -> None:
       pass
     def bar() -> None:
       pass
     def test(x) -> str:
       try:
         raise Exception()
       finally:
         bar(x)
      |}
           ~define_name:"test.test"
           ~expected:
             [
               ( "8:10-8:21",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__new__"; kind = Normal });
                        ]
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               {
                                 class_name = "BaseException";
                                 method_name = "__init__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "10:4-10:10",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.bar"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      (* TODO(T105570363): Resolve calls with mixed function and methods. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~cmp:DefineCallGraphForTest.equal_ignoring_types
           ~source:
             {|
      class Foo:
        def bar(self) -> None:
          pass

      def baz(self) -> None:
        pass

      def f(foo: Foo):
        for g in [foo.bar, baz]:
          g()
  |}
           ~define_name:"test.f"
           ~expected:
             [
               ( "10:11-10:25|artificial-call|for-iter",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"list"
                            (Target.Regular.Method
                               { class_name = "list"; method_name = "__iter__"; kind = Normal });
                        ]
                      ()) );
               ( "10:11-10:25|artificial-call|for-next",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"typing.Iterator"
                            (Target.Regular.Method
                               {
                                 class_name = "typing.Iterator";
                                 method_name = "__next__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "10:21-10:24|artificial-attribute-access|qualification:test.baz",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.baz"; kind = Normal });
                             ]
                           ())
                      ()) );
               ( "11:4-11:7",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.baz"; kind = Normal });
                        ]
                      ~unresolved:(CallGraph.Unresolved.True AnonymousCallableType)
                      ()) );
             ]
           ();
      (* TODO(T105570363): Resolve calls with mixed function and constructors. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~cmp:DefineCallGraphForTest.equal_ignoring_types
           ~source:
             {|
      class Foo:
        pass

      def bar(self) -> None:
        pass

      def f():
        for g in [Foo, bar]:
          g()
  |}
           ~define_name:"test.f"
           ~expected:
             [
               ( "9:11-9:21|artificial-call|for-iter",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~receiver_class:"list"
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               { class_name = "list"; method_name = "__iter__"; kind = Normal });
                        ]
                      ()) );
               ( "9:11-9:21|artificial-call|for-next",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"typing.Iterator"
                            (Target.Regular.Method
                               {
                                 class_name = "typing.Iterator";
                                 method_name = "__next__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "9:17-9:20|artificial-attribute-access|qualification:test.bar",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
                             ]
                           ())
                      ()) );
               ( "10:4-10:7",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.bar"; kind = Normal });
                        ]
                      ~unresolved:(CallGraph.Unresolved.True AnonymousCallableType)
                      ()) );
             ]
           ();
      (* Well-typed decorators are 'safely' ignored (when not inlined). *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      from typing import Callable, TypeVar
      from pyre_extensions import ParameterSpecification

      _T = TypeVar("_T")
      _TParams = ParameterSpecification("_TParams")

      class Timer:
        def __call__(self, func: Callable[_TParams, _T]) -> Callable[_TParams, _T]:
          return func

      def timer(name: str) -> Timer:
        return Timer()

      @timer("bar")
      def foo(x: int) -> int:
        return x

      def caller() -> None:
        foo(1)
    |}
           ~define_name:"test.caller"
           ~expected:
             [
               ( "20:2-20:8",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.integer)
                            (Target.Regular.Function { name = "test.foo"; kind = Decorated });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      from typing import Callable, TypeVar
      from pyre_extensions import ParameterSpecification

      _T = TypeVar("_T")
      _TParams = ParameterSpecification("_TParams")

      class Timer:
        def __call__(self, func: Callable[_TParams, _T]) -> Callable[_TParams, _T]:
          return func

      def timer(name: str) -> Timer:
        return Timer()

      class Foo:
        @timer("bar")
        def bar(self, x: int) -> int:
          return x

      def caller(foo: Foo) -> None:
        foo.bar(1)
    |}
           ~define_name:"test.caller"
           ~expected:
             [
               ( "21:2-21:12",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~receiver_class:"test.Foo"
                            (Target.Regular.Method
                               { class_name = "test.Foo"; method_name = "bar"; kind = Decorated });
                        ]
                      ()) );
             ]
           ();
      (* Partially-typed decorators are 'safely' ignored (when not inlined). *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      from typing import Callable, TypeVar
      _T = TypeVar("_T")

      class Timer:
        def __call__(self, func: Callable[..., _T]) -> Callable[..., _T]:
          return func

      def timer(name: str) -> Timer:
        return Timer()

      @timer("bar")
      def foo(x: int) -> int:
        return x

      def caller() -> None:
        foo(1)
    |}
           ~define_name:"test.caller"
           ~expected:
             [
               ( "17:2-17:8",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.integer)
                            (Target.Regular.Function { name = "test.foo"; kind = Decorated });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      from typing import Callable, TypeVar
      _T = TypeVar("_T")

      class Timer:
        def __call__(self, func: Callable[..., _T]) -> Callable[..., _T]:
          return func

      def timer(name: str) -> Timer:
        return Timer()

      class Foo:
        @timer("bar")
        def bar(self, x: int) -> int:
          return x

      def caller(foo: Foo) -> None:
        foo.bar(1)
    |}
           ~define_name:"test.caller"
           ~expected:
             [
               ( "18:2-18:12",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            (Target.Regular.Method
                               { class_name = "test.Foo"; method_name = "bar"; kind = Decorated });
                        ]
                      ()) );
             ]
           ();
      (* Untyped decorators are 'safely' ignored (when not inlined). *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      def timer(name: str):
        pass

      @timer("bar")
      def foo(x: int) -> int:
        return x

      def caller() -> None:
        foo(1)
    |}
           ~define_name:"test.caller"
           ~expected:
             [
               ( "10:2-10:8",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.foo"; kind = Decorated });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      def timer(name: str):
        pass

      class Foo:
        @timer("bar")
        def bar(self, x: int) -> int:
          return x

      def caller(foo: Foo) -> None:
        foo.bar(1)
    |}
           ~define_name:"test.caller"
           ~expected:
             [
               ( "11:2-11:12",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               { class_name = "test.Foo"; method_name = "bar"; kind = Decorated });
                        ]
                      ()) );
             ]
           ();
      (* Well-typed decorators with @classmethod or @staticmethod. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      from typing import Callable, TypeVar
      from pyre_extensions import ParameterSpecification

      _T = TypeVar("_T")
      _TParams = ParameterSpecification("_TParams")

      class Timer:
        def __call__(self, func: Callable[_TParams, _T]) -> Callable[_TParams, _T]:
          return func

      def timer(name: str) -> Timer:
        return Timer()

      class Foo:
        @classmethod
        @timer("bar")
        def bar(cls, x: int) -> int:
          return x

      def caller() -> None:
        Foo.bar(1)
    |}
           ~define_name:"test.caller"
           ~expected:
             [
               ( "22:2-22:12",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~is_class_method:true
                            ~receiver_class:"test.Foo"
                            (Target.Regular.Method
                               { class_name = "test.Foo"; method_name = "bar"; kind = Decorated });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      from typing import Callable, TypeVar
      from pyre_extensions import ParameterSpecification

      _T = TypeVar("_T")
      _TParams = ParameterSpecification("_TParams")

      class Timer:
        def __call__(self, func: Callable[_TParams, _T]) -> Callable[_TParams, _T]:
          return func

      def timer(name: str) -> Timer:
        return Timer()

      class Foo:
        @staticmethod
        @timer("bar")
        def bar(x: int) -> int:
          return x

      def caller() -> None:
        Foo.bar(1)
    |}
           ~define_name:"test.caller"
           ~expected:
             [
               ( "22:2-22:12",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.integer)
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "test.Foo"; method_name = "bar"; kind = Decorated });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      from typing import Callable, TypeVar
      from pyre_extensions import ParameterSpecification

      _T = TypeVar("_T")
      _TParams = ParameterSpecification("_TParams")

      class Timer:
        def __call__(self, func: Callable[_TParams, _T]) -> Callable[_TParams, _T]:
          return func

      def timer(name: str) -> Timer:
        return Timer()

      class Foo:
        @classmethod
        @timer("bar")
        def bar(cls, x: int) -> int:
          return x

        @classmethod
        def caller(cls) -> None:
          cls.bar(1)
    |}
           ~define_name:"test.Foo.caller"
           ~expected:
             [
               ( "23:4-23:14",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~is_class_method:true
                            ~receiver_class:"test.Foo"
                            (Target.Regular.Method
                               { class_name = "test.Foo"; method_name = "bar"; kind = Decorated });
                        ]
                      ()) );
             ]
           ();
      (* Decorators with type errors. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      from typing import Callable, TypeVar
      _T = TypeVar("_T")
      _TParams = ParameterSpecification("_TParams")

      class Timer:
        def __call__(self, func: Callable[_TParams, _T]) -> Callable[_TParams, _T]:
          return func

      def timer(name: str) -> Timer:
        return Timer()

      @timer(1) # Intended type error here.
      def foo(x: int) -> int:
        return x

      def caller() -> None:
        foo(1)
    |}
           ~define_name:"test.caller"
           ~expected:
             [
               ( "18:2-18:8",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.foo"; kind = Decorated });
                        ]
                      ()) );
             ]
           ();
      (* Resolving __call__ via __getattr__ when a union including self type is involved. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      from __future__ import annotations
      from typing import Union

      class CallViaGetattr:
        def __getattr__(self, name: str) -> Union[None, CallViaGetattr]:
          return None

      def baz(x: CallViaGetattr) -> None:
        y = print(x.attribute)
    |}
           ~define_name:"test.baz"
           ~expected:
             [
               ( "10:6-10:24",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "print"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      (* Detecting a __call__ picked up via __getattr__ redirection *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      from __future__ import annotations
      from typing import Union

      class CallableClass:
        def __call__(self) -> None:
          return None

      class CallViaGetattr:
        def __getattr__(self, name: str) -> Union[None, CallableClass]:
          return CallableClass()

      def baz(x: CallViaGetattr) -> None:
        y = print(x.attribute)
    |}
           ~define_name:"test.baz"
           ~expected:
             [
               ( "14:6-14:24",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "print"; kind = Normal });
                        ]
                      ~higher_order_parameters:
                        (HigherOrderParameterMap.from_list
                           [
                             {
                               index = 0;
                               call_targets =
                                 [
                                   CallTarget.create_regular
                                     ~implicit_receiver:true
                                     ~implicit_dunder_call:true
                                     ~receiver_class:"test.CallableClass"
                                     (Target.Regular.Method
                                        {
                                          class_name = "test.CallableClass";
                                          method_name = "__call__";
                                          kind = Normal;
                                        });
                                 ];
                               unresolved = CallGraph.Unresolved.False;
                             };
                           ])
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~object_targets:[Target.Regular.Object "test.Token.token"]
           ~source:
             {|
      class Token:
        token: str = ""

      class Token2:
        token2: str = ""

      def foo(obj: Token, obj2: Token2):
        return obj.token, obj2.token2
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "9:9-9:18",
                 ExpressionCallees.from_attribute_access
                   {
                     AttributeAccessCallees.property_targets = [];
                     global_targets =
                       [
                         CallTarget.create_regular
                           ~return_type:None
                           (Target.Regular.Object "test.Token.token");
                       ];
                     is_attribute = true;
                     if_called = CallCallees.empty;
                   } );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~object_targets:
             [Target.Regular.Object "test.A.attribute"; Target.Regular.Object "test.C.attribute"]
           ~source:
             {|
      from typing import Union

      class A:
        attribute: str = ""

      class B:
        attribute: str = ""

      class C:
        attribute: str = ""

      def foo(obj: Union[A, B, C]):
        return obj.attribute
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "14:9-14:22",
                 ExpressionCallees.from_attribute_access
                   {
                     AttributeAccessCallees.property_targets = [];
                     global_targets =
                       [
                         CallTarget.create_regular
                           ~return_type:None
                           (Target.Regular.Object "test.A.attribute");
                         CallTarget.create_regular
                           ~return_type:None
                           (Target.Regular.Object "test.C.attribute");
                       ];
                     is_attribute = true;
                     if_called = CallCallees.empty;
                   } );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~object_targets:[Target.Regular.Object "test.Token.token"]
           ~source:
             {|
      from typing import Optional

      class Token:
        token: str = ""

      class Request:
        access_token: Optional[Token] = None

      def foo(request: Request):
        return request.access_token.token
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "11:9-11:35",
                 ExpressionCallees.from_attribute_access
                   {
                     AttributeAccessCallees.property_targets = [];
                     global_targets =
                       [
                         CallTarget.create_regular
                           ~return_type:None
                           (Target.Regular.Object "test.Token.token");
                       ];
                     is_attribute = true;
                     if_called = CallCallees.empty;
                   } );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~skip_for_pyrefly:false
           ~object_targets:[Target.Regular.Object "test.Token.token"]
           ~source:
             {|
      class Token:
        token: str = ""

      def foo(obj: Token):
        return getattr(obj, "token", None)
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "6:9-6:36",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "getattr"; kind = Normal });
                        ]
                      ()) );
               ( "6:9-6:36|artificial-attribute-access|get-attr-constant-literal",
                 ExpressionCallees.from_attribute_access
                   {
                     AttributeAccessCallees.property_targets = [];
                     global_targets =
                       [
                         CallTarget.create_regular
                           ~return_type:None
                           (Target.Regular.Object "test.Token.token");
                       ];
                     is_attribute = true;
                     if_called = CallCallees.empty;
                   } );
             ]
           ~pyrefly_expected:
             [
               ( "6:9-6:36",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "builtins.getattr"; kind = Normal });
                        ]
                      ()) );
               ( "6:9-6:36|artificial-attribute-access|get-attr-constant-literal",
                 ExpressionCallees.from_attribute_access
                   {
                     AttributeAccessCallees.property_targets = [];
                     global_targets = [];
                     is_attribute = false;
                     if_called = CallCallees.empty;
                   } );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~object_targets:[Target.Regular.Object "test.Token.token"]
           ~source:
             {|
      class Token:
        token: str = ""

      def foo(obj: Token, x: str):
        return obj.__setattr__(obj, "token", x)
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "6:9-6:41",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"test.Token"
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__setattr__"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      class Test:
        def __setattr__(self, name: str, value):
          return

      def foo(obj: Test):
        obj.attribute = "value"
    |}
           ~define_name:"test.foo"
           ~expected:[] (* TODO(T137969662): We should see a call to `Test.__setattr__` *)
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:{|
      x = "x"

      def foo():
        return x
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "5:9-5:10|identifier|$local_test$x",
                 ExpressionCallees.from_identifier
                   (IdentifierCallees.create
                      ~global_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:None
                            (Target.Regular.Object "test.x");
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      def foo(a: int, b: float, c: str, d: typing.List[int], e):
        w = [1, 2, 3]
        x = 1
        y = "str"
        z = 2.3
        return f"{a}{b}{c}{d}{w}{x}{y}{z}{e}"
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "7:9-7:39|format-string-artificial",
                 ExpressionCallees.from_format_string_artificial
                   (FormatStringArtificialCallees.from_f_string_targets
                      [CallTarget.create ~return_type:None Target.ArtificialTargets.format_string])
               );
               ( "7:12-7:13|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~receiver_class:"int"
                          (Target.Regular.Method
                             { class_name = "object"; method_name = "__repr__"; kind = Normal });
                      ]) );
               ( "7:15-7:16|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~receiver_class:"float"
                          ~index:1
                          (Target.Regular.Method
                             { class_name = "object"; method_name = "__repr__"; kind = Normal });
                      ]) );
               ( "7:18-7:19|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~receiver_class:"str"
                          (Target.Regular.Method
                             { class_name = "str"; method_name = "__str__"; kind = Normal });
                      ]) );
               ( "7:21-7:22|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~index:2
                          ~receiver_class:"list"
                          (Target.Regular.Method
                             { class_name = "object"; method_name = "__repr__"; kind = Normal });
                      ]) );
               ( "7:24-7:25|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~index:3
                          ~receiver_class:"list"
                          (Target.Regular.Method
                             { class_name = "object"; method_name = "__repr__"; kind = Normal });
                      ]) );
               ( "7:27-7:28|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~index:4
                          ~receiver_class:"int"
                          (Target.Regular.Method
                             { class_name = "object"; method_name = "__repr__"; kind = Normal });
                      ]) );
               ( "7:30-7:31|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~index:1
                          ~receiver_class:"str"
                          (Target.Regular.Method
                             { class_name = "str"; method_name = "__str__"; kind = Normal });
                      ]) );
               ( "7:33-7:34|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~index:5
                          ~receiver_class:"float"
                          (Target.Regular.Method
                             { class_name = "object"; method_name = "__repr__"; kind = Normal });
                      ]) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      def bar(x):
        y = f"{x}" f"{x}"
        if bar(f"{x}"):
          return True
        else:
          return True
    |}
           ~define_name:"test.bar"
           ~expected:
             [
               ( "3:6-3:19|format-string-artificial",
                 ExpressionCallees.from_format_string_artificial
                   (FormatStringArtificialCallees.from_f_string_targets
                      [CallTarget.create ~return_type:None Target.ArtificialTargets.format_string])
               );
               ( "4:5-4:16",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.none)
                            (Target.Regular.Function { name = "test.bar"; kind = Normal });
                        ]
                      ()) );
               ( "4:9-4:15|format-string-artificial",
                 ExpressionCallees.from_format_string_artificial
                   (FormatStringArtificialCallees.from_f_string_targets
                      [
                        (* No duplicate targets, even if visiting both `if` and `if not`. *)
                        CallTarget.create
                          ~return_type:None
                          ~index:1
                          Target.ArtificialTargets.format_string;
                      ]) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:{|
      def foo(x: object):
        return f"{x}"
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "3:9-3:15|format-string-artificial",
                 ExpressionCallees.from_format_string_artificial
                   (FormatStringArtificialCallees.from_f_string_targets
                      [CallTarget.create ~return_type:None Target.ArtificialTargets.format_string])
               );
               ( "3:12-3:13|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        (* TODO(T112761296): Probably wrong call resolution *)
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~receiver_class:"object"
                          (Target.Regular.Method
                             { class_name = "object"; method_name = "__repr__"; kind = Normal });
                      ]) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:{|
      def foo(x: object):
        return f"{x}:{x}"
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "3:9-3:19|format-string-artificial",
                 ExpressionCallees.from_format_string_artificial
                   (FormatStringArtificialCallees.from_f_string_targets
                      [CallTarget.create ~return_type:None Target.ArtificialTargets.format_string])
               );
               ( "3:12-3:13|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        (* TODO(T112761296): Probably wrong call resolution *)
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~receiver_class:"object"
                          (Target.Regular.Method
                             { class_name = "object"; method_name = "__repr__"; kind = Normal });
                      ]) );
               ( "3:16-3:17|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        (* TODO(T112761296): Probably wrong call resolution *)
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~receiver_class:"object"
                          ~index:1
                          (Target.Regular.Method
                             { class_name = "object"; method_name = "__repr__"; kind = Normal });
                      ]) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:{|
      def foo(x: Any):
        return f"{x}"
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "3:9-3:15|format-string-artificial",
                 ExpressionCallees.from_format_string_artificial
                   (FormatStringArtificialCallees.from_f_string_targets
                      [CallTarget.create ~return_type:None Target.ArtificialTargets.format_string])
               );
               (* TODO(T112761296): Probably wrong call resolution. Expect an additional call
                  target. *)
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      class A:
        def __str__(self): return "stringified"

      def foo():
        a = A()
        "hello %s" % a
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "6:6-6:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__init__"; kind = Normal });
                        ]
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__new__"; kind = Normal });
                        ]
                      ()) );
               ( "7:2-7:16|artificial-call|binary",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          (* TODO(T146836847): Missing the stringify callee. *)
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"str"
                            (Target.Regular.Method
                               { class_name = "str"; method_name = "__mod__"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~cmp:DefineCallGraphForTest.equal_ignoring_types
           ~source:{|
      def foo(e: Exception):
        f"{e}"
        f"{type(e)}"
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "3:2-3:8|format-string-artificial",
                 ExpressionCallees.from_format_string_artificial
                   (FormatStringArtificialCallees.from_f_string_targets
                      [CallTarget.create ~return_type:None Target.ArtificialTargets.format_string])
               );
               ( "3:5-3:6|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~receiver_class:"Exception"
                          (Target.Regular.Method
                             {
                               class_name = "BaseException";
                               method_name = "__str__";
                               kind = Normal;
                             });
                      ]) );
               ( "4:2-4:14|format-string-artificial",
                 ExpressionCallees.from_format_string_artificial
                   (FormatStringArtificialCallees.from_f_string_targets
                      [
                        CallTarget.create
                          ~return_type:None
                          ~index:1
                          Target.ArtificialTargets.format_string;
                      ]) );
               ( "4:5-4:12|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        (* TODO(T112761296): Probably wrong call resolution *)
                        CallTarget.create_regular
                          ~index:1
                          ~implicit_receiver:true
                          (Target.Regular.Method
                             {
                               class_name = "BaseException";
                               method_name = "__str__";
                               kind = Normal;
                             });
                      ]) );
               ( "4:5-4:12",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "type"; method_name = "__new__"; kind = Normal });
                        ]
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               { class_name = "type"; method_name = "__init__"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      def foo(error_type: typing.Union[str, typing.Type[Exception]]):
        return f"{error_type}"
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "3:9-3:24|format-string-artificial",
                 ExpressionCallees.from_format_string_artificial
                   (FormatStringArtificialCallees.from_f_string_targets
                      [CallTarget.create ~return_type:None Target.ArtificialTargets.format_string])
               );
               ( "3:12-3:22|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          (Target.Regular.Method
                             { class_name = "object"; method_name = "__str__"; kind = Normal });
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~receiver_class:"str"
                          (Target.Regular.Method
                             { class_name = "str"; method_name = "__str__"; kind = Normal });
                      ]) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      def foo(error_type: typing.Type[Exception]):
        return f"{error_type}"
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "3:9-3:24|format-string-artificial",
                 ExpressionCallees.from_format_string_artificial
                   (FormatStringArtificialCallees.from_f_string_targets
                      [CallTarget.create ~return_type:None Target.ArtificialTargets.format_string])
               );
               ( "3:12-3:22|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        (* TODO(T112761296): Wrong call resolution *)
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          (Target.Regular.Method
                             {
                               class_name = "BaseException";
                               method_name = "__str__";
                               kind = Normal;
                             });
                      ]) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~cmp:DefineCallGraphForTest.equal_ignoring_types
           ~source:
             {|
      class A:
        def __str__(self):
          return "A"
      class B:
        pass
      def foo(x: typing.Union[A, B]):
        f"{x.__class__}"
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "8:2-8:18|format-string-artificial",
                 ExpressionCallees.from_format_string_artificial
                   (FormatStringArtificialCallees.from_f_string_targets
                      [CallTarget.create ~return_type:None Target.ArtificialTargets.format_string])
               );
               ( "8:5-8:16|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          (Target.Regular.Method
                             { class_name = "object"; method_name = "__str__"; kind = Normal });
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          (Target.Regular.Method
                             { class_name = "test.A"; method_name = "__str__"; kind = Normal });
                      ]) );
               ( "8:5-8:16",
                 ExpressionCallees.from_attribute_access
                   {
                     AttributeAccessCallees.property_targets =
                       [
                         CallTarget.create_regular
                           ~implicit_receiver:true
                           (Target.Regular.Method
                              { class_name = "object"; method_name = "__class__"; kind = Normal });
                       ];
                     global_targets = [];
                     is_attribute = false;
                     if_called = CallCallees.empty;
                   } );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~cmp:DefineCallGraphForTest.equal_ignoring_types
           ~source:
             {|
      class A:
        def __str__(self):
          return "A"
      def foo(x: A):
        f"{x.__class__}"
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "6:2-6:18|format-string-artificial",
                 ExpressionCallees.from_format_string_artificial
                   (FormatStringArtificialCallees.from_f_string_targets
                      [CallTarget.create ~return_type:None Target.ArtificialTargets.format_string])
               );
               ( "6:5-6:16|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        (* TODO(T112761296): Probably wrong call resolution *)
                        CallTarget.create_regular
                          (Target.Regular.Method
                             { class_name = "test.A"; method_name = "__str__"; kind = Normal });
                      ]) );
               ( "6:5-6:16",
                 ExpressionCallees.from_attribute_access
                   {
                     AttributeAccessCallees.property_targets =
                       [
                         CallTarget.create_regular
                           ~implicit_receiver:true
                           (Target.Regular.Method
                              { class_name = "object"; method_name = "__class__"; kind = Normal });
                       ];
                     global_targets = [];
                     is_attribute = false;
                     if_called = CallCallees.empty;
                   } );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~cmp:DefineCallGraphForTest.equal_ignoring_types
           ~source:{|
      def foo(e: Exception):
        return str(e) + "hello"
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "3:9-3:15|artificial-call|str-call-to-dunder-str",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"Exception"
                            (Target.Regular.Method
                               {
                                 class_name = "BaseException";
                                 method_name = "__str__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "3:9-3:25|artificial-call|binary",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"str"
                            (Target.Regular.Method
                               { class_name = "str"; method_name = "__add__"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
     def foo():
         pass

     def bar():
         pass

     def baz():
         foo()
         foo()
         bar()
         foo()
         bar()
  |}
           ~define_name:"test.baz"
           ~expected:
             [
               ( "9:4-9:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~index:0
                            (Target.Regular.Function { name = "test.foo"; kind = Normal });
                        ]
                      ()) );
               ( "10:4-10:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~index:1
                            (Target.Regular.Function { name = "test.foo"; kind = Normal });
                        ]
                      ()) );
               ( "11:4-11:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~index:0
                            (Target.Regular.Function { name = "test.bar"; kind = Normal });
                        ]
                      ()) );
               ( "12:4-12:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~index:2
                            (Target.Regular.Function { name = "test.foo"; kind = Normal });
                        ]
                      ()) );
               ( "13:4-13:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~index:1
                            (Target.Regular.Function { name = "test.bar"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
     def foo(x=None, y=None):
         pass

     def bar():
         foo(foo(), foo(foo()))
  |}
           ~define_name:"test.bar"
           ~expected:
             [
               ( "6:4-6:26",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~index:0
                            (Target.Regular.Function { name = "test.foo"; kind = Normal });
                        ]
                      ()) );
               ( "6:8-6:13",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~index:1
                            (Target.Regular.Function { name = "test.foo"; kind = Normal });
                        ]
                      ()) );
               ( "6:15-6:25",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~index:2
                            (Target.Regular.Function { name = "test.foo"; kind = Normal });
                        ]
                      ()) );
               ( "6:19-6:24",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~index:3
                            (Target.Regular.Function { name = "test.foo"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      from typing import Union

      class A:
          def foo():
              pass

      class B(A):
          pass

      class C(A):
          pass

      def test(x: Union[B, C]):
          x.foo()
          if isinstance(x, C):
              x.foo()
          else:
              x.foo()

          if isinstance(x, B):
              x.foo()
  |}
           ~define_name:"test.test"
           ~expected:
             [
               ( "15:4-15:11",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~index:0
                            ~receiver_class:"test.B"
                            (Target.Regular.Method
                               { class_name = "test.A"; method_name = "foo"; kind = Normal });
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~index:0
                            ~receiver_class:"test.C"
                            (Target.Regular.Method
                               { class_name = "test.A"; method_name = "foo"; kind = Normal });
                        ]
                      ()) );
               ( "16:7-16:23",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~index:0
                            ~return_type:(Some ReturnType.bool)
                            (Target.Regular.Function { name = "isinstance"; kind = Normal });
                        ]
                      ()) );
               ( "17:8-17:15",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~index:1
                            ~receiver_class:"test.C"
                            (Target.Regular.Method
                               { class_name = "test.A"; method_name = "foo"; kind = Normal });
                        ]
                      ()) );
               ( "19:8-19:15",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~index:2
                            ~receiver_class:"test.B"
                            (Target.Regular.Method
                               { class_name = "test.A"; method_name = "foo"; kind = Normal });
                        ]
                      ()) );
               ( "21:7-21:23",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~index:1
                            ~return_type:(Some ReturnType.bool)
                            (Target.Regular.Function { name = "isinstance"; kind = Normal });
                        ]
                      ()) );
               ( "22:8-22:15",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~index:3
                            ~receiver_class:"test.B"
                            (Target.Regular.Method
                               { class_name = "test.A"; method_name = "foo"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      (* Test the return type when using type variables. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
       import typing

       def bar(l: typing.List[int]):
         return l.__iter__().__next__()
      |}
           ~define_name:"test.bar"
           ~expected:
             [
               ( "5:9-5:21",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.unknown)
                            ~receiver_class:"list"
                            (Target.Regular.Method
                               { class_name = "list"; method_name = "__iter__"; kind = Normal });
                        ]
                      ()) );
               ( "5:9-5:32",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~receiver_class:"typing.Iterator"
                            (Target.Regular.Method
                               {
                                 class_name = "typing.Iterator";
                                 method_name = "__next__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
       import typing

       def bar(l: typing.List[int]):
         return l[0]
      |}
           ~define_name:"test.bar"
           ~expected:
             [
               ( "5:9-5:13|artificial-call|subscript-get-item",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~receiver_class:"list"
                            (Target.Regular.Method
                               { class_name = "list"; method_name = "__getitem__"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
       from typing import Union, overload

       @overload
       def foo(x: int) -> int:
         ...

       @overload
       def foo(x: str) -> str:
         ...

       def foo(x: Union[int, str]) -> Union[int, str]:
         return x

       def bar():
         return foo(0)
      |}
           ~define_name:"test.bar"
           ~expected:
             [
               ( "16:9-16:15",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.integer)
                            (Target.Regular.Function { name = "test.foo"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
       import typing

       T = typing.TypeVar("T")

       def foo(x: T) -> T:
         return T

       def bar():
         return foo(0)
      |}
           ~define_name:"test.bar"
           ~expected:
             [
               ( "10:9-10:15",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.integer)
                            (Target.Regular.Function { name = "test.foo"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      (* Nested defines. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
       def baz(x: int) -> int:
         return x

       def foo():
         def bar(x: int) -> int:
           return baz(x)

         return bar
      |}
           ~define_name:"test.foo.bar"
           ~expected:
             [
               ( "7:11-7:17",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.integer)
                            (Target.Regular.Function { name = "test.baz"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
       def baz(x: int) -> int:
         return x

       def foo():
         if 1 < 2:
           def bar(x: int) -> int:
             return baz(x)

           return bar
         else:
           return None
      |}
           ~define_name:"test.foo.bar"
           ~expected:
             [
               ( "8:13-8:19",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.integer)
                            (Target.Regular.Function { name = "test.baz"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
       def decorator(function):
           return function

       class Base:
           @decorator
           def query(self, arg):
               return arg

       class Child(Base):
           pass

       class SubChild(Child):
           def query(self, arg):
               return arg

       def foo(base: Base, child: Child):
           base.query(1)
           child.query(1)
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "18:4-18:17",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            (Target.Regular.Override
                               {
                                 Target.Method.class_name = "test.Base";
                                 method_name = "query";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "19:4-19:18",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               {
                                 Target.Method.class_name = "test.Base";
                                 method_name = "query";
                                 kind = Decorated;
                               });
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               {
                                 Target.Method.class_name = "test.SubChild";
                                 method_name = "query";
                                 kind = Normal;
                               });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
       def decorator(function):
           return function

       class BaseA:
           @decorator
           def query(self, arg):
               return arg

       class BaseB:
           pass

       class BaseC:
           @decorator
           def query(self, arg):
               return arg

       class Child(BaseB, BaseA, BaseC):
           pass

       def foo(base: BaseA, child: Child):
           base.query(1)
           child.query(1)
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "22:4-22:17",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               {
                                 Target.Method.class_name = "test.BaseA";
                                 method_name = "query";
                                 kind = Decorated;
                               });
                        ]
                      ()) );
               ( "23:4-23:18",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~index:1
                            (Target.Regular.Method
                               {
                                 (* Not `test.BaseC`, because `A` is the first parent class of
                                    `Child`. *)
                                 Target.Method.class_name = "test.BaseA";
                                 method_name = "query";
                                 kind = Decorated;
                               });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
      from abc import abstractclassmethod
      from typing import TypeVar, Generic
      TInput = TypeVar("TInput")

      class C(Generic[TInput]):
        @abstractclassmethod
        def f(cls, arg: TInput) -> TInput:
          pass
        @classmethod
        def h(cls, arg: TInput) -> TInput:
          pass
        @classmethod
        def g(cls, arg: TInput):
          cls.f(arg)
          functools.partial(cls.f, arg)
          cls.h(arg)
          functools.partial(cls.h, arg)
      |}
           ~define_name:"test.C.g"
           ~expected:
             [
               ( "15:4-15:14",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~is_class_method:true
                            (Target.Regular.Method
                               { class_name = "test.C"; method_name = "f"; kind = Normal });
                        ]
                      ()) );
               ( "16:4-16:33",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__new__"; kind = Normal });
                        ]
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               {
                                 class_name = "functools.partial";
                                 method_name = "__init__";
                                 kind = Normal;
                               });
                        ]
                      ~shim_target:
                        (Some
                           {
                             ShimTarget.call_targets =
                               [
                                 CallTarget.create_regular
                                   ~implicit_receiver:true
                                   ~is_class_method:true
                                   ~index:1
                                   (Target.Regular.Method
                                      { class_name = "test.C"; method_name = "f"; kind = Normal });
                               ];
                             decorated_targets = [];
                             argument_mapping =
                               {
                                 ShimArgumentMapping.identifier = "functools.partial";
                                 callee = ShimArgumentMapping.Target.Argument { index = 0 };
                                 arguments =
                                   [
                                     {
                                       ShimArgumentMapping.Argument.name = None;
                                       value = ShimArgumentMapping.Target.Argument { index = 1 };
                                     };
                                   ];
                               };
                           })
                      ()) );
               ( "17:4-17:14",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~is_class_method:true
                            ~receiver_class:"test.C"
                            (Target.Regular.Method
                               { class_name = "test.C"; method_name = "h"; kind = Normal });
                        ]
                      ()) );
               ( "18:4-18:33",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~is_static_method:true
                            ~index:1
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__new__"; kind = Normal });
                        ]
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~index:1
                            (Target.Regular.Method
                               {
                                 class_name = "functools.partial";
                                 method_name = "__init__";
                                 kind = Normal;
                               });
                        ]
                      ~shim_target:
                        (Some
                           {
                             ShimTarget.call_targets =
                               [
                                 CallTarget.create_regular
                                   ~implicit_receiver:true
                                   ~is_class_method:true
                                   ~receiver_class:"test.C"
                                   ~index:1
                                   (Target.Regular.Method
                                      { class_name = "test.C"; method_name = "h"; kind = Normal });
                               ];
                             decorated_targets = [];
                             argument_mapping =
                               {
                                 ShimArgumentMapping.identifier = "functools.partial";
                                 callee = ShimArgumentMapping.Target.Argument { index = 0 };
                                 arguments =
                                   [
                                     {
                                       ShimArgumentMapping.Argument.name = None;
                                       value = ShimArgumentMapping.Target.Argument { index = 1 };
                                     };
                                   ];
                               };
                           })
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      from typing import Generic, TypeVar

      T = TypeVar("T")
      def decorator(function):
          return function

      class A(Generic[T]):
          @decorator
          def query(self, arg: T) -> T:
              pass
      class B(A[int]):
          pass
      class C(A[int]):
          def query(self, arg: int) -> int:
              return arg
      class D(B):
          def query(self, arg: int) -> int:
              pass

      def foo(base: A[int], child_b: B, child_c: C, child_d: D):
          base.query(1)
          child_b.query(1)
          child_c.query(1)
          child_d.query(1)
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "22:4-22:17",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            (Target.Regular.Override
                               {
                                 Target.Method.class_name = "test.A";
                                 method_name = "query";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "23:4-23:20",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                          (* TODO(T118125320): Return type is None, which is incorrect *)
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               {
                                 Target.Method.class_name = "test.A";
                                 method_name = "query";
                                 kind = Decorated;
                               });
                          CallTarget.create_regular
                          (* TODO(T118125320): Return type is None, which is incorrect *)
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               {
                                 Target.Method.class_name = "test.D";
                                 method_name = "query";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "24:4-24:20",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~receiver_class:"test.C"
                            (Target.Regular.Method
                               {
                                 Target.Method.class_name = "test.C";
                                 method_name = "query";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "25:4-25:20",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~receiver_class:"test.D"
                            ~index:1
                            (Target.Regular.Method
                               {
                                 Target.Method.class_name = "test.D";
                                 method_name = "query";
                                 kind = Normal;
                               });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
      from typing import Generic, TypeVar

      T = TypeVar("T")
      def decorator(function):
          return function

      class A(Generic[T]):
          @decorator
          def query(self, arg: T) -> None:
              pass
      class B(A[int]):
          pass

      def foo(base: A[T], arg: T) -> None:
          base.query(arg)
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "16:4-16:19",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               {
                                 Target.Method.class_name = "test.A";
                                 method_name = "query";
                                 kind = Decorated;
                               });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
       def foo(l: typing.AsyncIterator[int | str]):
         async for x in l:
           pass
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "3:17-3:18|artificial-call|for-iter",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"typing.AsyncIterator"
                            (Target.Regular.Method
                               {
                                 class_name = "typing.AsyncIterator";
                                 method_name = "__aiter__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "3:17-3:18|artificial-call|for-next",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"typing.AsyncIterator"
                            (Target.Regular.Method
                               {
                                 class_name = "typing.AsyncIterator";
                                 method_name = "__anext__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
       class A:
         def f(self) -> typing.List[int]:
           return [1, 2]
       def g() -> A:
         return A()
       def id(arg):
         return arg
       def foo(l0: typing.AsyncIterator[int], l1: typing.List[int], l2: typing.AsyncIterable[int]):
         x = [x async for x in l0]
         x = [x for x in l1]  # List comprehension
         x = [x async for x in l2]  # List comprehension
         x = [x for x in g().f()]  # Iterator as a compound AST node
         x = {x for x in l1}  # Set comprehension
         x = {x async for x in l2}  # Set comprehension
         x = {x:0 for x in l1}  # Dictionary comprehension
         x = {x:0 async for x in l2}  # Dictionary comprehension
         x = (x for x in l1) # Generator comprehension
         x = (x async for x in l2)  # Generator comprehension
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "10:24-10:26|artificial-call|generator-iter",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"typing.AsyncIterator"
                            (Target.Regular.Method
                               {
                                 class_name = "typing.AsyncIterator";
                                 method_name = "__aiter__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "10:24-10:26|artificial-call|generator-next",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~receiver_class:"typing.AsyncIterator"
                            (Target.Regular.Method
                               {
                                 class_name = "typing.AsyncIterator";
                                 method_name = "__anext__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "11:18-11:20|artificial-call|generator-iter",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"list"
                            (Target.Regular.Method
                               { class_name = "list"; method_name = "__iter__"; kind = Normal });
                        ]
                      ()) );
               ( "11:18-11:20|artificial-call|generator-next",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~receiver_class:"typing.Iterator"
                            (Target.Regular.Method
                               {
                                 class_name = "typing.Iterator";
                                 method_name = "__next__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "12:24-12:26|artificial-call|generator-iter",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"typing.AsyncIterable"
                            (Target.Regular.Method
                               {
                                 class_name = "typing.AsyncIterable";
                                 method_name = "__aiter__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "12:24-12:26|artificial-call|generator-next",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~receiver_class:"typing.AsyncIterator"
                            ~index:1
                            (Target.Regular.Method
                               {
                                 class_name = "typing.AsyncIterator";
                                 method_name = "__anext__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "13:18-13:21",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.g"; kind = Normal });
                        ]
                      ()) );
               ( "13:18-13:25|artificial-call|generator-iter",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"list"
                            ~index:1
                            (Target.Regular.Method
                               { class_name = "list"; method_name = "__iter__"; kind = Normal });
                        ]
                      ()) );
               ( "13:18-13:25|artificial-call|generator-next",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"typing.Iterator"
                            ~return_type:(Some ReturnType.integer)
                            ~index:1
                            (Target.Regular.Method
                               {
                                 class_name = "typing.Iterator";
                                 method_name = "__next__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "13:18-13:25",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"test.A"
                            (Target.Regular.Method
                               { class_name = "test.A"; method_name = "f"; kind = Normal });
                        ]
                      ()) );
               ( "14:18-14:20|artificial-call|generator-iter",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"list"
                            ~index:2
                            (Target.Regular.Method
                               { class_name = "list"; method_name = "__iter__"; kind = Normal });
                        ]
                      ()) );
               ( "14:18-14:20|artificial-call|generator-next",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~receiver_class:"typing.Iterator"
                            ~index:2
                            (Target.Regular.Method
                               {
                                 class_name = "typing.Iterator";
                                 method_name = "__next__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "15:24-15:26|artificial-call|generator-iter",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"typing.AsyncIterable"
                            ~index:1
                            (Target.Regular.Method
                               {
                                 class_name = "typing.AsyncIterable";
                                 method_name = "__aiter__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "15:24-15:26|artificial-call|generator-next",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~receiver_class:"typing.AsyncIterator"
                            ~index:2
                            (Target.Regular.Method
                               {
                                 class_name = "typing.AsyncIterator";
                                 method_name = "__anext__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "16:20-16:22|artificial-call|generator-iter",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"list"
                            ~index:3
                            (Target.Regular.Method
                               { class_name = "list"; method_name = "__iter__"; kind = Normal });
                        ]
                      ()) );
               ( "16:20-16:22|artificial-call|generator-next",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~receiver_class:"typing.Iterator"
                            ~index:3
                            (Target.Regular.Method
                               {
                                 class_name = "typing.Iterator";
                                 method_name = "__next__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "17:26-17:28|artificial-call|generator-iter",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"typing.AsyncIterable"
                            ~index:2
                            (Target.Regular.Method
                               {
                                 class_name = "typing.AsyncIterable";
                                 method_name = "__aiter__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "17:26-17:28|artificial-call|generator-next",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~receiver_class:"typing.AsyncIterator"
                            ~index:3
                            (Target.Regular.Method
                               {
                                 class_name = "typing.AsyncIterator";
                                 method_name = "__anext__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "18:18-18:20|artificial-call|generator-iter",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"list"
                            ~index:4
                            (Target.Regular.Method
                               { class_name = "list"; method_name = "__iter__"; kind = Normal });
                        ]
                      ()) );
               ( "18:18-18:20|artificial-call|generator-next",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~receiver_class:"typing.Iterator"
                            ~index:4
                            (Target.Regular.Method
                               {
                                 class_name = "typing.Iterator";
                                 method_name = "__next__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "19:24-19:26|artificial-call|generator-iter",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"typing.AsyncIterable"
                            ~index:3
                            (Target.Regular.Method
                               {
                                 class_name = "typing.AsyncIterable";
                                 method_name = "__aiter__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "19:24-19:26|artificial-call|generator-next",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.integer)
                            ~receiver_class:"typing.AsyncIterator"
                            ~index:4
                            (Target.Regular.Method
                               {
                                 class_name = "typing.AsyncIterator";
                                 method_name = "__anext__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:true
           ~source:
             {|
       class A:
         def foo(self): pass
       class B:
         def foo(self): pass
       def f(l: typing.AsyncIterator[A], x: B):
         ([x async for x in l], x.foo())
      |}
           ~define_name:"test.f"
           ~expected:
             [
               ( "7:21-7:22|artificial-call|generator-iter",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"typing.AsyncIterator"
                            (Target.Regular.Method
                               {
                                 class_name = "typing.AsyncIterator";
                                 method_name = "__aiter__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "7:21-7:22|artificial-call|generator-next",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"typing.AsyncIterator"
                            (Target.Regular.Method
                               {
                                 class_name = "typing.AsyncIterator";
                                 method_name = "__anext__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "7:25-7:32",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"test.B"
                            (Target.Regular.Method
                               {
                                 Target.Method.class_name = "test.B";
                                 method_name = "foo";
                                 kind = Normal;
                               });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
      class Object:
        pass

      x = Object()
      y = Object()

      def foo():
        x.bar = ""
        y.bar = ""

        baz(x)
        baz(y)

      def baz(x: Object):
        pass
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "9:2-9:3|identifier|$local_test$x",
                 ExpressionCallees.from_identifier
                   (IdentifierCallees.create
                      ~global_targets:
                        [
                          CallTarget.create_regular
                            ~index:0
                            ~return_type:None
                            (Target.Regular.Object "test.x");
                        ]
                      ()) );
               ( "10:2-10:3|identifier|$local_test$y",
                 ExpressionCallees.from_identifier
                   (IdentifierCallees.create
                      ~global_targets:
                        [
                          CallTarget.create_regular
                            ~index:0
                            ~return_type:None
                            (Target.Regular.Object "test.y");
                        ]
                      ()) );
               ( "12:6-12:7|identifier|$local_test$x",
                 ExpressionCallees.from_identifier
                   (IdentifierCallees.create
                      ~global_targets:
                        [
                          CallTarget.create_regular
                            ~index:1
                            ~return_type:None
                            (Target.Regular.Object "test.x");
                        ]
                      ()) );
               ( "12:2-12:8",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~index:0
                            (Target.Regular.Function { name = "test.baz"; kind = Normal });
                        ]
                      ()) );
               ( "13:6-13:7|identifier|$local_test$y",
                 ExpressionCallees.from_identifier
                   (IdentifierCallees.create
                      ~global_targets:
                        [
                          CallTarget.create_regular
                            ~index:1
                            ~return_type:None
                            (Target.Regular.Object "test.y");
                        ]
                      ()) );
               ( "13:2-13:8",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~index:1
                            (Target.Regular.Function { name = "test.baz"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
      class Object:
        pass

      x = Object()

      def foo():
        return x.bar
    |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "8:9-8:10|identifier|$local_test$x",
                 ExpressionCallees.from_identifier
                   (IdentifierCallees.create
                      ~global_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:None
                            (Target.Regular.Object "test.x");
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~object_targets:[Target.Regular.Object "test.A.B"]
           ~source:
             {|
      from typing import Any, MutableMapping
      from typing_extensions import Self
      from pyre_extensions import PyreReadOnly

      class A:
        B: MutableMapping[str, Any] = {}

        def __init__(self) -> None:
          self.B: MutableMapping[str, Any] = {}

        def self_readonly(self: PyreReadOnly[Self]) -> None:
          self.B.get("")
     |}
           ~define_name:"test.A.self_readonly"
           ~expected:
             [
               ( "13:4-13:10",
                 ExpressionCallees.from_attribute_access
                   {
                     AttributeAccessCallees.property_targets = [];
                     global_targets =
                       [
                         CallTarget.create_regular
                           ~return_type:None
                           (Target.Regular.Object "test.A.B");
                       ];
                     is_attribute = true;
                     if_called = CallCallees.empty;
                   } );
               ( "13:4-13:18",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"typing.MutableMapping"
                            ~return_type:(Some ReturnType.none)
                            (Target.Regular.Method
                               { class_name = "typing.Mapping"; method_name = "get"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~object_targets:[Target.Regular.Object "test.A.B"]
           ~source:
             {|
      from typing import Any, MutableMapping
      from typing_extensions import Self
      from pyre_extensions import PyreReadOnly

      class A:
        def __init__(self) -> None:
          self.B: MutableMapping[str, Any] = {}

        def self_untyped(self) -> None:
          self.B.get("")
    |}
           ~define_name:"test.A.self_untyped"
           ~expected:
             [
               ( "11:4-11:10",
                 ExpressionCallees.from_attribute_access
                   {
                     AttributeAccessCallees.property_targets = [];
                     global_targets =
                       [
                         CallTarget.create_regular
                           ~return_type:None
                           (Target.Regular.Object "test.A.B");
                       ];
                     is_attribute = true;
                     if_called = CallCallees.empty;
                   } );
               ( "11:4-11:18",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"typing.MutableMapping"
                            ~return_type:(Some ReturnType.none)
                            (Target.Regular.Method
                               { class_name = "typing.Mapping"; method_name = "get"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:{|
      x = ""
      def foo():
        global x
        x = "str"
      |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "5:2-5:3|identifier|$local_test$x",
                 ExpressionCallees.from_identifier
                   (IdentifierCallees.create
                      ~global_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:None
                            (Target.Regular.Object "test.x");
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
      def outer():
        x = ""
        def inner():
          nonlocal x
          x = "str"
      |}
           ~define_name:"test.outer.inner"
           ~expected:
             [
               ( "6:4-6:5|identifier|$local_test?outer$x",
                 ExpressionCallees.from_identifier
                   (IdentifierCallees.create
                      ~nonlocal_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:None
                            (Target.Regular.Object "test.outer.x");
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
      def outer():
        x = ""
        def inner():
          y = x
      |}
           ~define_name:"test.outer.inner"
           ~expected:
             [
               ( "5:8-5:9|identifier|$local_test?outer$x",
                 ExpressionCallees.from_identifier
                   (IdentifierCallees.create
                      ~nonlocal_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:None
                            (Target.Regular.Object "test.outer.x");
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:{|
     def foo():
       return bar
     def bar(): ...
  |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "3:9-3:12|artificial-attribute-access|qualification:test.bar",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:{|
     def foo():
       yield bar
     def bar(): ...
  |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "3:8-3:11|artificial-attribute-access|qualification:test.bar",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
     class C:
       def m(self): ...
     def foo(c: C):
       return c.m
  |}
           ~define_name:"test.foo"
           ~expected:[]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:{|
     def foo():
       def inner(): ...
       return inner
  |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "3:2-3:18",
                 ExpressionCallees.from_define
                   (DefineCallees.create
                      ~define_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.foo.inner"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
     from typing import Callable, Any
     def bar(f: Callable[Any, Any]) -> Callable[Any, Any]:
       return f
     def foo(after_functions: list[Callable[Any, Any]]) -> None:
       for after_code in after_functions:
         bar(after_code)
  |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "6:20-6:35|artificial-call|for-iter",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"list"
                            (Target.Regular.Method
                               { class_name = "list"; method_name = "__iter__"; kind = Normal });
                        ]
                      ()) );
               ( "6:20-6:35|artificial-call|for-next",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"typing.Iterator"
                            (Target.Regular.Method
                               {
                                 class_name = "typing.Iterator";
                                 method_name = "__next__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "7:4-7:19",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.bar"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
      from typing import Callable, Any
      def decorator_1(callable: Callable[[Any], Any]) -> Callable[[Any], Any]:
        return callable
      def decorator_2(callable: Callable[[Any, int], int]) -> Callable[[Any, int], int]:
        return callable
      def decorator_3(callable: Callable[[Any, int], int]) -> Callable[[Any, int], int]:
        return callable

      class Foo:
        @decorator_2
        def callee_1(self, x: int) -> int:
          return x
        @decorator_2
        def callee_2(self, x: int) -> int:
          return x
        def not_callee_1(self, x: int) -> int:
          return x
        @decorator_3
        def not_callee_2(self, x: int) -> int:
          return x

      @decorator_1
      def caller(foo: Foo) -> Foo:  # Test return callees
        return foo
    |}
           ~define_name:"test.caller"
           ~expected:
             [
               ( "25:2-25:12",
                 ExpressionCallees.from_return
                   {
                     ReturnShimCallees.call_targets =
                       [
                         CallTarget.create_regular
                           (Target.Regular.Method
                              { class_name = "test.Foo"; method_name = "callee_1"; kind = Normal });
                         CallTarget.create_regular
                           (Target.Regular.Method
                              { class_name = "test.Foo"; method_name = "callee_2"; kind = Normal });
                       ];
                     arguments = [ReturnShimCallees.ReturnExpression];
                   } );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
      from typing import Callable, Optional
      def decorator_1(callable: Callable[[Any], Any]) -> Callable[[Any], Any]:
        return callable
      def decorator_2(callable: Callable[[Any, int], int]) -> Callable[[Any, int], int]:
        return callable

      class Foo:
        @decorator_2
        def callee(self, x: int) -> int:
          return x

      @decorator_1
      def caller(foo: Optional[Foo]) -> Optional[Foo]:  # Test stripping
        return foo
    |}
           ~define_name:"test.caller"
           ~expected:
             [
               ( "15:2-15:12",
                 ExpressionCallees.from_return
                   {
                     ReturnShimCallees.call_targets =
                       [
                         CallTarget.create_regular
                           (Target.Regular.Method
                              { class_name = "test.Foo"; method_name = "callee"; kind = Normal });
                       ];
                     arguments = [ReturnShimCallees.ReturnExpression];
                   } );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
      from typing import Callable, List
      def decorator_1(callable: Callable[[Any], Any]) -> Callable[[Any], Any]:
        return callable
      def decorator_2(callable: Callable[[Any, int], int]) -> Callable[[Any, int], int]:
        return callable

      class Foo:
        @decorator_2
        def callee(self, x: int) -> int:
          return x

      @decorator_1
      def caller(foo: Foo) -> List[Foo]:  # Test stripping
        return [foo]
    |}
           ~define_name:"test.caller"
           ~expected:
             [
               ( "15:2-15:14",
                 ExpressionCallees.from_return
                   {
                     ReturnShimCallees.call_targets =
                       [
                         CallTarget.create_regular
                           (Target.Regular.Method
                              { class_name = "test.Foo"; method_name = "callee"; kind = Normal });
                       ];
                     arguments = [ReturnShimCallees.ReturnExpressionElement];
                   } );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
      from typing import Callable, Sequence
      def decorator_1(callable: Callable[[Any], Any]) -> Callable[[Any], Any]:
        return callable
      def decorator_2(callable: Callable[[Any, int], int]) -> Callable[[Any, int], int]:
        return callable

      class Foo:
        @decorator_2
        def callee(self, x: int) -> int:
          return x

      @decorator_1
      def caller(foo: Foo) -> Sequence[Foo]:  # Test stripping
        return [foo]
    |}
           ~define_name:"test.caller"
           ~expected:
             [
               ( "15:2-15:14",
                 ExpressionCallees.from_return
                   {
                     ReturnShimCallees.call_targets =
                       [
                         CallTarget.create_regular
                           (Target.Regular.Method
                              { class_name = "test.Foo"; method_name = "callee"; kind = Normal });
                       ];
                     arguments = [ReturnShimCallees.ReturnExpressionElement];
                   } );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
      from typing import Callable, Set
      def decorator_1(callable: Callable[[Any], Any]) -> Callable[[Any], Any]:
        return callable
      def decorator_2(callable: Callable[[Any, int], int]) -> Callable[[Any, int], int]:
        return callable

      class Foo:
        @decorator_2
        def callee(self, x: int) -> int:
          return x

      @decorator_1
      def caller(foo: Foo) -> Set[Foo]:  # Test stripping
        return {foo}
    |}
           ~define_name:"test.caller"
           ~expected:
             [
               ( "15:2-15:14",
                 ExpressionCallees.from_return
                   {
                     ReturnShimCallees.call_targets =
                       [
                         CallTarget.create_regular
                           (Target.Regular.Method
                              { class_name = "test.Foo"; method_name = "callee"; kind = Normal });
                       ];
                     arguments = [ReturnShimCallees.ReturnExpressionElement];
                   } );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
      from typing import Callable
      def decorator_1(callable: Callable[[Any], Any]) -> Callable[[Any], Any]:
        return callable
      def decorator_2(callable: Callable[[Any, int], int]) -> Callable[[Any, int], int]:
        return callable

      class Foo:
        @decorator_2
        def callee(self, x: int) -> int:
          return x

      @decorator_1
      def caller() -> Foo:
        return Foo()  # Test co-existence of return callees and expression callees
    |}
           ~define_name:"test.caller"
           ~expected:
             [
               ( "15:9-15:14",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__init__"; kind = Normal });
                        ]
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__new__"; kind = Normal });
                        ]
                      ()) );
               ( "15:2-15:14",
                 ExpressionCallees.from_return
                   {
                     ReturnShimCallees.call_targets =
                       [
                         CallTarget.create_regular
                           (Target.Regular.Method
                              { class_name = "test.Foo"; method_name = "callee"; kind = Normal });
                       ];
                     arguments = [ReturnShimCallees.ReturnExpression];
                   } );
             ]
           ();
    ]


let test_call_graph_of_define_foo_and_bar =
  let source =
    {|
      class A:
        def __str__(self):
          return "A"

      class B:
        def __repr__(self):
          return "B"

      class C:
        def __str__(self):
          return "C"
        def __repr__(self):
          return "C"

      class D:
        def foo():
          pass

      def foo():
        a = A()
        b = B()
        c = C()
        d = D()
        return f"{a}hello{b}world{c}{d}"

      def bar(x: typing.Union[A, B, C, D]):
        return f"{x}"
    |}
  in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source
           ~define_name:"test.foo"
           ~expected:
             [
               ( "21:6-21:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__init__"; kind = Normal });
                        ]
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__new__"; kind = Normal });
                        ]
                      ()) );
               ( "22:6-22:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~index:1
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__init__"; kind = Normal });
                        ]
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~index:1
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__new__"; kind = Normal });
                        ]
                      ()) );
               ( "23:6-23:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~index:2
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__init__"; kind = Normal });
                        ]
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~index:2
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__new__"; kind = Normal });
                        ]
                      ()) );
               ( "24:6-24:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~index:3
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__init__"; kind = Normal });
                        ]
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~index:3
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__new__"; kind = Normal });
                        ]
                      ()) );
               ( "25:9-25:34|format-string-artificial",
                 ExpressionCallees.from_format_string_artificial
                   (FormatStringArtificialCallees.from_f_string_targets
                      [CallTarget.create ~return_type:None Target.ArtificialTargets.format_string])
               );
               ( "25:12-25:13|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~receiver_class:"test.A"
                          (Target.Regular.Method
                             { class_name = "test.A"; method_name = "__str__"; kind = Normal });
                      ]) );
               ( "25:20-25:21|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~receiver_class:"test.B"
                          (Target.Regular.Method
                             { class_name = "test.B"; method_name = "__repr__"; kind = Normal });
                      ]) );
               ( "25:28-25:29|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~receiver_class:"test.C"
                          (Target.Regular.Method
                             { class_name = "test.C"; method_name = "__str__"; kind = Normal });
                      ]) );
               ( "25:31-25:32|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~receiver_class:"test.D"
                          (Target.Regular.Method
                             { class_name = "object"; method_name = "__repr__"; kind = Normal });
                      ]) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source
           ~define_name:"test.bar"
           ~expected:
             [
               ( "28:9-28:15|format-string-artificial",
                 ExpressionCallees.from_format_string_artificial
                   (FormatStringArtificialCallees.from_f_string_targets
                      [CallTarget.create ~return_type:None Target.ArtificialTargets.format_string])
               );
               ( "28:12-28:13|format-string-stringify",
                 ExpressionCallees.from_format_string_stringify
                   (FormatStringStringifyCallees.from_stringify_targets
                      [
                        (* TODO(T112028293): Properly resolve `__str__` calls on union-typed
                           variables *)
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~receiver_class:"test.B"
                          (Target.Regular.Method
                             { class_name = "object"; method_name = "__str__"; kind = Normal });
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~receiver_class:"test.D"
                          (Target.Regular.Method
                             { class_name = "object"; method_name = "__str__"; kind = Normal });
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~receiver_class:"test.A"
                          (Target.Regular.Method
                             { class_name = "test.A"; method_name = "__str__"; kind = Normal });
                        CallTarget.create_regular
                          ~implicit_receiver:true
                          ~receiver_class:"test.C"
                          (Target.Regular.Method
                             { class_name = "test.C"; method_name = "__str__"; kind = Normal });
                      ]) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:{|
     def foo():
       return bar
     def bar(): ...
  |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "3:9-3:12|artificial-attribute-access|qualification:test.bar",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:{|
     def foo():
       yield bar
     def bar(): ...
  |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "3:8-3:11|artificial-attribute-access|qualification:test.bar",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
     def foo(x):
       ... # stub
     def bar():
       pass
     def baz():
       foo(bar)
  |}
           ~define_name:"test.baz"
           ~expected:
             [
               ( "7:2-7:10",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.foo"; kind = Normal });
                        ]
                      ~higher_order_parameters:
                        (HigherOrderParameterMap.from_list
                           [
                             {
                               index = 0;
                               call_targets =
                                 [
                                   CallTarget.create_regular
                                     (Target.Regular.Function { name = "test.bar"; kind = Normal });
                                 ];
                               unresolved = CallGraph.Unresolved.False;
                             };
                           ])
                      ()) );
               ( "7:6-7:9|artificial-attribute-access|qualification:test.bar",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
     def decorator(f):
       def inner():
         return 0
       return inner
     @decorator
     def foo1():
       ...  # Not considered as being decorated due to missing the body
     @decorator
     def foo2():
       return 0
     def identity(x):
       return x
     def bar(b):
       if b:
         return identity(foo1)  # Redirect actual parameters
       else:
         return identity(foo2)
  |}
           ~define_name:"test.bar"
           ~expected:
             [
               ( "16:11-16:25",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.identity"; kind = Normal });
                        ]
                      ~higher_order_parameters:
                        (HigherOrderParameterMap.from_list
                           [
                             {
                               index = 0;
                               call_targets =
                                 [
                                   CallTarget.create_regular
                                     (Target.Regular.Function { name = "test.foo1"; kind = Normal });
                                 ];
                               unresolved = CallGraph.Unresolved.False;
                             };
                           ])
                      ()) );
               ( "16:20-16:24|artificial-attribute-access|qualification:test.foo1",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.foo1"; kind = Normal });
                             ]
                           ())
                      ()) );
               ( "18:11-18:25",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~index:1
                            (Target.Regular.Function { name = "test.identity"; kind = Normal });
                        ]
                      ~higher_order_parameters:
                        (HigherOrderParameterMap.from_list
                           [
                             {
                               index = 0;
                               call_targets =
                                 [
                                   CallTarget.create_regular
                                     (Target.Regular.Function
                                        { name = "test.foo2"; kind = Decorated });
                                 ];
                               unresolved = CallGraph.Unresolved.False;
                             };
                           ])
                      ()) );
               ( "18:20-18:24|artificial-attribute-access|qualification:test.foo2",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.foo2"; kind = Decorated });
                             ]
                           ())
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
     def decorator(f):
       def inner():
         return 0
       return inner
     class C:
       @decorator
       def __init__(self, x):
         print(x)
       @decorator
       def __new__(cls, *args, **kwargs):
         ...
     def foo():
       return C(0)  # Redirect `__init__` and `__new__`
  |}
           ~define_name:"test.foo"
           ~expected:
             [
               ( "14:9-14:13",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~unresolved:(Unresolved.True (BypassingDecorators CannotResolveExports))
                      (* Because Pyre cannot resolve `C.__init__` and `C.__new__`. *)
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
     class A:
       def __call__(self) -> int:
         return 0
     def foo(x):
       pass
     def bar():
       a = A()
       foo(a)
  |}
           ~define_name:"test.bar"
           ~expected:
             [
               ( "8:6-8:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.none)
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__init__"; kind = Normal });
                        ]
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.unknown)
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__new__"; kind = Normal });
                        ]
                      ()) );
               ( "9:2-9:8",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.foo"; kind = Normal });
                        ]
                      ~higher_order_parameters:
                        (HigherOrderParameterMap.from_list
                           [
                             {
                               index = 0;
                               call_targets =
                                 [
                                   CallTarget.create_regular
                                     ~implicit_receiver:true
                                     ~implicit_dunder_call:true
                                     ~receiver_class:"test.A"
                                     ~return_type:(Some ReturnType.integer)
                                     (Target.Regular.Method
                                        {
                                          class_name = "test.A";
                                          method_name = "__call__";
                                          kind = Normal;
                                        });
                                 ];
                               unresolved = CallGraph.Unresolved.False;
                             };
                           ])
                      ()) );
             ]
           ();
      (* Same test as above, but should NOT lead to higher order parameter since
       * the callee has a body and is annotated. *)
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
     class A:
       def __call__(self) -> int:
         return 0
     def foo(x: A):
       pass
     def bar():
       a = A()
       foo(a)
  |}
           ~define_name:"test.bar"
           ~expected:
             [
               ( "8:6-8:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.none)
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__init__"; kind = Normal });
                        ]
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.unknown)
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__new__"; kind = Normal });
                        ]
                      ()) );
               ( "9:2-9:8",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.foo"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
     from typing import Optional
     class A:
       def __call__(self) -> int:
         return 0
     def foo(x: Optional[A]):
       pass
     def bar():
       a = A()
       foo(a)
  |}
           ~define_name:"test.bar"
           ~expected:
             [
               ( "9:6-9:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.none)
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__init__"; kind = Normal });
                        ]
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.unknown)
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__new__"; kind = Normal });
                        ]
                      ()) );
               ( "10:2-10:8",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.foo"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_call_graph_of_define
           ~_migrated_to_pyrefly:false
           ~source:
             {|
     from typing import Optional
     class A:
       def __call__(self) -> int:
         return 0
     class B(A):
       def __call__(self) -> int:
         return 1
     def foo(x: Optional[A]):
       pass
     def bar():
       a = A()
       foo(a)
  |}
           ~define_name:"test.bar"
           ~expected:
             [
               ( "12:6-12:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.none)
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__init__"; kind = Normal });
                        ]
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.unknown)
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__new__"; kind = Normal });
                        ]
                      ()) );
               ( "13:2-13:8",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.foo"; kind = Normal });
                        ]
                      ()) );
             ]
           ();
    ]


let test_higher_order_call_graph_of_define =
  let open CallGraphTestHelper in
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:{|
     def foo():
       raise bar
     def bar():
       pass
  |}
           ~define_name:"test.foo"
           ~expected_call_graph:
             [
               ( "3:8-3:11|artificial-attribute-access|qualification:test.bar",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ~expected_returned_callables:[]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:{|
     async def foo():
       yield bar
     def bar():
       pass
  |}
           ~define_name:"test.foo"
           ~expected_call_graph:
             [
               ( "3:8-3:11|artificial-attribute-access|qualification:test.bar",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ~expected_returned_callables:
             [
               CallTarget.create_regular
                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:{|
     async def foo():
       return bar
     def bar():
       pass
  |}
           ~define_name:"test.foo"
           ~expected_call_graph:
             [
               ( "3:9-3:12|artificial-attribute-access|qualification:test.bar",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ~expected_returned_callables:
             [
               CallTarget.create_regular
                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:
             {|
     async def foo():
       return await bar
     async def bar():
       pass
  |}
           ~define_name:"test.foo"
           ~expected_call_graph:
             [
               ( "3:15-3:18|artificial-attribute-access|qualification:test.bar",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ~expected_returned_callables:
             [
               CallTarget.create_regular
                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:
             {|
     def foo():
       bar(foo, baz)
     def bar(x, y):
       pass
     def baz():
       pass
  |}
           ~define_name:"test.foo"
           ~expected_call_graph:
             [
               ( "3:2-3:15",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create
                            (create_parameterized_target
                               ~regular:
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal })
                               ~parameters:
                                 [
                                   ( create_positional_parameter 0 "x",
                                     Target.Regular.Function { name = "test.foo"; kind = Normal }
                                     |> Target.from_regular );
                                   ( create_positional_parameter 1 "y",
                                     Target.Regular.Function { name = "test.baz"; kind = Normal }
                                     |> Target.from_regular );
                                 ]);
                        ]
                      ()) );
               ( "3:6-3:9|artificial-attribute-access|qualification:test.foo",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.foo"; kind = Normal });
                             ]
                           ())
                      ()) );
               ( "3:11-3:14|artificial-attribute-access|qualification:test.baz",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.baz"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ~expected_returned_callables:[]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:
             {|
     def bar(x, y):
       pass
     def baz(x, y):
       pass
     def foo():
       bar(baz, 0)
       bar(1, 2)
  |}
           ~define_name:"test.foo"
           ~expected_call_graph:
             [
               ( "7:2-7:13",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create
                            (create_parameterized_target
                               ~regular:
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal })
                               ~parameters:
                                 [
                                   ( create_positional_parameter 0 "x",
                                     Target.Regular.Function { name = "test.baz"; kind = Normal }
                                     |> Target.from_regular );
                                 ]);
                        ]
                      ()) );
               ( "7:6-7:9|artificial-attribute-access|qualification:test.baz",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.baz"; kind = Normal });
                             ]
                           ())
                      ()) );
               ( "8:2-8:11",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.bar"; kind = Normal });
                        ]
                      ()) );
             ]
           ~expected_returned_callables:[]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:
             {|
     from builtins import _test_sink
     def foo(x):
       ... # stub
     def bar():
       pass
     def baz():
       foo(bar)
       _test_sink(bar)
  |}
           ~define_name:"test.baz"
           ~expected_call_graph:
             [
               ( "8:2-8:10",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.foo"; kind = Normal });
                        ]
                      ~higher_order_parameters:
                        (HigherOrderParameterMap.from_list
                           [
                             {
                               index = 0;
                               call_targets =
                                 [
                                   CallTarget.create_regular
                                     (Target.Regular.Function { name = "test.bar"; kind = Normal });
                                 ];
                               unresolved = CallGraph.Unresolved.False;
                             };
                           ])
                      ()) );
               ( "8:6-8:9|artificial-attribute-access|qualification:test.bar",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
                             ]
                           ())
                      ()) );
               ( "9:2-9:17",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "_test_sink"; kind = Normal });
                        ]
                      ~higher_order_parameters:
                        (HigherOrderParameterMap.from_list
                           [
                             {
                               index = 0;
                               call_targets =
                                 [
                                   CallTarget.create_regular
                                     ~index:1
                                     (Target.Regular.Function { name = "test.bar"; kind = Normal });
                                 ];
                               unresolved = CallGraph.Unresolved.False;
                             };
                           ])
                      ()) );
               ( "9:13-9:16|artificial-attribute-access|qualification:test.bar",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ~expected_returned_callables:[]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:
             {|
     def decorator(f):
       def inner(x, y):
         return f(y, x)
       return inner
     @decorator
     def foo(x, y):
       y()
     def baz():
       pass
     def bar():
        foo(baz, 0)
  |}
           ~define_name:"test.bar"
           ~expected_call_graph:
             [
               ( "12:3-12:14",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~decorated_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.foo"; kind = Decorated });
                        ]
                      ()) );
               ( "12:7-12:10|artificial-attribute-access|qualification:test.baz",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.baz"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ~expected_returned_callables:[]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:
             {|
     def bar(x, y):
       pass
     def baz():
       pass
     def foo():
       bar(y=baz, x=0)
  |}
           ~define_name:"test.foo"
           ~expected_call_graph:
             [
               ( "7:2-7:17",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create
                            (create_parameterized_target
                               ~regular:
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal })
                               ~parameters:
                                 [
                                   ( create_positional_parameter 1 "y",
                                     Target.Regular.Function { name = "test.baz"; kind = Normal }
                                     |> Target.from_regular );
                                 ]);
                        ]
                      ()) );
               ( "7:8-7:11|artificial-attribute-access|qualification:test.baz",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.baz"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ~expected_returned_callables:[]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:{|
     def bar():
       pass
     def foo(g):
       g(1)
       return g
  |}
           ~define_name:"test.foo"
           ~expected_call_graph:
             [
               ( "5:2-5:6",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.bar"; kind = Normal });
                        ]
                      ~unresolved:CallGraph.Unresolved.False
                      ()) );
             ]
           ~expected_returned_callables:
             [
               CallTarget.create_regular
                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
             ]
           ~initial_state:
             (let callables_to_definitions_map = CallablesSharedMemory.ReadWrite.empty () in
              let initial_state =
                CallGraphBuilder.HigherOrderCallGraph.State.initialize_from_roots
                  ~callables_to_definitions_map:
                    (CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
                  [
                    ( create_positional_parameter 0 "g",
                      Target.Regular.Function { name = "test.bar"; kind = Normal }
                      |> Target.from_regular );
                  ]
              in
              initial_state)
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:
             {|
     def bar(x):
       return x
     def baz():
       return
     def foo():
       return bar(baz)
  |}
           ~define_name:"test.foo"
           ~expected_call_graph:
             [
               ( "7:9-7:17",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create
                            (create_parameterized_target
                               ~regular:
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal })
                               ~parameters:
                                 [
                                   ( create_positional_parameter 0 "x",
                                     Target.Regular.Function { name = "test.baz"; kind = Normal }
                                     |> Target.from_regular );
                                 ]);
                        ]
                      ()) );
               ( "7:13-7:16|artificial-attribute-access|qualification:test.baz",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.baz"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ~expected_returned_callables:[]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:
             {|
     class A:
       @classmethod
       def bar(cls):
         pass
       def baz(self):
         pass
     class B(A):
       pass
     def foo(a: A, b1: bool, b2: bool):
       if b1:
         return A.bar
       elif b2:
         return A().baz
       else:
         return a.baz
  |}
           ~define_name:"test.foo"
           ~expected_call_graph:
             [
               ( "14:11-14:14",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~init_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~return_type:(Some ReturnType.unknown)
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__init__"; kind = Normal });
                        ]
                      ~new_targets:
                        [
                          CallTarget.create_regular
                            ~return_type:(Some ReturnType.unknown)
                            ~is_static_method:true
                            (Target.Regular.Method
                               { class_name = "object"; method_name = "__new__"; kind = Normal });
                        ]
                      ()) );
             ]
           ~expected_returned_callables:[]
             (* TODO(T213339738): Expect resolving attribute access and hence returning `A.bar` and
                `A.baz` *)
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:
             {|
     class C:
       def foo(self):
         return 0
     def bar(c: C):
       return c.foo
  |}
           ~define_name:"test.bar"
           ~expected_call_graph:[] (* TODO(T213339738): Expect resolving attribute access. *)
           ~expected_returned_callables:[]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:
             {|
     def foo():
       return 0
     def bar():
       x = foo  # Test assignments
       return x
  |}
           ~define_name:"test.bar"
           ~expected_call_graph:
             [
               ( "5:6-5:9|artificial-attribute-access|qualification:test.foo",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.foo"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ~expected_returned_callables:
             [
               CallTarget.create_regular
                 (Target.Regular.Function { name = "test.foo"; kind = Normal });
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:
             {|
     from typing import Callable
     def foo():
       return 0
     class C:
       f: Callable
     def bar(c: C):
       c.f = foo  # Test assignments
       return c.f
  |}
           ~define_name:"test.bar"
           ~expected_call_graph:
             [
               ( "8:8-8:11|artificial-attribute-access|qualification:test.foo",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.foo"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ~expected_returned_callables:[]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:
             {|
     def foo():
       return 0
     def bar():
       l = []
       l[0] = foo  # Test assignments
       return l[0]
  |}
           ~define_name:"test.bar"
           ~expected_call_graph:
             [
               ( "6:2-6:12|artificial-call|subscript-set-item",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~receiver_class:"list"
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               { class_name = "list"; method_name = "__setitem__"; kind = Normal });
                        ]
                      ~higher_order_parameters:
                        (HigherOrderParameterMap.from_list
                           [
                             {
                               index = 1;
                               call_targets =
                                 [
                                   CallTarget.create_regular
                                     (Target.Regular.Function { name = "test.foo"; kind = Normal });
                                 ];
                               unresolved = CallGraph.Unresolved.False;
                             };
                           ])
                      ()) );
               ( "6:9-6:12|artificial-attribute-access|qualification:test.foo",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.foo"; kind = Normal });
                             ]
                           ())
                      ()) );
               ( "7:9-7:13|artificial-call|subscript-get-item",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~receiver_class:"list"
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               { class_name = "list"; method_name = "__getitem__"; kind = Normal });
                        ]
                      ()) );
             ]
           ~expected_returned_callables:[]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:
             {|
     def foo():
       return 0
     def bar():
       l = []
       l['key'] = foo  # Test assignments
       return l['key']
  |}
           ~define_name:"test.bar"
           ~expected_call_graph:
             [
               ( "6:2-6:16|artificial-call|subscript-set-item",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~receiver_class:"list"
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               { class_name = "list"; method_name = "__setitem__"; kind = Normal });
                        ]
                      ~higher_order_parameters:
                        (HigherOrderParameterMap.from_list
                           [
                             {
                               index = 1;
                               call_targets =
                                 [
                                   CallTarget.create_regular
                                     (Target.Regular.Function { name = "test.foo"; kind = Normal });
                                 ];
                               unresolved = CallGraph.Unresolved.False;
                             };
                           ])
                      ()) );
               ( "6:13-6:16|artificial-attribute-access|qualification:test.foo",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.foo"; kind = Normal });
                             ]
                           ())
                      ()) );
               ( "7:9-7:17|artificial-call|subscript-get-item",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~receiver_class:"list"
                            ~implicit_receiver:true
                            (Target.Regular.Method
                               { class_name = "list"; method_name = "__getitem__"; kind = Normal });
                        ]
                      ()) );
             ]
           ~expected_returned_callables:[]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:
             {|
     def foo():
       return 0
     def bar():
       x = foo
       x = None  # Test assignments
       return x
  |}
           ~define_name:"test.bar"
           ~expected_call_graph:
             [
               ( "5:6-5:9|artificial-attribute-access|qualification:test.foo",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.foo"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ~expected_returned_callables:[]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:
             {|
     def foo(**kwargs):
       return
     def bar():
       return
     def baz():
       return
     def main():
       return foo(f1=bar, f2=baz)  # Different `formal_path`s under same root `kwarg`
  |}
           ~define_name:"test.main"
           ~expected_call_graph:
             [
               ( "9:9-9:28",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.foo"; kind = Normal });
                        ]
                      ~higher_order_parameters:
                        (HigherOrderParameterMap.from_list
                           [
                             {
                               index = 0;
                               call_targets =
                                 [
                                   CallTarget.create_regular
                                     (Target.Regular.Function { name = "test.bar"; kind = Normal });
                                 ];
                               unresolved = CallGraph.Unresolved.False;
                             };
                             {
                               index = 1;
                               call_targets =
                                 [
                                   CallTarget.create_regular
                                     (Target.Regular.Function { name = "test.baz"; kind = Normal });
                                 ];
                               unresolved = CallGraph.Unresolved.False;
                             };
                           ])
                      ()) );
               ( "9:16-9:19|artificial-attribute-access|qualification:test.bar",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
                             ]
                           ())
                      ()) );
               ( "9:24-9:27|artificial-attribute-access|qualification:test.baz",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.baz"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ~expected_returned_callables:[]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:
             {|
     def foo():
       for x in [1, 2]:
         def dummy_trace():
           return dummy_trace  # Avoid creating "recursive" targets
         dummy_trace()
  |}
           ~define_name:"test.foo"
           ~expected_call_graph:
             [
               ( "3:11-3:17|artificial-call|for-iter",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"list"
                            (Target.Regular.Method
                               { class_name = "list"; method_name = "__iter__"; kind = Normal });
                        ]
                      ()) );
               ( "3:11-3:17|artificial-call|for-next",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"typing.Iterator"
                            ~return_type:(Some ReturnType.integer)
                            (Target.Regular.Method
                               {
                                 class_name = "typing.Iterator";
                                 method_name = "__next__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "4:4-5:24",
                 ExpressionCallees.from_define
                   (DefineCallees.create
                      ~define_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function
                               { name = "test.foo.dummy_trace"; kind = Normal });
                        ]
                      ()) );
               ( "6:4-6:17",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function
                               { name = "test.foo.dummy_trace"; kind = Normal });
                        ]
                      ()) );
             ]
           ~expected_returned_callables:[]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:
             {|
     def foo():
       for x in [1, 2]:
         def bar():
           return baz  # Avoid creating mutually "recursive" targets
         def baz():
           return bar
         bar()
         baz()
  |}
           ~define_name:"test.foo"
           ~expected_call_graph:
             [
               ( "3:11-3:17|artificial-call|for-iter",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"list"
                            (Target.Regular.Method
                               { class_name = "list"; method_name = "__iter__"; kind = Normal });
                        ]
                      ()) );
               ( "3:11-3:17|artificial-call|for-next",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            ~implicit_receiver:true
                            ~receiver_class:"typing.Iterator"
                            ~return_type:(Some ReturnType.integer)
                            (Target.Regular.Method
                               {
                                 class_name = "typing.Iterator";
                                 method_name = "__next__";
                                 kind = Normal;
                               });
                        ]
                      ()) );
               ( "4:4-5:16",
                 ExpressionCallees.from_define
                   (DefineCallees.create
                      ~define_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.foo.bar"; kind = Normal });
                        ]
                      ()) );
               ( "6:4-7:16",
                 ExpressionCallees.from_define
                   (DefineCallees.create
                      ~define_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.foo.baz"; kind = Normal });
                        ]
                      ()) );
               ( "8:4-8:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.foo.bar"; kind = Normal });
                        ]
                      ()) );
               ( "9:4-9:9",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.foo.baz"; kind = Normal });
                          CallTarget.create
                            (create_parameterized_target
                               ~regular:
                                 (Target.Regular.Function { name = "test.foo.baz"; kind = Normal })
                               ~parameters:
                                 [
                                   ( AccessPath.Root.Variable "$local_test?foo$bar",
                                     Target.Regular.Function
                                       { name = "test.foo.bar"; kind = Normal }
                                     |> Target.from_regular );
                                 ]);
                        ]
                      ()) );
             ]
           ~expected_returned_callables:[]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:
             {|
     def foo(f):
       return f
     def bar():
       return
     def baz():
       return
     def main(flag: bool):
       x = bar
       if flag:
         x = baz
       return foo(x)  # Test limiting the number of parameterized call targets
  |}
           ~define_name:"test.main"
           ~maximum_parameterized_targets_at_call_site:1
           ~expected_call_graph:
             [
               ( "9:6-9:9|artificial-attribute-access|qualification:test.bar",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
                             ]
                           ())
                      ()) );
               ( "11:8-11:11|artificial-attribute-access|qualification:test.baz",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.baz"; kind = Normal });
                             ]
                           ())
                      ()) );
               ( "12:9-12:15",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.foo"; kind = Normal });
                        ]
                      ~higher_order_parameters:
                        (HigherOrderParameterMap.from_list
                           [
                             {
                               index = 0;
                               call_targets =
                                 [
                                   CallTarget.create_regular
                                     (Target.Regular.Function { name = "test.bar"; kind = Normal });
                                   CallTarget.create_regular
                                     (Target.Regular.Function { name = "test.baz"; kind = Normal });
                                 ];
                               unresolved = CallGraph.Unresolved.False;
                             };
                           ])
                      ()) );
             ]
           ~expected_returned_callables:[]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:
             {|
     def foo(f):
       return f
     def bar():
       return
     def main():
       foo(bar)  # Test keeping higher order parameters
  |}
           ~define_name:"test.main"
           ~called_when_parameter:
             ([Target.Regular.Function { name = "test.bar"; kind = Normal } |> Target.from_regular]
             |> Target.HashSet.of_list)
           ~expected_call_graph:
             [
               ( "7:2-7:10",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.foo"; kind = Normal });
                        ]
                      ~higher_order_parameters:
                        (HigherOrderParameterMap.from_list
                           [
                             {
                               index = 0;
                               call_targets =
                                 [
                                   CallTarget.create_regular
                                     (Target.Regular.Function { name = "test.bar"; kind = Normal });
                                 ];
                               unresolved = CallGraph.Unresolved.False;
                             };
                           ])
                      ()) );
               ( "7:6-7:9|artificial-attribute-access|qualification:test.bar",
                 ExpressionCallees.from_attribute_access
                   (AttributeAccessCallees.create
                      ~if_called:
                        (CallCallees.create
                           ~call_targets:
                             [
                               CallTarget.create_regular
                                 (Target.Regular.Function { name = "test.bar"; kind = Normal });
                             ]
                           ())
                      ()) );
             ]
           ~expected_returned_callables:[]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_of_define
           ~source:
             {|
     def foo(f):
       ...  # We don't create parameterized targets for stubs
     def bar():
       return
     def baz(f):
       foo(f)  # Test creating additional higher order parameters
     def main():
       baz(bar)
  |}
           ~define_name:"test.baz"
           ~callable:
             (create_parameterized_target
                ~regular:(Target.Regular.Function { name = "test.baz"; kind = Normal })
                ~parameters:
                  [
                    ( create_positional_parameter 0 "f",
                      Target.Regular.Function { name = "test.bar"; kind = Normal }
                      |> Target.from_regular );
                  ])
           ~expected_call_graph:
             [
               ( "7:2-7:8",
                 ExpressionCallees.from_call
                   (CallCallees.create
                      ~call_targets:
                        [
                          CallTarget.create_regular
                            (Target.Regular.Function { name = "test.foo"; kind = Normal });
                        ]
                      ~higher_order_parameters:
                        (HigherOrderParameterMap.from_list
                           [ (* TODO(T223511074): Expect `bar` here. *) ])
                      ()) );
             ]
           ~expected_returned_callables:[]
           ();
    ]


let assert_resolve_decorator_callees
    ?(debug = false)
    ?((* Whether to run this test with PyreflyApi (i.e import call graphs from pyrefly) *)
      skip_for_pyrefly = true)
    ?pyrefly_expected
    ~source
    ~expected
    ()
    context
  =
  let _, pyre_api, configuration =
    TestHelper.setup_single_py_file
      ~force_pyre1:skip_for_pyrefly
      ~requires_type_of_expressions:false
      ~file_name:"test.py"
      ~context
      ~source
      ()
  in
  let static_analysis_configuration =
    Configuration.StaticAnalysis.create
      ~maximum_target_depth:Configuration.StaticAnalysis.default_maximum_target_depth
      configuration
      ()
  in
  let qualifier = Reference.create "test" in
  let initial_callables = FetchCallables.from_qualifier ~configuration ~pyre_api ~qualifier in
  let override_graph_shared_memory =
    qualifier
    |> OverrideGraph.Heap.from_qualifier
         ~pyre_api
         ~skip_overrides_targets:Reference.SerializableSet.empty
    |> OverrideGraph.SharedMemory.from_heap
  in
  let module TestResult = struct
    type t =
      | Decorators of {
          return_expression: string;
          define_name: string;
          callable: Target.t;
          call_graph: CallGraph.DefineCallGraphForTest.t;
        }
      | Undecorated
    [@@deriving show, equal]

    let from_expected = function
      | Some (decorator_call, callable, define_name, call_graph) ->
          Decorators
            {
              return_expression = decorator_call;
              callable = Target.from_regular callable;
              define_name;
              call_graph = DefineCallGraphForTest.from_expected call_graph;
            }
      | None -> Undecorated
  end
  in
  let definitions = FetchCallables.get_definitions initial_callables in
  let definitions_and_stubs = FetchCallables.get ~definitions:true ~stubs:true initial_callables in
  let scheduler = Test.mock_scheduler () in
  let scheduler_policy = Scheduler.Policy.legacy_fixed_chunk_count () in
  let callables_to_definitions_map =
    CallablesSharedMemory.ReadWrite.from_callables
      ~scheduler
      ~scheduler_policy
      ~pyre_api
      definitions_and_stubs
  in
  let type_of_expression_shared_memory =
    Interprocedural.TypeOfExpressionSharedMemory.create
      ~pyre_api
      ~callables_to_definitions_map:
        (CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
      ()
  in
  let callables_to_decorators_map =
    CallableToDecoratorsMap.SharedMemory.create
      ~callables_to_definitions_map:
        (CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
      ~scheduler
      ~scheduler_policy
      ~is_pyrefly:(PyrePysaApi.ReadOnly.is_pyrefly pyre_api)
      definitions_and_stubs
  in
  let pyrefly_define_call_graphs =
    match pyre_api with
    | PyrePysaApi.ReadOnly.Pyre1 _ -> None
    | PyrePysaApi.ReadOnly.Pyrefly _ ->
        let { CallGraph.SharedMemory.define_call_graphs; _ } =
          CallGraphBuilder.build_whole_program_call_graph
            ~scheduler
            ~static_analysis_configuration
            ~pyre_api
            ~resolve_module_path:None
            ~callables_to_definitions_map:
              (CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
            ~callables_to_decorators_map:
              (CallableToDecoratorsMap.SharedMemory.read_only callables_to_decorators_map)
            ~type_of_expression_shared_memory
            ~override_graph:
              (Some
                 (Interprocedural.OverrideGraph.SharedMemory.read_only override_graph_shared_memory))
            ~store_shared_memory:true
            ~attribute_targets:Target.Set.empty
            ~skip_analysis_targets:(Target.HashSet.create ())
            ~check_invariants:true
            ~definitions
            ~create_dependency_for:CallGraph.AllTargetsUseCase.Everything
        in
        Some define_call_graphs
  in
  let actual =
    definitions
    |> List.map ~f:(fun callable ->
           let open Option.Monad_infix in
           (* For simplicity, don't compare the cases when there exist no decorator callees. *)
           callable
           |> CallableToDecoratorsMap.SharedMemory.decorated_callable_body
                (CallableToDecoratorsMap.SharedMemory.read_only callables_to_decorators_map)
           >>| (fun ({
                       CallableToDecoratorsMap.DecoratedDefineBody.return_expression;
                       define_name;
                       decorated_callable;
                       _;
                     } as body) ->
                 let call_graph =
                   match pyre_api with
                   | PyrePysaApi.ReadOnly.Pyre1 _ ->
                       CallGraphBuilder.call_graph_of_decorated_callable
                         ~debug
                         ~pyre_api
                         ~override_graph:
                           (Some (OverrideGraph.SharedMemory.read_only override_graph_shared_memory))
                         ~callables_to_definitions_map:
                           (CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
                         ~type_of_expression_shared_memory
                         ~callables_to_decorators_map:
                           (CallableToDecoratorsMap.SharedMemory.read_only
                              callables_to_decorators_map)
                         ~callable
                         ~body
                   | PyrePysaApi.ReadOnly.Pyrefly _ ->
                       let define_call_graphs =
                         pyrefly_define_call_graphs
                         |> Option.value_exn ~message:"missing define call graphs from pyrefly"
                         |> CallGraph.SharedMemory.read_only
                       in
                       CallGraph.SharedMemory.ReadOnly.get
                         define_call_graphs
                         ~cache:false
                         ~callable:decorated_callable
                       |> Option.value_exn ~message:"missing call graph for decorated target"
                 in

                 TestResult.Decorators
                   {
                     return_expression = Expression.show return_expression;
                     define_name = Reference.show define_name;
                     callable = decorated_callable;
                     call_graph = DefineCallGraph.for_test call_graph;
                   })
           |> Option.value ~default:TestResult.Undecorated
           |> fun result -> callable, result)
    |> Target.Map.of_alist_exn
  in
  let expected =
    match pyrefly_expected with
    | Some pyrefly_expected when PyrePysaApi.ReadOnly.is_pyrefly pyre_api -> pyrefly_expected
    | _ -> expected
  in
  let expected =
    expected
    |> List.map ~f:(fun (callable, expected) ->
           Target.from_regular callable, TestResult.from_expected expected)
    |> Target.Map.of_alist_exn
  in
  OverrideGraph.SharedMemory.cleanup override_graph_shared_memory;
  CallablesSharedMemory.ReadWrite.cleanup callables_to_definitions_map;
  assert_equal
    ~cmp:(Target.Map.equal TestResult.equal)
    ~printer:(Format.asprintf "%a" (Target.Map.pp TestResult.pp))
    expected
    actual


let test_resolve_decorator_callees =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_resolve_decorator_callees
           ~skip_for_pyrefly:false
           ~source:
             {|
     def decorator(f1):
       def inner():
         ...
       return inner
     def decorator2(f2):
       def inner():
         ...
       return inner
     @decorator2
     @decorator
     def foo():
       return 0
  |}
           ~expected:
             [
               Target.Regular.Function { name = "test.$toplevel"; kind = Normal }, None;
               Target.Regular.Function { name = "test.decorator"; kind = Normal }, None;
               Target.Regular.Function { name = "test.decorator2"; kind = Normal }, None;
               ( Target.Regular.Function { name = "test.foo"; kind = Normal },
                 Some
                   ( "test.decorator2(test.decorator(test.foo))",
                     Target.Regular.Function { name = "test.foo"; kind = Decorated },
                     "test.foo.@decorated",
                     [
                       ( "12:0-13:10|artificial-attribute-access|for-decorated-target-callee:test.foo",
                         ExpressionCallees.from_attribute_access
                           (AttributeAccessCallees.create
                              ~if_called:
                                (CallCallees.create
                                   ~call_targets:
                                     [
                                       CallTarget.create_regular
                                         (Target.Regular.Function
                                            { name = "test.foo"; kind = Normal });
                                     ]
                                   ())
                              ()) );
                       ( "10:1-10:11|artificial-call|for-decorated-target",
                         ExpressionCallees.from_call
                           (CallCallees.create
                              ~call_targets:
                                [
                                  CallTarget.create_regular
                                    (Target.Regular.Function
                                       { name = "test.decorator2"; kind = Normal });
                                ]
                              ()) );
                       ( "11:1-11:10|artificial-call|for-decorated-target",
                         ExpressionCallees.from_call
                           (CallCallees.create
                              ~call_targets:
                                [
                                  CallTarget.create_regular
                                    (Target.Regular.Function
                                       { name = "test.decorator"; kind = Normal });
                                ]
                              ~higher_order_parameters:
                                (HigherOrderParameterMap.from_list
                                   [
                                     {
                                       index = 0;
                                       call_targets =
                                         [
                                           CallTarget.create_regular
                                             (Target.Regular.Function
                                                { name = "test.foo"; kind = Normal });
                                         ];
                                       unresolved = CallGraph.Unresolved.False;
                                     };
                                   ])
                              ()) );
                     ] ) );
             ]
           ~pyrefly_expected:
             [
               Target.Regular.Function { name = "test.$toplevel"; kind = Normal }, None;
               Target.Regular.Function { name = "test.decorator"; kind = Normal }, None;
               Target.Regular.Function { name = "test.decorator2"; kind = Normal }, None;
               ( Target.Regular.Function { name = "test.foo"; kind = Normal },
                 Some
                   ( "decorator2(decorator(foo))",
                     Target.Regular.Function { name = "test.foo"; kind = Decorated },
                     "test.foo.@decorated",
                     [
                       ( "12:0-13:10|identifier|foo",
                         ExpressionCallees.from_identifier
                           (IdentifierCallees.create
                              ~if_called:
                                (CallCallees.create
                                   ~call_targets:
                                     [
                                       CallTarget.create_regular
                                         (Target.Regular.Function
                                            { name = "test.foo"; kind = Normal });
                                     ]
                                   ())
                              ()) );
                       ( "10:1-10:11|artificial-call|for-decorated-target",
                         ExpressionCallees.from_call
                           (CallCallees.create
                              ~call_targets:
                                [
                                  CallTarget.create_regular
                                    (Target.Regular.Function
                                       { name = "test.decorator2"; kind = Normal });
                                ]
                              ()) );
                       ( "11:1-11:10|artificial-call|for-decorated-target",
                         ExpressionCallees.from_call
                           (CallCallees.create
                              ~call_targets:
                                [
                                  CallTarget.create_regular
                                    (Target.Regular.Function
                                       { name = "test.decorator"; kind = Normal });
                                ]
                              ()) );
                     ] ) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_resolve_decorator_callees
           ~source:
             {|
     from abc import abstractmethod
     @property
     @classmethod
     @abstractmethod
     def foo():
       return 0
  |}
           ~expected:
             [
               Target.Regular.Function { name = "test.$toplevel"; kind = Normal }, None;
               Target.Regular.Function { name = "test.foo"; kind = Normal }, None;
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_resolve_decorator_callees
           ~source:
             {|
     def decorator_factory(arg1, arg2):
       def decorator():
         ...
       return decorator
     @decorator_factory(1, 2)
     def foo():
       return 0
  |}
           ~expected:
             [
               Target.Regular.Function { name = "test.$toplevel"; kind = Normal }, None;
               Target.Regular.Function { name = "test.decorator_factory"; kind = Normal }, None;
               ( Target.Regular.Function { name = "test.foo"; kind = Normal },
                 Some
                   ( "test.decorator_factory(1, 2)(test.foo)",
                     Target.Regular.Function { name = "test.foo"; kind = Decorated },
                     "test.foo.@decorated",
                     [
                       ( "7:0-8:10|artificial-attribute-access|for-decorated-target-callee:test.foo",
                         ExpressionCallees.from_attribute_access
                           (AttributeAccessCallees.create
                              ~if_called:
                                (CallCallees.create
                                   ~call_targets:
                                     [
                                       CallTarget.create_regular
                                         (Target.Regular.Function
                                            { name = "test.foo"; kind = Normal });
                                     ]
                                   ())
                              ()) );
                       ( "6:1-6:24",
                         ExpressionCallees.from_call
                           (CallCallees.create
                              ~call_targets:
                                [
                                  CallTarget.create_regular
                                    (Target.Regular.Function
                                       { name = "test.decorator_factory"; kind = Normal });
                                ]
                              ()) );
                       ( "6:1-6:24|artificial-call|for-decorated-target",
                         ExpressionCallees.from_call
                           (CallCallees.create
                              ~unresolved:
                                (CallGraph.Unresolved.True (BypassingDecorators UnknownCallCallee))
                              ~higher_order_parameters:
                                (HigherOrderParameterMap.from_list
                                   [
                                     {
                                       index = 0;
                                       call_targets =
                                         [
                                           CallTarget.create_regular
                                             (Target.Regular.Function
                                                { name = "test.foo"; kind = Normal });
                                         ];
                                       unresolved = CallGraph.Unresolved.False;
                                     };
                                   ])
                              ()) );
                     ] ) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_resolve_decorator_callees
           ~skip_for_pyrefly:false
           ~source:
             {|
     def decorator_factory(arg1, arg2):
       def decorator():
         ...
       return decorator
     def bar():
       ...
     @decorator_factory(1, bar)
     def foo():
       return 0
  |}
           ~expected:
             [
               Target.Regular.Function { name = "test.$toplevel"; kind = Normal }, None;
               Target.Regular.Function { name = "test.decorator_factory"; kind = Normal }, None;
               ( Target.Regular.Function { name = "test.foo"; kind = Normal },
                 Some
                   ( "test.decorator_factory(1, test.bar)(test.foo)",
                     Target.Regular.Function { name = "test.foo"; kind = Decorated },
                     "test.foo.@decorated",
                     [
                       ( "9:0-10:10|artificial-attribute-access|for-decorated-target-callee:test.foo",
                         ExpressionCallees.from_attribute_access
                           (AttributeAccessCallees.create
                              ~if_called:
                                (CallCallees.create
                                   ~call_targets:
                                     [
                                       CallTarget.create_regular
                                         (Target.Regular.Function
                                            { name = "test.foo"; kind = Normal });
                                     ]
                                   ())
                              ()) );
                       ( "8:22-8:25|artificial-attribute-access|qualification:test.bar",
                         ExpressionCallees.from_attribute_access
                           (AttributeAccessCallees.create
                              ~if_called:
                                (CallCallees.create
                                   ~call_targets:
                                     [
                                       CallTarget.create_regular
                                         (Target.Regular.Function
                                            { name = "test.bar"; kind = Normal });
                                     ]
                                   ())
                              ()) );
                       ( "8:1-8:26",
                         ExpressionCallees.from_call
                           (CallCallees.create
                              ~call_targets:
                                [
                                  CallTarget.create_regular
                                    (Target.Regular.Function
                                       { name = "test.decorator_factory"; kind = Normal });
                                ]
                              ~higher_order_parameters:
                                (HigherOrderParameterMap.from_list
                                   [
                                     {
                                       index = 1;
                                       call_targets =
                                         [
                                           CallTarget.create_regular
                                             (Target.Regular.Function
                                                { name = "test.bar"; kind = Normal });
                                         ];
                                       unresolved = CallGraph.Unresolved.False;
                                     };
                                   ])
                              ()) );
                       ( "8:1-8:26|artificial-call|for-decorated-target",
                         ExpressionCallees.from_call
                           (CallCallees.create
                              ~unresolved:
                                (CallGraph.Unresolved.True (BypassingDecorators UnknownCallCallee))
                              ~higher_order_parameters:
                                (HigherOrderParameterMap.from_list
                                   [
                                     {
                                       index = 0;
                                       call_targets =
                                         [
                                           CallTarget.create_regular
                                             (Target.Regular.Function
                                                { name = "test.foo"; kind = Normal });
                                         ];
                                       unresolved = CallGraph.Unresolved.False;
                                     };
                                   ])
                              ()) );
                     ] ) );
             ]
           ~pyrefly_expected:
             [
               Target.Regular.Function { name = "test.$toplevel"; kind = Normal }, None;
               Target.Regular.Function { name = "test.decorator_factory"; kind = Normal }, None;
               ( Target.Regular.Function { name = "test.foo"; kind = Normal },
                 Some
                   ( "decorator_factory(1, bar)(foo)",
                     Target.Regular.Function { name = "test.foo"; kind = Decorated },
                     "test.foo.@decorated",
                     [
                       ( "9:0-10:10|identifier|foo",
                         ExpressionCallees.from_identifier
                           (IdentifierCallees.create
                              ~if_called:
                                (CallCallees.create
                                   ~call_targets:
                                     [
                                       CallTarget.create_regular
                                         (Target.Regular.Function
                                            { name = "test.foo"; kind = Normal });
                                     ]
                                   ())
                              ()) );
                       ( "8:1-8:26",
                         ExpressionCallees.from_call
                           (CallCallees.create
                              ~call_targets:
                                [
                                  CallTarget.create_regular
                                    (Target.Regular.Function
                                       { name = "test.decorator_factory"; kind = Normal });
                                ]
                              ~higher_order_parameters:
                                (HigherOrderParameterMap.from_list
                                   [
                                     {
                                       index = 1;
                                       call_targets =
                                         [
                                           CallTarget.create_regular
                                             (Target.Regular.Function
                                                { name = "test.bar"; kind = Normal });
                                         ];
                                       unresolved = CallGraph.Unresolved.False;
                                     };
                                   ])
                              ()) );
                       ( "8:1-8:26|artificial-call|for-decorated-target",
                         ExpressionCallees.from_call
                           (CallCallees.create
                              ~unresolved:(CallGraph.Unresolved.True UnexpectedCalleeExpression)
                              ()) );
                       ( "8:22-8:25",
                         ExpressionCallees.from_identifier
                           (IdentifierCallees.create
                              ~if_called:
                                (CallCallees.create
                                   ~call_targets:
                                     [
                                       CallTarget.create_regular
                                         (Target.Regular.Function
                                            { name = "test.bar"; kind = Normal });
                                     ]
                                   ())
                              ()) );
                     ] ) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_resolve_decorator_callees
           ~source:
             {|
     class ClassDecorator:
       def __init__(self, f):
         self.f = f
       def __call__(self, *args, **kwargs):
         return self.f(*args, **kwargs)
     @ClassDecorator
     def foo():
       return 0
  |}
           ~expected:
             [
               Target.Regular.Function { name = "test.$toplevel"; kind = Normal }, None;
               ( Target.Regular.Method
                   {
                     class_name = "test.ClassDecorator";
                     method_name = "$class_toplevel";
                     kind = Normal;
                   },
                 None );
               ( Target.Regular.Method
                   { class_name = "test.ClassDecorator"; method_name = "__call__"; kind = Normal },
                 None );
               ( Target.Regular.Method
                   { class_name = "test.ClassDecorator"; method_name = "__init__"; kind = Normal },
                 None );
               ( Target.Regular.Function { name = "test.foo"; kind = Normal },
                 Some
                   ( "test.ClassDecorator(test.foo)",
                     Target.Regular.Function { name = "test.foo"; kind = Decorated },
                     "test.foo.@decorated",
                     [
                       ( "8:0-9:10|artificial-attribute-access|for-decorated-target-callee:test.foo",
                         ExpressionCallees.from_attribute_access
                           (AttributeAccessCallees.create
                              ~if_called:
                                (CallCallees.create
                                   ~call_targets:
                                     [
                                       CallTarget.create_regular
                                         (Target.Regular.Function
                                            { name = "test.foo"; kind = Normal });
                                     ]
                                   ())
                              ()) );
                       ( "7:1-7:15|artificial-call|for-decorated-target",
                         ExpressionCallees.from_call
                           (CallCallees.create
                              ~init_targets:
                                [
                                  CallTarget.create_regular
                                    ~implicit_receiver:true
                                    ~return_type:(Some ReturnType.unknown)
                                    (Target.Regular.Method
                                       {
                                         class_name = "test.ClassDecorator";
                                         method_name = "__init__";
                                         kind = Normal;
                                       });
                                ]
                              ~new_targets:
                                [
                                  CallTarget.create_regular
                                    ~return_type:(Some ReturnType.unknown)
                                    ~is_static_method:true
                                    (Target.Regular.Method
                                       {
                                         class_name = "object";
                                         method_name = "__new__";
                                         kind = Normal;
                                       });
                                ]
                              ~higher_order_parameters:
                                (HigherOrderParameterMap.from_list
                                   [
                                     {
                                       index = 0;
                                       call_targets =
                                         [
                                           CallTarget.create_regular
                                             ~implicit_receiver:true
                                             ~implicit_dunder_call:true
                                             ~receiver_class:"test.ClassDecorator"
                                             (Target.Regular.Method
                                                {
                                                  class_name = "test.ClassDecorator";
                                                  method_name = "__call__";
                                                  kind = Normal;
                                                });
                                         ];
                                       unresolved = CallGraph.Unresolved.False;
                                     };
                                   ])
                              ()) );
                     ] ) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_resolve_decorator_callees
           ~source:
             {|
     class A:
       @classmethod
       def decorator(cls, f):
         def inner():
           ...
         return inner
     @A.decorator
     def foo():
       return 0
  |}
           ~expected:
             [
               Target.Regular.Function { name = "test.$toplevel"; kind = Normal }, None;
               ( Target.Regular.Method
                   { class_name = "test.A"; method_name = "$class_toplevel"; kind = Normal },
                 None );
               ( Target.Regular.Method
                   { class_name = "test.A"; method_name = "decorator"; kind = Normal },
                 None );
               ( Target.Regular.Function { name = "test.foo"; kind = Normal },
                 Some
                   ( "test.A.decorator(test.foo)",
                     Target.Regular.Function { name = "test.foo"; kind = Decorated },
                     "test.foo.@decorated",
                     [
                       ( "9:0-10:10|artificial-attribute-access|for-decorated-target-callee:test.foo",
                         ExpressionCallees.from_attribute_access
                           (AttributeAccessCallees.create
                              ~if_called:
                                (CallCallees.create
                                   ~call_targets:
                                     [
                                       CallTarget.create_regular
                                         (Target.Regular.Function
                                            { name = "test.foo"; kind = Normal });
                                     ]
                                   ())
                              ()) );
                       ( "8:1-8:12|artificial-call|for-decorated-target",
                         ExpressionCallees.from_call
                           (CallCallees.create
                              ~call_targets:
                                [
                                  CallTarget.create_regular
                                    ~receiver_class:"test.A"
                                    ~implicit_receiver:true
                                    ~is_class_method:true
                                    (Target.Regular.Method
                                       {
                                         class_name = "test.A";
                                         method_name = "decorator";
                                         kind = Normal;
                                       });
                                ]
                              ~higher_order_parameters:
                                (HigherOrderParameterMap.from_list
                                   [
                                     {
                                       index = 0;
                                       call_targets =
                                         [
                                           CallTarget.create_regular
                                             (Target.Regular.Function
                                                { name = "test.foo"; kind = Normal });
                                         ];
                                       unresolved = CallGraph.Unresolved.False;
                                     };
                                   ])
                              ()) );
                     ] ) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_resolve_decorator_callees
           ~source:
             {|
     class A:
       def decorator(self, f):
         def inner():
           ...
         return inner

       @decorator
       def foo(self):
         return 0
  |}
           ~expected:
             [
               Target.Regular.Function { name = "test.$toplevel"; kind = Normal }, None;
               ( Target.Regular.Method
                   { class_name = "test.A"; method_name = "$class_toplevel"; kind = Normal },
                 None );
               ( Target.Regular.Method
                   { class_name = "test.A"; method_name = "decorator"; kind = Normal },
                 None );
               ( Target.Regular.Method { class_name = "test.A"; method_name = "foo"; kind = Normal },
                 Some
                   ( "test.A.decorator(test.A.foo)",
                     Target.Regular.Method
                       { class_name = "test.A"; method_name = "foo"; kind = Decorated },
                     "test.A.foo.@decorated",
                     [
                       ( "8:3-8:12|artificial-call|for-decorated-target",
                         ExpressionCallees.from_call
                           (CallCallees.create
                              ~call_targets:
                                [
                                  CallTarget.create_regular
                                    (Target.Regular.Method
                                       {
                                         class_name = "test.A";
                                         method_name = "decorator";
                                         kind = Normal;
                                       });
                                ]
                              ~higher_order_parameters:
                                (HigherOrderParameterMap.from_list
                                   [
                                     {
                                       index = 0;
                                       call_targets =
                                         [
                                           CallTarget.create_regular
                                             ~implicit_receiver:true
                                             (Target.Regular.Method
                                                {
                                                  class_name = "test.A";
                                                  method_name = "foo";
                                                  kind = Normal;
                                                });
                                         ];
                                       unresolved = CallGraph.Unresolved.False;
                                     };
                                   ])
                              ()) );
                       ( "9:2-10:12|artificial-attribute-access|for-decorated-target-callee:test.A.foo",
                         ExpressionCallees.from_attribute_access
                           (AttributeAccessCallees.create
                              ~if_called:
                                (CallCallees.create
                                   ~call_targets:
                                     [
                                       CallTarget.create_regular
                                         ~implicit_receiver:true
                                         (Target.Regular.Method
                                            {
                                              class_name = "test.A";
                                              method_name = "foo";
                                              kind = Normal;
                                            });
                                     ]
                                   ())
                              ()) );
                     ] ) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_resolve_decorator_callees
           ~source:
             {|
     import typing
     class A:
       @staticmethod
       def decorator(f): ...
     class B:
       @staticmethod
       def decorator(f): ...
     a_or_b: typing.Union[A, B] = A() if 1 > 2 else B()
     @a_or_b.decorator
     def foo():
       return 0
  |}
           ~expected:
             [
               Target.Regular.Function { name = "test.$toplevel"; kind = Normal }, None;
               ( Target.Regular.Method
                   { class_name = "test.A"; method_name = "$class_toplevel"; kind = Normal },
                 None );
               ( Target.Regular.Method
                   { class_name = "test.B"; method_name = "$class_toplevel"; kind = Normal },
                 None );
               ( Target.Regular.Function { name = "test.foo"; kind = Normal },
                 Some
                   ( "test.a_or_b.decorator(test.foo)",
                     Target.Regular.Function { name = "test.foo"; kind = Decorated },
                     "test.foo.@decorated",
                     [
                       ( "11:0-12:10|artificial-attribute-access|for-decorated-target-callee:test.foo",
                         ExpressionCallees.from_attribute_access
                           (AttributeAccessCallees.create
                              ~if_called:
                                (CallCallees.create
                                   ~call_targets:
                                     [
                                       CallTarget.create_regular
                                         (Target.Regular.Function
                                            { name = "test.foo"; kind = Normal });
                                     ]
                                   ())
                              ()) );
                       ( "10:1-10:17|artificial-call|for-decorated-target",
                         ExpressionCallees.from_call
                           (CallCallees.create
                              ~call_targets:
                                [
                                  CallTarget.create_regular
                                    ~is_static_method:true
                                    (Target.Regular.Method
                                       {
                                         class_name = "test.A";
                                         method_name = "decorator";
                                         kind = Normal;
                                       });
                                  CallTarget.create_regular
                                    ~is_static_method:true
                                    (Target.Regular.Method
                                       {
                                         class_name = "test.B";
                                         method_name = "decorator";
                                         kind = Normal;
                                       });
                                ]
                              ~higher_order_parameters:
                                (HigherOrderParameterMap.from_list
                                   [
                                     {
                                       index = 0;
                                       call_targets =
                                         [
                                           CallTarget.create_regular
                                             (Target.Regular.Function
                                                { name = "test.foo"; kind = Normal });
                                         ];
                                       unresolved = CallGraph.Unresolved.False;
                                     };
                                   ])
                              ()) );
                     ] ) );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_resolve_decorator_callees
           ~source:
             {|
     class MyClass(object):
       @property
       def my_attr(self):
           return self._my_attr
       @my_attr.setter
       def my_attr(self, value):
           self._my_attr = value
  |}
           ~expected:
             [
               Target.Regular.Function { name = "test.$toplevel"; kind = Normal }, None;
               ( Target.Regular.Method
                   { class_name = "test.MyClass"; method_name = "$class_toplevel"; kind = Normal },
                 None );
               ( Target.Regular.Method
                   { class_name = "test.MyClass"; method_name = "my_attr"; kind = Normal },
                 None );
               ( Target.Regular.Method
                   {
                     class_name = "test.MyClass";
                     method_name = "my_attr";
                     kind = Pyre1PropertySetter;
                   },
                 None );
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_resolve_decorator_callees
           ~source:
             {|
     def decorator(f):
       def inner(x):
         return f(x)
       return inner
     def main():
       @decorator  # Test creating decorated targets for inner functions
       def inner(x):
         return
  |}
           ~expected:
             [
               Target.Regular.Function { name = "test.$toplevel"; kind = Normal }, None;
               Target.Regular.Function { name = "test.decorator"; kind = Normal }, None;
               Target.Regular.Function { name = "test.decorator.inner"; kind = Normal }, None;
               Target.Regular.Function { name = "test.main"; kind = Normal }, None;
               ( Target.Regular.Function { name = "test.main.inner"; kind = Normal },
                 Some
                   ( "test.decorator(test.main.inner)",
                     Target.Regular.Function { name = "test.main.inner"; kind = Decorated },
                     "test.main.inner.@decorated",
                     [
                       ( "7:3-7:12|artificial-call|for-decorated-target",
                         ExpressionCallees.from_call
                           (CallCallees.create
                              ~call_targets:
                                [
                                  CallTarget.create_regular
                                    (Target.Regular.Function
                                       { name = "test.decorator"; kind = Normal });
                                ]
                              ()) );
                       ( "8:2-9:10|artificial-attribute-access|for-decorated-target-callee:test.main",
                         ExpressionCallees.from_attribute_access
                           (AttributeAccessCallees.create
                              ~if_called:
                                (CallCallees.create
                                   ~call_targets:
                                     [
                                       CallTarget.create_regular
                                         (Target.Regular.Function
                                            { name = "test.main"; kind = Normal });
                                     ]
                                   ())
                              ()) );
                       ( "8:2-9:10|artificial-attribute-access|for-decorated-target-callee:test.main.inner",
                         ExpressionCallees.from_attribute_access
                           (AttributeAccessCallees.create
                              ~if_called:
                                (CallCallees.create
                                   ~call_targets:
                                     [
                                       CallTarget.create_regular
                                         (Target.Regular.Function
                                            { name = "test.main.inner"; kind = Normal });
                                     ]
                                   ())
                              ()) );
                     ] ) );
             ]
           ();
    ]


let () =
  "interproceduralCallGraph"
  >::: [
         test_call_graph_of_define;
         test_call_graph_of_define_foo_and_bar;
         test_higher_order_call_graph_of_define;
         test_resolve_decorator_callees;
       ]
  |> Test.run
