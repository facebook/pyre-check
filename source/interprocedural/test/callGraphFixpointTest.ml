(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Test
open Interprocedural
open CallGraph
open CallGraphBuilder
open CallGraphTestHelper

module Expected = struct
  type t = {
    callable: Target.t;
    returned_callables: CallTarget.t list;
    call_graph: (string * ExpressionCallees.t) list;
  }
end

let assert_higher_order_call_graph_fixpoint
    ?(max_iterations = 10)
    ?(skip_analysis_targets = Target.HashSet.create ())
    ?(called_when_parameter = Target.HashSet.create ())
    ?(maximum_target_depth = Configuration.StaticAnalysis.default_maximum_target_depth)
    ~source
    ~expected
    ()
    context
  =
  let _, pyre_api, configuration =
    TestHelper.setup_single_py_file
      ~force_pyre1:true
      ~requires_type_of_expressions:false
      ~file_name:"test.py"
      ~context
      ~source
      ()
  in
  let static_analysis_configuration =
    Configuration.StaticAnalysis.create
      ~maximum_target_depth
      ~higher_order_call_graph_max_iterations:max_iterations
      configuration
      ()
  in
  let qualifier = Ast.Reference.create "test" in
  let override_graph_heap =
    OverrideGraph.Heap.from_qualifier
      ~pyre_api
      ~skip_overrides_targets:Ast.Reference.SerializableSet.empty
      qualifier
  in
  let override_graph_shared_memory = OverrideGraph.SharedMemory.from_heap override_graph_heap in
  let initial_callables = FetchCallables.from_qualifier ~configuration ~pyre_api ~qualifier in
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
  let ({ SharedMemory.whole_program_call_graph; define_call_graphs } as call_graph) =
    CallGraphBuilder.build_whole_program_call_graph
      ~scheduler
      ~static_analysis_configuration
      ~pyre_api
      ~resolve_module_path:None
      ~override_graph:(Some (OverrideGraph.SharedMemory.read_only override_graph_shared_memory))
      ~store_shared_memory:true
      ~attribute_targets:Target.Set.empty
      ~skip_analysis_targets
      ~definitions
      ~callables_to_definitions_map:
        (CallablesSharedMemory.ReadOnly.read_only callables_to_definitions_map)
      ~callables_to_decorators_map:
        (CallableToDecoratorsMap.SharedMemory.read_only callables_to_decorators_map)
      ~type_of_expression_shared_memory
      ~check_invariants:true
      ~create_dependency_for:Interprocedural.CallGraph.AllTargetsUseCase.CallGraphDependency
  in
  let dependency_graph =
    DependencyGraph.build_whole_program_dependency_graph
      ~static_analysis_configuration
      ~prune:DependencyGraph.PruneMethod.None
      ~initial_callables
      ~call_graph:whole_program_call_graph
      ~overrides:override_graph_heap
      ~ignore_decorated_targets:false
  in
  let fixpoint_state =
    CallGraphFixpoint.compute
      ~scheduler
      ~scheduler_policy
      ~static_analysis_configuration
      ~resolve_module_path:None
      ~pyre_api
      ~call_graph
      ~dependency_graph
      ~override_graph_shared_memory
      ~skip_analysis_targets
      ~called_when_parameter
      ~callables_to_definitions_map
      ~callables_to_decorators_map
      ~type_of_expression_shared_memory
  in
  List.iter expected ~f:(fun { Expected.callable; call_graph; returned_callables } ->
      let actual_call_graph =
        callable
        |> CallGraphFixpoint.get_model fixpoint_state
        |> Option.value ~default:HigherOrderCallGraph.empty
        |> HigherOrderCallGraphForTest.from_actual
      in
      let expected_call_graph =
        HigherOrderCallGraphForTest.from_expected
          { HigherOrderCallGraphForTest.Expected.call_graph; returned_callables }
      in
      assert_equal
        ~cmp:HigherOrderCallGraphForTest.equal
        ~printer:(fun call_graph ->
          Format.asprintf
            "For callable %a: %a"
            Target.pp
            callable
            HigherOrderCallGraphForTest.pp
            call_graph)
        ~pp_diff:(Test.diff ~print:HigherOrderCallGraphForTest.pp)
        expected_call_graph
        actual_call_graph);
  OverrideGraph.SharedMemory.cleanup override_graph_shared_memory;
  SharedMemory.cleanup define_call_graphs;
  CallGraphFixpoint.cleanup ~keep_models:false fixpoint_state.CallGraphFixpoint.fixpoint;
  CallablesSharedMemory.ReadWrite.cleanup callables_to_definitions_map;
  ()


let test_higher_order_call_graph_fixpoint =
  test_list
    [
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def foo():
       return 0
     def bar():
       return foo
     def baz():
       return bar()
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.bar"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "5:9-5:12|artificial-attribute-access|qualification:test.foo",
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
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       (Target.Regular.Function { name = "test.foo"; kind = Normal });
                   ];
               };
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.baz"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "7:9-7:14",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  (Target.Regular.Function { name = "test.bar"; kind = Normal });
                              ]
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       (Target.Regular.Function { name = "test.foo"; kind = Normal });
                   ];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def foo():
       return 0
     def bar(arg):
       return foo
     def baz():
       return bar(foo)
  |}
           ~max_iterations:1
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.baz"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
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
                                         ( create_positional_parameter 0 "arg",
                                           Target.Regular.Function
                                             { name = "test.foo"; kind = Normal }
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                     ( "7:13-7:16|artificial-attribute-access|qualification:test.foo",
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
                   ];
                 returned_callables = [];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:{|
     def foo():
       return foo
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.foo"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "3:9-3:12|artificial-attribute-access|qualification:test.foo",
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
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       (Target.Regular.Function { name = "test.foo"; kind = Normal });
                   ];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:{|
     def foo():
       return foo()
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.foo"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "3:9-3:14",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  (Target.Regular.Function { name = "test.foo"; kind = Normal });
                              ]
                            ()) );
                   ];
                 returned_callables = [];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def bar():
       return 0
     def foo():
       if 1 == 1:
         return bar
       else:
         return foo()
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.foo"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "5:5-5:11|artificial-call|comparison",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  ~implicit_receiver:true
                                  ~receiver_class:"int"
                                  ~return_type:(Some ReturnType.bool)
                                  ~index:0
                                  (Target.Regular.Method
                                     { class_name = "int"; method_name = "__eq__"; kind = Normal });
                              ]
                            ()) );
                     ( "5:5-5:11|artificial-call|normalize-not-comparison>comparison",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  ~implicit_receiver:true
                                  ~receiver_class:"int"
                                  ~return_type:(Some ReturnType.bool)
                                  ~index:0
                                  (Target.Regular.Method
                                     { class_name = "int"; method_name = "__ne__"; kind = Normal });
                              ]
                            ()) );
                     ( "6:11-6:14|artificial-attribute-access|qualification:test.bar",
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
                     ( "8:11-8:16",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  (Target.Regular.Function { name = "test.foo"; kind = Normal });
                              ]
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       (Target.Regular.Function { name = "test.bar"; kind = Normal });
                   ];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def propagate(x):
       return x
     def bar():
       return 0
     def foo():
       return propagate(bar)
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.foo"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "7:9-7:23",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:
                                       (Target.Regular.Function
                                          { name = "test.propagate"; kind = Normal })
                                     ~parameters:
                                       [
                                         ( create_positional_parameter 0 "x",
                                           Target.Regular.Function
                                             { name = "test.bar"; kind = Normal }
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                     ( "7:19-7:22|artificial-attribute-access|qualification:test.bar",
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
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       (Target.Regular.Function { name = "test.bar"; kind = Normal });
                   ];
               };
               {
                 Expected.callable =
                   create_parameterized_target
                     ~regular:(Target.Regular.Function { name = "test.propagate"; kind = Normal })
                     ~parameters:
                       [
                         ( create_positional_parameter 0 "x",
                           Target.Regular.Function { name = "test.bar"; kind = Normal }
                           |> Target.from_regular );
                       ];
                 call_graph = [];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       (Target.Regular.Function { name = "test.bar"; kind = Normal });
                   ];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def propagate(x):
       return x
     def wrap_propagate(x):
       return propagate(x)
     def bar():
       return 0
     def foo():
       return wrap_propagate(bar)
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.foo"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "9:9-9:28",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:
                                       (Target.Regular.Function
                                          { name = "test.wrap_propagate"; kind = Normal })
                                     ~parameters:
                                       [
                                         ( create_positional_parameter 0 "x",
                                           Target.Regular.Function
                                             { name = "test.bar"; kind = Normal }
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                     ( "9:24-9:27|artificial-attribute-access|qualification:test.bar",
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
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       (Target.Regular.Function { name = "test.bar"; kind = Normal });
                   ];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def decorator(f):
       g = f
       def inner():
         f()
       return inner
     def bar():
       return 0
     def foo():
       return decorator(bar)
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.foo"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "10:9-10:23",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:
                                       (Target.Regular.Function
                                          { name = "test.decorator"; kind = Normal })
                                     ~parameters:
                                       [
                                         ( create_positional_parameter 0 "f",
                                           Target.Regular.Function
                                             { name = "test.bar"; kind = Normal }
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                     ( "10:19-10:22|artificial-attribute-access|qualification:test.bar",
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
                   ];
                 returned_callables =
                   [
                     CallTarget.create
                       (create_parameterized_target
                          ~regular:
                            (Target.Regular.Function
                               { name = "test.decorator.inner"; kind = Normal })
                          ~parameters:
                            [
                              ( AccessPath.Root.Variable "$parameter$f",
                                Target.Regular.Function { name = "test.bar"; kind = Normal }
                                |> Target.from_regular );
                            ]);
                   ];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def decorator(f):
       g = f
       def inner(x, y):
         return f(y, x)
       return inner
     @decorator
     def foo(x, y):
       return y
     def baz():
       pass
     def bar():
        return foo(baz, 0)
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.bar"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "13:10-13:21",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:
                                       (Target.Regular.Function
                                          { name = "test.decorator.inner"; kind = Normal })
                                     ~parameters:
                                       [
                                         ( create_positional_parameter 0 "x",
                                           Target.Regular.Function
                                             { name = "test.baz"; kind = Normal }
                                           |> Target.from_regular );
                                         ( AccessPath.Root.Variable "$parameter$f",
                                           Target.Regular.Function
                                             { name = "test.foo"; kind = Normal }
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                     ( "13:14-13:17|artificial-attribute-access|qualification:test.baz",
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
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       (Target.Regular.Function { name = "test.baz"; kind = Normal });
                   ];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     from typing import Callable
     def outer(f: Callable):
       def inner():
         g = f
         def inner_most():
           return 0
         return inner_most
       return inner()
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.outer"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "4:2-8:21",
                       ExpressionCallees.from_define
                         (DefineCallees.create
                            ~define_targets:
                              [
                                CallTarget.create_regular
                                  (Target.Regular.Function
                                     { name = "test.outer.inner"; kind = Normal });
                              ]
                            ()) );
                     ( "9:9-9:16",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  (Target.Regular.Function
                                     { name = "test.outer.inner"; kind = Normal });
                              ]
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       (Target.Regular.Function
                          { name = "test.outer.inner.inner_most"; kind = Normal });
                   ];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def foo(x: int):
       pass
     def bar(x: int):
       pass
     def decorator(f):
       def wrapper():
         return foo
       return wrapper
     @decorator
     def decorated(x: int):
       return bar
     def baz():
       return decorated()(1)
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.baz"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "14:9-14:20",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  (Target.Regular.Function
                                     { name = "test.decorator.wrapper"; kind = Normal });
                              ]
                            ()) );
                     ( "14:9-14:23",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  (Target.Regular.Function { name = "test.foo"; kind = Normal });
                              ]
                            ~unresolved:CallGraph.Unresolved.False
                            ()) );
                   ];
                 returned_callables = [];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def decorator(f):
       def inner():
         return f
       return inner
     @decorator
     def foo1():
       return 0
     class C:
       @decorator
       @classmethod
       def foo2(cls):
         return 0
     @decorator
     def foo3():
       return 0
     def bar(x: bool, y: bool):
       if x:
         return foo1  # Redirect `Decorated` target from attribute access
       elif y:
         return C.foo2  # Redirect `Decorated` target from attribute access
       else:
         f = foo3
         return f  # Return `Decorated` target from identifiers
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.bar"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "19:11-19:15|artificial-attribute-access|qualification:test.foo1",
                       ExpressionCallees.from_attribute_access (AttributeAccessCallees.create ()) );
                     (* TODO: Resolve `C.foo`. *)
                     ( "23:8-23:12|artificial-attribute-access|qualification:test.foo3",
                       ExpressionCallees.from_attribute_access (AttributeAccessCallees.create ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create
                       (create_parameterized_target
                          ~regular:
                            (Target.Regular.Function
                               { name = "test.decorator.inner"; kind = Normal })
                          ~parameters:
                            [
                              ( AccessPath.Root.Variable "$parameter$f",
                                Target.Regular.Function { name = "test.foo1"; kind = Normal }
                                |> Target.from_regular );
                            ]);
                     CallTarget.create
                       (create_parameterized_target
                          ~regular:
                            (Target.Regular.Function
                               { name = "test.decorator.inner"; kind = Normal })
                          ~parameters:
                            [
                              ( AccessPath.Root.Variable "$parameter$f",
                                Target.Regular.Function { name = "test.foo3"; kind = Normal }
                                |> Target.from_regular );
                            ]);
                   ];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def foo():
       return
     def bar():
       return
     class A:
       def m():
         return foo
     class B(A):
       def m():
         return bar
     def baz(a: A):
       return a.m()  # Test `Override` target
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.baz"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "13:9-13:14",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  ~implicit_receiver:true
                                  ~receiver_class:"test.A"
                                  (Target.Regular.Override
                                     { class_name = "test.A"; method_name = "m"; kind = Normal });
                              ]
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       (Target.Regular.Function { name = "test.foo"; kind = Normal });
                     CallTarget.create_regular
                       (Target.Regular.Function { name = "test.bar"; kind = Normal });
                   ];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def decorator(f):
       def foo():
         bar()
         f()
         return
       def bar():
         return
       return foo  # Test the closure of `foo`
     @decorator
     def baz():
       return
     def main():
       baz()
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.main"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "14:2-14:7",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:
                                       (Target.Regular.Function
                                          { name = "test.decorator.foo"; kind = Normal })
                                     ~parameters:
                                       [
                                         ( AccessPath.Root.Variable "$parameter$f",
                                           Target.Regular.Function
                                             { name = "test.baz"; kind = Normal }
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                   ];
                 returned_callables = [];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def baz():
       return
     def foo(f):
       return baz
     def bar():
       def g():
         return
       return foo(g)  # Test skip analysis
  |}
           ~skip_analysis_targets:
             (Target.HashSet.of_list
                [
                  Target.Regular.Function { name = "test.foo"; kind = Normal } |> Target.from_regular;
                ])
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.bar"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "7:2-8:10",
                       ExpressionCallees.from_define
                         (DefineCallees.create
                            ~define_targets:
                              [
                                CallTarget.create_regular
                                  (Target.Regular.Function { name = "test.bar.g"; kind = Normal });
                              ]
                            ()) );
                     ( "9:9-9:15",
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
                                           (Target.Regular.Function
                                              { name = "test.bar.g"; kind = Normal });
                                       ];
                                     unresolved = CallGraph.Unresolved.False;
                                   };
                                 ])
                            ()) );
                   ];
                 returned_callables = [];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     class A:
       @classmethod
       def f(x, g):
         return g
     def foo():
       return
     @functools.partial(A.f, "abc")
     def bar():
       return foo
     def baz():
       return bar()  # Test resolving calls that require redirecting expressions
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.baz"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "12:9-12:14",
                       (* TODO(T226554045): Handle shimming in decorators *)
                       ExpressionCallees.from_call (CallCallees.create ()) );
                   ];
                 returned_callables = [];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     from typing import ParamSpec, Protocol, TypeVar
     T = TypeVar("T")
     P = ParamSpec("P")
     class DecoratorProtocol(Protocol):
       def __call__(self, func: Callable[P, T]) -> Callable[P, T]: ...
     def log(flag: bool) -> DecoratorProtocol:
       def inner(func: Callable[P, T]) -> Callable[P, T]:
         def wrapper(*args: P.args, **kwargs: P.kwargs) -> T:
           return func(*args, **kwargs)
         return wrapper
       return inner
     @log(flag=True)
     def foo():
       return  # Test building higher order call graphs for decorated targets
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.log"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "8:2-11:18",
                       ExpressionCallees.from_define
                         (DefineCallees.create
                            ~define_targets:
                              [
                                CallTarget.create_regular
                                  (Target.Regular.Function
                                     { name = "test.log.inner"; kind = Normal });
                              ]
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       (Target.Regular.Function { name = "test.log.inner"; kind = Normal });
                   ];
               };
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.foo"; kind = Decorated }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "13:1-13:15",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  (Target.Regular.Function { name = "test.log"; kind = Normal });
                              ]
                            ()) );
                     ( "13:1-13:15|artificial-call|for-decorated-target",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  ~implicit_receiver:true
                                  ~implicit_dunder_call:true
                                  ~receiver_class:"test.DecoratorProtocol"
                                  (Target.Regular.Method
                                     {
                                       class_name = "test.DecoratorProtocol";
                                       method_name = "__call__";
                                       kind = Normal;
                                     });
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:
                                       (Target.Regular.Function
                                          { name = "test.log.inner"; kind = Normal })
                                     ~parameters:
                                       [
                                         ( create_positional_parameter 0 "func",
                                           Target.Regular.Function
                                             { name = "test.foo"; kind = Normal }
                                           |> Target.from_regular );
                                       ]);
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
                     ( "14:0-15:8|artificial-attribute-access|for-decorated-target-callee:test.foo",
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
                   ];
                 returned_callables =
                   [
                     CallTarget.create
                       (create_parameterized_target
                          ~regular:
                            (Target.Regular.Function
                               { name = "test.log.inner.wrapper"; kind = Normal })
                          ~parameters:
                            [
                              ( AccessPath.Root.Variable "$parameter$func",
                                Target.Regular.Function { name = "test.foo"; kind = Normal }
                                |> Target.from_regular );
                            ]);
                     CallTarget.create_regular
                       (Target.Regular.Function { name = "test.foo"; kind = Normal });
                   ];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     from typing import TypeVar, Generic, Callable
     T = TypeVar("T")
     R = TypeVar("R")
     class classproperty(Generic[T, R]):
       def __init__(self, fget: Callable[[type[T]], R]) -> None:
         self.fget = fget
       # pyre-fixme[14]:
       def __get__(self, obj: object, obj_cls_type: type[T]) -> R:
         # pyre-fixme[16]:
         return self.fget.__get__(None, obj_cls_type)()
     def foo():
       return
     class MyClass:
       @classproperty
       def bar(cls):
         return foo
     def main(o: MyClass):
       return o.bar()  # Test storing decorated functions into object attributes
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.main"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "19:9-19:16",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  ~implicit_receiver:true
                                  (Target.Regular.Method
                                     {
                                       class_name = "test.MyClass";
                                       method_name = "bar";
                                       kind = Normal;
                                     });
                              ]
                            ()) );
                   ];
                 returned_callables =
                   [
                     (* TODO: Handle descriptors properly. Since `o.bar` evaluates into `foo`
                        (because of calling `classproperty.__get__`, which results in calling
                        `MyClass.bar`), `o.bar()` is `foo()`. Hence nothing should be returned
                        here. *)
                     CallTarget.create_regular
                       (Target.Regular.Function { name = "test.foo"; kind = Normal });
                   ];
               };
               {
                 Expected.callable =
                   create_parameterized_target
                     ~regular:
                       (Target.Regular.Method
                          {
                            class_name = "test.classproperty";
                            method_name = "__init__";
                            kind = Normal;
                          })
                     ~parameters:
                       [
                         ( create_positional_parameter 1 "fget",
                           Target.Regular.Method
                             { class_name = "test.MyClass"; method_name = "bar"; kind = Normal }
                           |> Target.from_regular );
                       ];
                 call_graph = [];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       ~implicit_receiver:true
                       (Target.Regular.Method
                          { class_name = "test.MyClass"; method_name = "bar"; kind = Normal });
                   ];
               };
               {
                 Expected.callable =
                   Target.Regular.Method
                     { class_name = "test.MyClass"; method_name = "bar"; kind = Decorated }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "15:3-15:16|artificial-call|for-decorated-target",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~init_targets:
                              [
                                CallTarget.create
                                  ~implicit_receiver:true
                                  (create_parameterized_target
                                     ~regular:
                                       (Target.Regular.Method
                                          {
                                            class_name = "test.classproperty";
                                            method_name = "__init__";
                                            kind = Normal;
                                          })
                                     ~parameters:
                                       [
                                         ( create_positional_parameter 1 "fget",
                                           Target.Regular.Method
                                             {
                                               class_name = "test.MyClass";
                                               method_name = "bar";
                                               kind = Normal;
                                             }
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ~new_targets:
                              [
                                CallTarget.create_regular
                                  ~is_static_method:true
                                  (Target.Regular.Method
                                     {
                                       class_name = "object";
                                       method_name = "__new__";
                                       kind = Normal;
                                     });
                              ]
                            ()) );
                     ( "16:2-17:14|artificial-attribute-access|for-decorated-target-callee:test.MyClass.bar",
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
                                            class_name = "test.MyClass";
                                            method_name = "bar";
                                            kind = Normal;
                                          });
                                   ]
                                 ())
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       ~implicit_receiver:true
                       (Target.Regular.Method
                          { class_name = "test.MyClass"; method_name = "bar"; kind = Normal });
                   ];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def decorator(f):
       def inner(*args, **kwargs):
         f(*args, **kwargs)
       return inner
     def foo():
       return
     @unknown_decorator
     @decorator
     def bar():
       return foo  # Cannot resolve callees on the decorator
     def main():
       return bar()
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.main"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "13:9-13:14",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:
                                       (Target.Regular.Function
                                          { name = "test.decorator.inner"; kind = Normal })
                                     ~parameters:
                                       [
                                         ( AccessPath.Root.Variable "$parameter$f",
                                           Target.Regular.Function
                                             { name = "test.bar"; kind = Normal }
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                   ];
                 returned_callables = [];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     from typing import Callable, TypeVar
     _TClass = TypeVar("_TClass")
     _TReturnType = TypeVar("_TReturnType")
     class BaseCachedProperty(Generic[_TClass, _TReturnType]):
       def __init__(
           self, f: Callable[[_TClass], _TReturnType], doc: str = ...
       ) -> None: ...
       def __get__(
           self, obj: None, cls: Type[_TClass]
       ) -> BaseCachedProperty[_TClass, _TReturnType]: ...
     def foo():
       return
     class A:
       @BaseCachedProperty  # Test decorators with stub
       async def bar(self):
         return foo
     def main(a: A):
       return await a.bar()
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.main"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "19:15-19:22",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  ~implicit_receiver:true
                                  (Target.Regular.Method
                                     { class_name = "test.A"; method_name = "bar"; kind = Normal });
                              ]
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       (Target.Regular.Function { name = "test.foo"; kind = Normal });
                   ];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     from typing import Callable, TypeVar
     _TClass = TypeVar("_TClass")
     _TReturnType = TypeVar("_TReturnType")
     _TAwaitableReturnType = TypeVar("_TAwaitableReturnType", bound=Awaitable[object])
     class BaseCachedProperty(Generic[_TClass, _TReturnType]):
       def __init__(
           self, f: Callable[[_TClass], _TReturnType], doc: str = ...
       ) -> None:
         self.f = f
       def __get__(
           self, obj: None, cls: Type[_TClass]
       ) -> BaseCachedProperty[_TClass, _TReturnType]: ...
     class async_cached_property(BaseCachedProperty[_TClass, _TAwaitableReturnType]): ...
     def foo():
       return
     class A:
       @async_cached_property  # Test overrides
       async def bar(self):
         return foo
     def main(a: A):
       return await a.bar()
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.main"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "22:15-22:22",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  ~implicit_receiver:true
                                  (Target.Regular.Method
                                     { class_name = "test.A"; method_name = "bar"; kind = Normal });
                              ]
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       (Target.Regular.Function { name = "test.foo"; kind = Normal });
                   ];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def foo():
       return
     class A:
       @__classproperty__  # Test property targets in `__classproperty__(test.A.name)`
       def name(cls):
         return foo
     def main(a: A):
       return A.name  # Test accessing a property
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Method
                     { class_name = "test.A"; method_name = "name"; kind = Decorated }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "5:3-5:20|artificial-call|for-decorated-target",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~unresolved:(Unresolved.True (BypassingDecorators CannotResolveExports))
                            ()) );
                     ( "6:2-7:14|artificial-attribute-access|for-decorated-target-callee:test.A.name",
                       ExpressionCallees.from_attribute_access
                         (AttributeAccessCallees.create
                            ~property_targets:
                              [
                                CallTarget.create_regular
                                  ~implicit_receiver:true
                                  (Target.Regular.Method
                                     { class_name = "test.A"; method_name = "name"; kind = Normal });
                              ]
                            ~is_attribute:false
                            ()) );
                   ];
                 returned_callables = [];
               };
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.main"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "9:9-9:15",
                       ExpressionCallees.from_attribute_access
                         (AttributeAccessCallees.create
                            ~property_targets:
                              [
                                CallTarget.create_regular
                                  ~implicit_receiver:true
                                  (Target.Regular.Method
                                     { class_name = "test.A"; method_name = "name"; kind = Normal });
                              ]
                            ~is_attribute:false
                            ()) );
                   ];
                 returned_callables = [ (* TODO(T222400916): This should be `test.foo`. *) ];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def foo():
       return
     class A:
       @__property__  # Test property targets in `__property__(test.A.name)`
       def name(self):
         return foo
     def main(a: A):
       return a.name  # Test accessing a property
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Method
                     { class_name = "test.A"; method_name = "name"; kind = Decorated }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "5:3-5:15|artificial-call|for-decorated-target",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~unresolved:(Unresolved.True (BypassingDecorators CannotResolveExports))
                            ()) );
                     ( "6:2-7:14|artificial-attribute-access|for-decorated-target-callee:test.A.name",
                       ExpressionCallees.from_attribute_access
                         (AttributeAccessCallees.create
                            ~property_targets:
                              [
                                CallTarget.create_regular
                                  ~implicit_receiver:true
                                  (Target.Regular.Method
                                     { class_name = "test.A"; method_name = "name"; kind = Normal });
                              ]
                            ~is_attribute:false
                            ()) );
                   ];
                 returned_callables = [ (* TODO(T222400916): This should be `test.foo`. *) ];
               };
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.main"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "9:9-9:15",
                       ExpressionCallees.from_attribute_access
                         (AttributeAccessCallees.create
                            ~property_targets:
                              [
                                CallTarget.create_regular
                                  ~implicit_receiver:true
                                  (Target.Regular.Method
                                     { class_name = "test.A"; method_name = "name"; kind = Normal });
                              ]
                            ~is_attribute:false
                            ()) );
                   ];
                 returned_callables = [ (* TODO(T222400916): This should be `test.foo`. *) ];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     class Base:
       def __enter__(self):
         return self
       def __exit__(self, exc_type: object, exc_value: object, traceback: object) -> bool:
         return False
       def __call__(self, func):
         def inner(*args, **kwds):
           func(*args, **kwds)
         return inner
     base = Base()
     @base  # Test custom context manager
     def contextmanager(x):
       print(x)
     class Subclass(Base):
       def __init__(self, name):
         self.name = name
     subclass = Subclass("123")
     @subclass  # Test subclass and decorator factory
     def contextmanager_subclass(x):
       print(x)
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.contextmanager"; kind = Decorated }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "12:1-12:5|artificial-call|for-decorated-target",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create
                                  ~implicit_receiver:true
                                  ~implicit_dunder_call:true
                                  ~receiver_class:"test.Base"
                                  (create_parameterized_target
                                     ~regular:
                                       (Target.Regular.Method
                                          {
                                            class_name = "test.Base";
                                            method_name = "__call__";
                                            kind = Normal;
                                          })
                                     ~parameters:
                                       [
                                         ( create_positional_parameter 1 "func",
                                           Target.Regular.Function
                                             { name = "test.contextmanager"; kind = Normal }
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                     ( "13:0-14:10|artificial-attribute-access|for-decorated-target-callee:test.contextmanager",
                       ExpressionCallees.from_attribute_access
                         (AttributeAccessCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       (Target.Regular.Function
                                          { name = "test.contextmanager"; kind = Normal });
                                   ]
                                 ())
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create
                       (create_parameterized_target
                          ~regular:
                            (Target.Regular.Function
                               { name = "test.Base.__call__.inner"; kind = Normal })
                          ~parameters:
                            [
                              ( AccessPath.Root.Variable "$parameter$func",
                                Target.Regular.Function
                                  { name = "test.contextmanager"; kind = Normal }
                                |> Target.from_regular );
                            ]);
                   ];
               };
               {
                 Expected.callable =
                   Target.Regular.Function
                     { name = "test.contextmanager_subclass"; kind = Decorated }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "19:1-19:9|artificial-call|for-decorated-target",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create
                                  ~implicit_receiver:true
                                  ~implicit_dunder_call:true
                                  ~receiver_class:"test.Subclass"
                                  (create_parameterized_target
                                     ~regular:
                                       (Target.Regular.Method
                                          {
                                            class_name = "test.Base";
                                            method_name = "__call__";
                                            kind = Normal;
                                          })
                                     ~parameters:
                                       [
                                         ( create_positional_parameter 1 "func",
                                           Target.Regular.Function
                                             {
                                               name = "test.contextmanager_subclass";
                                               kind = Normal;
                                             }
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                     ( "20:0-21:10|artificial-attribute-access|for-decorated-target-callee:test.contextmanager_subclass",
                       ExpressionCallees.from_attribute_access
                         (AttributeAccessCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       (Target.Regular.Function
                                          { name = "test.contextmanager_subclass"; kind = Normal });
                                   ]
                                 ())
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create
                       (create_parameterized_target
                          ~regular:
                            (Target.Regular.Function
                               { name = "test.Base.__call__.inner"; kind = Normal })
                          ~parameters:
                            [
                              ( AccessPath.Root.Variable "$parameter$func",
                                Target.Regular.Function
                                  { name = "test.contextmanager_subclass"; kind = Normal }
                                |> Target.from_regular );
                            ]);
                   ];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     class Base:
       def __call__(self, func):
         ...  # Test when `__call__` is a stub
     class Subclass(Base):
       def __init__(self, name):
         self.name = name
     subclass = Subclass("123")
     @subclass
     def stub_contextmanager(x):
       print(x)
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.stub_contextmanager"; kind = Decorated }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "9:1-9:9|artificial-call|for-decorated-target",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  ~implicit_receiver:true
                                  ~implicit_dunder_call:true
                                  ~receiver_class:"test.Subclass"
                                  (Target.Regular.Method
                                     {
                                       class_name = "test.Base";
                                       method_name = "__call__";
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
                                              { name = "test.stub_contextmanager"; kind = Normal });
                                       ];
                                     unresolved = CallGraph.Unresolved.False;
                                   };
                                 ])
                            ()) );
                     ( "10:0-11:10|artificial-attribute-access|for-decorated-target-callee:test.stub_contextmanager",
                       ExpressionCallees.from_attribute_access
                         (AttributeAccessCallees.create
                            ~if_called:
                              (CallCallees.create
                                 ~call_targets:
                                   [
                                     CallTarget.create_regular
                                       (Target.Regular.Function
                                          { name = "test.stub_contextmanager"; kind = Normal });
                                   ]
                                 ())
                            ()) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       (Target.Regular.Function { name = "test.stub_contextmanager"; kind = Normal });
                   ];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def decorator():
       ...
     def foo():
       return
     @decorator  # Test stub call targets in decorated targets
     def bar():
       return foo
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.bar"; kind = Decorated }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "6:1-6:10|artificial-call|for-decorated-target",
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
                                              { name = "test.bar"; kind = Normal });
                                       ];
                                     unresolved = CallGraph.Unresolved.False;
                                   };
                                 ])
                            ()) );
                     ( "7:0-8:12|artificial-attribute-access|for-decorated-target-callee:test.bar",
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
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       (Target.Regular.Function { name = "test.bar"; kind = Normal });
                   ];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     class A():
       @property
       def foo(self) -> int:
         return 0
     def bar(f):
       return
     def main(a: A):
       x = a.foo  # Test not propagating property targets
       return bar(x)
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.main"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "10:9-10:15",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  (Target.Regular.Function { name = "test.bar"; kind = Normal });
                              ]
                            ()) );
                     ( "9:6-9:11",
                       ExpressionCallees.from_attribute_access
                         (AttributeAccessCallees.create
                            ~property_targets:
                              [
                                CallTarget.create_regular
                                  ~implicit_receiver:true
                                  ~return_type:(Some ReturnType.integer)
                                  (Target.Regular.Method
                                     { class_name = "test.A"; method_name = "foo"; kind = Normal });
                              ]
                            ~is_attribute:false
                            ()) );
                   ];
                 returned_callables = [];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def foo():
       return
     def bar(f):
       return
     def main():
       return bar(foo)  # Test treating parameters as being called
  |}
           ~called_when_parameter:
             ([Target.Regular.Function { name = "test.foo"; kind = Normal } |> Target.from_regular]
             |> Target.HashSet.of_list)
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.main"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "7:9-7:17",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  (Target.Regular.Function { name = "test.bar"; kind = Normal });
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
                     ( "7:13-7:16|artificial-attribute-access|qualification:test.foo",
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
                   ];
                 returned_callables = [];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def decorator(f):
       def inner(x):
         return f(x)
       return inner
     def main():
       @decorator
       def inner(x):
         return
       inner(0)  # Test creating decorated targets for inner functions
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.main"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "10:2-10:10",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:
                                       (Target.Regular.Function
                                          { name = "test.decorator.inner"; kind = Normal })
                                     ~parameters:
                                       [
                                         ( AccessPath.Root.Variable "$parameter$f",
                                           Target.Regular.Function
                                             { name = "test.main.inner"; kind = Normal }
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                     ( "7:2-9:10",
                       ExpressionCallees.from_define
                         (DefineCallees.create
                            ~define_targets:
                              [
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:
                                       (Target.Regular.Function
                                          { name = "test.decorator.inner"; kind = Normal })
                                     ~parameters:
                                       [
                                         ( AccessPath.Root.Variable "$parameter$f",
                                           Target.Regular.Function
                                             { name = "test.main.inner"; kind = Normal }
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                   ];
                 returned_callables = [];
               };
             ]
           ();
      labeled_test_case __FUNCTION__ __LINE__
      @@ assert_higher_order_call_graph_fixpoint
           ~source:
             {|
     def decorator_factory(x: int):
       def decorator(f):
         def inner(x):
           return f(x)
         return inner
       return decorator
     def main():
       @decorator_factory(1)
       def inner(x):
         return
       inner(0)  # Test creating decorated targets for inner functions
  |}
           ~expected:
             [
               {
                 Expected.callable =
                   Target.Regular.Function { name = "test.main"; kind = Normal }
                   |> Target.from_regular;
                 call_graph =
                   [
                     ( "12:2-12:10",
                       ExpressionCallees.from_call
                         (CallCallees.create
                            ~call_targets:
                              [
                                CallTarget.create_regular
                                  (Target.Regular.Function
                                     { name = "test.main.inner"; kind = Normal });
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:
                                       (Target.Regular.Function
                                          {
                                            name = "test.decorator_factory.decorator.inner";
                                            kind = Normal;
                                          })
                                     ~parameters:
                                       [
                                         ( AccessPath.Root.Variable "$parameter$f",
                                           Target.Regular.Function
                                             { name = "test.main.inner"; kind = Normal }
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                     ( "9:2-11:10",
                       ExpressionCallees.from_define
                         (DefineCallees.create
                            ~define_targets:
                              [
                                CallTarget.create_regular
                                  (Target.Regular.Function
                                     { name = "test.main.inner"; kind = Normal });
                                CallTarget.create
                                  (create_parameterized_target
                                     ~regular:
                                       (Target.Regular.Function
                                          {
                                            name = "test.decorator_factory.decorator.inner";
                                            kind = Normal;
                                          })
                                     ~parameters:
                                       [
                                         ( AccessPath.Root.Variable "$parameter$f",
                                           Target.Regular.Function
                                             { name = "test.main.inner"; kind = Normal }
                                           |> Target.from_regular );
                                       ]);
                              ]
                            ()) );
                   ];
                 returned_callables = [];
               };
             ]
           ();
    ]


let () = "callGraphFixpoint" >::: [test_higher_order_call_graph_fixpoint] |> Test.run
