(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Test
open TestHelper
open Interprocedural
open CallGraph
open CallGraphTestHelper

module Expected = struct
  type t = {
    callable: string;
    returned_callables: CallTarget.t list;
    call_graph: (string * LocationCallees.t) list;
  }
end

let assert_higher_order_call_graph_fixpoint ~source ~expected () context =
  let handle = "test.py" in
  let configuration, pyre_api =
    initialize_pyre_and_fail_on_errors ~context ~handle ~source_content:source ~models_source:None
  in
  let static_analysis_configuration = Configuration.StaticAnalysis.create configuration () in
  let qualifier = Ast.Reference.create (String.chop_suffix_exn handle ~suffix:".py") in
  let source = source_from_qualifier ~pyre_api qualifier in
  let initial_callables = FetchCallables.from_source ~configuration ~pyre_api ~source in
  let definitions = FetchCallables.get_definitions initial_callables in
  let override_graph_heap = OverrideGraph.Heap.from_source ~pyre_api ~source in
  let override_graph_shared_memory = OverrideGraph.SharedMemory.from_heap override_graph_heap in
  let ({ MutableDefineCallGraphSharedMemory.whole_program_call_graph; define_call_graphs } as
      call_graph)
    =
    MutableDefineCallGraphSharedMemory.build_whole_program_call_graph
      ~scheduler:(Test.mock_scheduler ())
      ~static_analysis_configuration
      ~pyre_api
      ~resolve_module_path:None
      ~override_graph:(Some (OverrideGraph.SharedMemory.read_only override_graph_shared_memory))
      ~store_shared_memory:true
      ~attribute_targets:Target.Set.empty
      ~skip_analysis_targets:Target.Set.empty
      ~definitions
  in
  let dependency_graph =
    DependencyGraph.build_whole_program_dependency_graph
      ~static_analysis_configuration
      ~prune:DependencyGraph.PruneMethod.None
      ~initial_callables
      ~call_graph:whole_program_call_graph
      ~overrides:override_graph_heap
  in
  let fixpoint_state =
    CallGraphFixpoint.compute
      ~scheduler:(Test.mock_scheduler ())
      ~scheduler_policy:(Scheduler.Policy.legacy_fixed_chunk_count ())
      ~pyre_api
      ~call_graph
      ~dependency_graph
      ~override_graph_shared_memory
      ~initial_callables
      ~max_iterations:10
  in
  List.iter expected ~f:(fun { Expected.callable; call_graph; returned_callables } ->
      let actual_call_graph =
        callable
        |> Ast.Reference.create
        |> Target.create_function
        |> CallGraphFixpoint.get_model fixpoint_state
        |> Option.value ~default:HigherOrderCallGraph.empty
        |> ImmutableHigherOrderCallGraph.from_higher_order_call_graph
      in
      let expected_call_graph =
        ImmutableHigherOrderCallGraph.from_input
          { ImmutableHigherOrderCallGraph.Input.call_graph; returned_callables }
      in
      assert_equal
        ~cmp:ImmutableHigherOrderCallGraph.equal
        ~printer:ImmutableHigherOrderCallGraph.show
        expected_call_graph
        actual_call_graph);
  OverrideGraph.SharedMemory.cleanup override_graph_shared_memory;
  MutableDefineCallGraphSharedMemory.cleanup define_call_graphs;
  CallGraphFixpoint.cleanup fixpoint_state;
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
                 Expected.callable = "test.bar";
                 call_graph =
                   [
                     ( "5:9-5:12",
                       LocationCallees.Singleton
                         (ExpressionCallees.from_attribute_access
                            (AttributeAccessCallees.create
                               ~callable_targets:
                                 [
                                   CallTarget.create_regular
                                     (Target.Regular.Function { name = "test.foo"; kind = Normal });
                                 ]
                               ())) );
                   ];
                 returned_callables =
                   [
                     CallTarget.create_regular
                       (Target.Regular.Function { name = "test.foo"; kind = Normal });
                   ];
               };
               {
                 Expected.callable = "test.baz";
                 call_graph =
                   [
                     ( "7:9-7:14",
                       LocationCallees.Singleton
                         (ExpressionCallees.from_call
                            (CallCallees.create
                               ~call_targets:
                                 [
                                   CallTarget.create_regular
                                     (Target.Regular.Function { name = "test.bar"; kind = Normal });
                                 ]
                               ())) );
                   ];
                 returned_callables = [];
                 (* TODO: Expect `foo` *)
               };
             ]
           ();
    ]


let () = "callGraphFixpoint" >::: [test_higher_order_call_graph_fixpoint] |> Test.run
