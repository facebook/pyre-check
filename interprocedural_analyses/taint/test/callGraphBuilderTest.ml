(* Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Test

let assert_call_graph ~context source expected =
  let _ =
    ScratchProject.setup ~context ["test.py", source]
    |> ScratchProject.build_type_environment_and_postprocess
         ~call_graph_builder:(module Taint.CallGraphBuilder)
  in
  let assert_call_graph_consistent (name, expected) =
    let actual =
      Analysis.Callgraph.get ~caller:name
      |> List.map ~f:(fun { Analysis.Callgraph.callee; _ } -> callee)
    in
    assert_equal
      ~cmp:(List.equal Analysis.Callgraph.equal_callee)
      ~printer:(fun callees ->
        List.map callees ~f:Analysis.Callgraph.show_callee |> String.concat ~sep:", ")
      expected
      actual
  in
  List.iter expected ~f:assert_call_graph_consistent


let test_partial context =
  let assert_call_graph = assert_call_graph ~context in
  assert_call_graph
    {|
      from functools import partial
      def foo(a, b):
        pass
      def bar(a):
        return partial(foo, a)
    |}
    [
      ( !&"test.bar",
        [
          Analysis.Callgraph.Function !&"test.foo";
          Analysis.Callgraph.Method
            {
              class_name = Type.Primitive "functools.partial";
              direct_target = !&"functools.partial.__init__";
              dispatch = Analysis.Callgraph.Static;
              is_optional_class_attribute = false;
            };
        ] );
    ];
  (* Our support for partially called methods is missing at the moment. *)
  assert_call_graph
    {|
      from functools import partial
      class C:
        def foo(self, x: int, y: str) -> None: ...

      def bar(c: C, x: int) -> None:
        partial(c.foo, x)
    |}
    [
      ( !&"test.bar",
        [
          Analysis.Callgraph.Method
            {
              class_name = Type.Primitive "functools.partial";
              direct_target = !&"functools.partial.__init__";
              dispatch = Analysis.Callgraph.Static;
              is_optional_class_attribute = false;
            };
        ] );
    ]


let test_multiprocessing_process context =
  let assert_call_graph = assert_call_graph ~context in
  assert_call_graph
    {|
      import multiprocessing
      def foo(x):
        return x

      def bar():
         multiprocessing.Process(target=foo, args=(1,))
    |}
    [
      ( !&"test.bar",
        [
          Analysis.Callgraph.Method
            {
              class_name = Type.Primitive "multiprocessing.context.Process";
              direct_target = !&"multiprocessing.context.Process.__init__";
              dispatch = Analysis.Callgraph.Static;
              is_optional_class_attribute = false;
            };
          Analysis.Callgraph.Function !&"test.foo";
        ] );
    ]


let () =
  "callGraphBuilder"
  >::: ["partial" >:: test_partial; "multiprocessing_process" >:: test_multiprocessing_process]
  |> Test.run
