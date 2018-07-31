(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Ast
open Statement

open Test
open Interprocedural

module Parallel = Hack_parallel.Std


let parse_source ?(qualifier=[]) source =
  parse ~qualifier source
  |> Preprocessing.preprocess


let configuration = Configuration.create ()


let environment ?(configuration = configuration) sources =
  let _ = Test.parse "" in  (* Make sure Test module is loaded. *)
  let environment = Environment.Builder.create () in
  Service.Environment.populate (Environment.handler ~configuration environment) sources;
  Environment.handler ~configuration environment


let setup_environment sources =
  let () = Parallel.Daemon.check_entry_point () in
  environment sources ~configuration


let create_call_graph ?(test_file = "test_file") source =
  let handle = File.Handle.create test_file in
  let source = parse_source source in
  let () = AstSharedMemory.add_source handle source in
  let environment = setup_environment [source] in
  TypeCheck.check configuration environment source |> ignore;
  let call_graph =
    Service.Analysis.record_and_merge_call_graph environment CallGraph.empty handle source
  in
  let () = Service.Analysis.record_overrides environment source in
  let callables =
    Service.Analysis.record_path_of_definitions handle source
    |> List.map ~f:(fun { Node.value = { Define.name; _ }; _ } -> Callable.make_real name)
  in
  let () = TypeCheck.check configuration environment source |> ignore in
  call_graph, callables


let test_fixpoint _ =
  let source =
    {|
    class Foo:
      def __init__(self):
        pass

      def bar(self):
        return 10

      def qux(self):
        return self.bar()
    |}
  in
  let call_graph, all_callables = create_call_graph source in
  let caller_map = CallGraph.reverse call_graph in
  let analyses = [Taint.Analysis.abstract_kind] in
  let iterations =
    Analysis.compute_fixpoint
      ~workers:None
      ~analyses
      ~caller_map
      ~all_callables
      Fixpoint.Epoch.initial
  in
  assert_equal 2 iterations ~printer:Int.to_string


let () =
  "taint">:::[
    "fixpoint">::test_fixpoint;
  ]
  |> run_test_tt_main
