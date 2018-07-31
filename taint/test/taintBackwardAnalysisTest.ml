(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Ast
open Pyre
open Statement
open Taint

open Test
open Interprocedural


type taint_in_taint_out_expectation = {
  define_name: string;
  taint_in_taint_out_parameters: int list;
}


let assert_taint_in_taint_out source { define_name; taint_in_taint_out_parameters } =
  let { Node.value = define; _ } =
    parse source
    |> Preprocessing.preprocess
    |> Preprocessing.defines
    |> List.hd_exn
  in
  let backward_model = BackwardAnalysis.run define in
  let taint_model = { Taint.Result.empty_model with backward = backward_model; } in
  let call_target = `RealTarget (Access.create define_name) in
  let () =
    Result.empty_model
    |> Result.with_model Taint.Result.kind taint_model
    |> Fixpoint.add_predefined call_target
  in
  match Fixpoint.get_model call_target >>= Result.get_model Taint.Result.kind with
  | None -> assert_failure ("no model for " ^ define_name)
  | Some { backward = { taint_in_taint_out; _ }; _ } ->
      let extract_parameter_position root _ positions =
        match root with
        | AccessPath.Root.Parameter { position; _ } -> Int.Set.add positions position
        | _ -> positions
      in
      let taint_in_taint_out_positions =
        Domains.BackwardState.fold
          taint_in_taint_out
          ~f:extract_parameter_position
          ~init:Int.Set.empty
      in
      let expected_positions = Int.Set.of_list taint_in_taint_out_parameters in
      assert_equal
        ~cmp:Int.Set.equal
        ~printer:(fun set -> Sexp.to_string [%message (set: Int.Set.t)])
        expected_positions
        taint_in_taint_out_positions


type taint_sink_expectation = {
  define_name: string;
  taint_sink_parameters: (int * Taint.Sinks.t list) list;
}


let assert_taint_sinks source { define_name; taint_sink_parameters } =
  let { Node.value = define; _ } =
    parse source
    |> Preprocessing.preprocess
    |> Preprocessing.defines
    |> List.hd_exn
  in
  let backward_model = BackwardAnalysis.run define in
  let extract_sinks_by_parameter_position root sink_tree sink_map =
    match root with
    | AccessPath.Root.Parameter { position; _ } ->
        let sinks =
          Domains.BackwardState.collapse sink_tree
          |> Domains.BackwardTaint.elements
        in
        let sinks =
          Int.Map.find sink_map position
          |> Option.value ~default:[]
          |> List.rev_append sinks
          |> List.dedup_and_sort ~compare:Taint.Sinks.compare
        in
        Int.Map.set sink_map ~key:position ~data:sinks
    | _ ->
        sink_map
  in
  let taint_map =
    Domains.BackwardState.fold
      backward_model.sink_taint
      ~f:extract_sinks_by_parameter_position
      ~init:Int.Map.empty
  in
  let check_each_position ~key:position ~data =
    match data with
    | `Both (expected, actual) ->
        assert_equal
          ~cmp:(List.equal ~equal:Taint.Sinks.equal)
          ~printer:(fun list -> Sexp.to_string [%message (list: Taint.Sinks.t list)])
          expected
          actual
    | `Left _ -> ()
    | `Right _ -> ()
  in
  let expected_positions = Int.Map.of_alist_exn taint_sink_parameters in
  Int.Map.iter2 ~f:check_each_position expected_positions taint_map


let test_plus_taint_in_taint_out _ =
  assert_taint_in_taint_out
    {|
    def test_plus_taint_in_taint_out(tainted_parameter1, parameter2):
      tainted_value = tainted_parameter1 + 5
      return tainted_value
    |}
    {
      define_name = "test_plus_taint_in_taint_out";
      taint_in_taint_out_parameters = [0];
    }


let test_concatenate_taint_in_taint_out _ =
  assert_taint_in_taint_out
    {|
    def test_concatenate_taint_in_taint_out(parameter0, tainted_parameter1):
      unused_parameter = parameter0
      command_unsafe = 'echo' + tainted_parameter1 + ' >> /dev/null'
      return command_unsafe
    |}
    {
      define_name = "test_concatenate_taint_in_taint_out";
      taint_in_taint_out_parameters = [1];
    }


let test_sink _ =
  assert_taint_sinks
    {|
    def test_sink(parameter0, tainted_parameter1):
      unused_parameter = parameter0
      command_unsafe = 'echo' + tainted_parameter1 + ' >> /dev/null'
      __testSink(command_unsafe)
    |}
    {
      define_name = "test_sink";
      taint_sink_parameters = [1, [Taint.Sinks.TestSink]];
    }


let test_rce_sink _ =
  assert_taint_sinks
    {|
    def test_rce_sink(parameter0, tainted_parameter1):
      unused_parameter = parameter0
      command_unsafe = 'echo' + tainted_parameter1 + ' >> /dev/null'
      __testRCESink(command_unsafe)
    |}
    {
      define_name = "test_rce_sink";
      taint_sink_parameters = [1, [Taint.Sinks.RemoteCodeExecution]];
    }


let test_rce_and_test_sink _ =
  assert_taint_sinks
    {|
    def test_rce_and_test_sink(test_only, rce_only, both):
      __testSink(test_only)
      __testRCESink(rce_only)
      if True:
        __testSink(both)
      else:
        __testRCESink(both)
    |}
    {
      define_name = "test_rce_and_test_sink";
      taint_sink_parameters = [
        0, [Taint.Sinks.TestSink];
        1, [Taint.Sinks.RemoteCodeExecution];
        2, [Taint.Sinks.RemoteCodeExecution; Taint.Sinks.TestSink];
      ];
    }


let () =
  "taint">:::[
    "plus_taint_in_taint_out">::test_plus_taint_in_taint_out;
    "concatenate_taint_in_taint_out">::test_concatenate_taint_in_taint_out;
    "rce_sink">::test_rce_sink;
    "test_sink">::test_sink;
    "rce_and_test_sink">::test_rce_and_test_sink;
  ]
  |> run_test_tt_main
