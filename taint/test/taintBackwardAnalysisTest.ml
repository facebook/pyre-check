(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Ast
open Statement
open Taint
open Domains

open Test


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
  let { BackwardAnalysis.taint_in_taint_out; _ } =
    Option.value_exn (BackwardAnalysis.run define)
  in
  Taint.SharedMemory.add_model
    ~define:(Access.create define_name)
    { taint_in_taint_out; backward = BackwardState.empty; forward = ForwardState.empty };
  match Taint.SharedMemory.get_model (Access.create define_name) with
  | None -> assert_failure ("no model for " ^ define_name)
  | Some { taint_in_taint_out; _ } ->
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
        ~printer:(Fn.compose Sexp.to_string Int.Set.sexp_of_t)
        expected_positions
        taint_in_taint_out_positions


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
      define_name= "test_concatenate_taint_in_taint_out";
      taint_in_taint_out_parameters = [1];
    }


let () =
  "taint">:::[
    "plus_taint_in_taint_out">::test_plus_taint_in_taint_out;
    "concatenate_taint_in_taint_out">::test_concatenate_taint_in_taint_out;
  ]
  |> run_test_tt_main
