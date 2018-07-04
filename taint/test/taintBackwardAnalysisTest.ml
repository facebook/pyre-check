(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Ast
open Statement
open Taint

open Test


let parse_source source =
  let metadata =
    Source.Metadata.create
      ~number_of_lines:(-1)
      ()
  in
  parse ~qualifier:[] source
  |> (fun source -> { source with Source.metadata })
  |> Preprocessing.preprocess


type tito_expectation = {
  define_name:string;
  tito_parameters: int list;
}

let get_model models name =
  List.find models ~f:(fun { BackwardAnalysis.define_name; _ } -> Access.show define_name = name)

let assert_tito source tito =
  let { Node.value = define; _ } =
    parse_source source
    |> Preprocessing.defines
    |> List.hd_exn
  in
  match BackwardAnalysis.run (Cfg.create define) with
  | None -> assert_equal true false ~msg:"produced no result"
  | Some result ->
      match get_model result.BackwardAnalysis.FixpointState.models tito.define_name with
      | None -> assert_equal true false ~msg:("no tito model for "^tito.define_name)
      | Some model ->
          let extract_parameter_pos k _ positions =
            match k with
            | AccessPath.Root.Parameter { position; _ } -> Int.Set.add positions position
            | _ -> positions
          in
          let tito_positions =
            Domains.BackwardState.fold model.taint_in_taint_out
              ~f:extract_parameter_pos
              ~init:Int.Set.empty in
          let expected_positions = Int.Set.of_list tito.tito_parameters in
          assert_equal
            ~cmp:Int.Set.equal
            ~printer:(fun s -> Int.Set.sexp_of_t s |> Sexp.to_string)
            expected_positions
            tito_positions

let test_plus_tito _ =
  assert_tito
    {|
    def test_plus_tito(tainted_param1, param2):
      tainted_value = tainted_param1 + 5
      return tainted_value
    |}
    {
      define_name="test_plus_tito";
      tito_parameters=[0];
    }


let test_concat_tito _ =
  assert_tito
    {|
    def test_concat_tito(param0, tainted_param1):
      unused_param = param0
      command_unsafe = 'echo' + tainted_param1 + ' >> /dev/null'
      return command_unsafe
    |}
    {
      define_name="test_concat_tito";
      tito_parameters=[1];
    }

let () =
  "taint">:::[
    "plus_tito">::test_plus_tito;
    "concat_tito">::test_concat_tito;
  ]
  |> run_test_tt_main
