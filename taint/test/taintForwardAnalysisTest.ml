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


let parse_source source =
  parse ~qualifier:[] source
  |> Preprocessing.preprocess


type source_expectation = {
  define_name: string;
  returns: Sources.t list;
}


let get_model models name =
  List.find models ~f:(fun { ForwardAnalysis.define_name; _ } -> Access.show define_name = name)


let assert_sources source expectation =
  let { Node.value = define; _ } =
    parse_source source
    |> Preprocessing.defines
    |> List.hd_exn
  in
  match ForwardAnalysis.run (Cfg.create define) with
  | None -> assert_failure "produced no result"
  | Some result ->
      match get_model result.ForwardAnalysis.FixpointState.models expectation.define_name with
      | None -> assert_failure ("no model for "^expectation.define_name)
      | Some model ->
          let returned_sources =
            ForwardState.read AccessPath.Root.LocalResult model.source_taint
            |> ForwardState.collapse
            |> ForwardTaint.leaves
            |> List.map ~f:Sources.show
            |> String.Set.of_list
          in
          let expected_sources =
            List.map ~f:Sources.show expectation.returns
            |> String.Set.of_list
          in
          assert_equal
            ~cmp:String.Set.equal
            ~printer:(Fn.compose Sexp.to_string String.Set.sexp_of_t)
            expected_sources
            returned_sources


let test_simple_source _ =
  assert_sources
    {|
    def simple_source():
      return __testSource()
    |}
    {
      define_name = "simple_source";
      returns = [Sources.TestSource];
    }


let test_local_copy _ =
  assert_sources
    {|
    def copy_source():
      var = __testSource()
      return var
    |}
    {
      define_name = "copy_source";
      returns = [Sources.TestSource];
    }


let () =
  "taint">:::[
    "simple">::test_simple_source;
    "copy">::test_local_copy;
  ]
  |> run_test_tt_main
