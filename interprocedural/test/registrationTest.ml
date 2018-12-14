(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Interprocedural


module SimpleAnalysis = Interprocedural.Result.Make(struct
    type result = string
    type call_model = int

    let name = "simple-test-analysis"
    let empty_model = 0
    let obscure_model = -1

    let get_errors _ =
      []

    let show_call_model = string_of_int

    let join ~iteration:_ a b =
      a + b

    let widen ~iteration ~previous ~next =
      join ~iteration previous next

    let reached_fixpoint ~iteration:_ ~previous ~next =
      next <= previous

    let externalize _ _ _ = []

    let metadata () = `Assoc ["foo", `String "bar"]
  end)


include SimpleAnalysis.Register(struct
    let init ~types:_ ~functions:_ = ()

    let analyze ~callable:_ ~environment:_ ~define:_ =
      "some result", 5
  end)


let test_simple_analysis _ =
  match  AnalysisKind.analysis_by_name SimpleAnalysis.name with
  | None -> assert_failure "Lookup of analysis module failed."
  | Some analysis_kind ->
      let Result.Analysis { analysis; _ } = Result.get_abstract_analysis analysis_kind in
      let module Analysis = (val analysis) in
      assert_equal (Analysis.empty_model |> Analysis.show_call_model) "0";
      assert_equal (Analysis.obscure_model |> Analysis.show_call_model) "-1";
      ()


let () =
  "interproceduralRegistration">:::[
    "test_simple_analysis">::test_simple_analysis;
  ]
  |> Test.run
