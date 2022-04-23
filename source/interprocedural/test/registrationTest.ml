(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Interprocedural

module SimpleAnalysis = Interprocedural.AnalysisResult.Make (struct
  type result = string

  type call_model = int [@@deriving show]

  let name = "simple-test-analysis"

  let empty_model = 0

  let obscure_model = -1

  let join ~iteration:_ a b = a + b

  let widen ~iteration ~previous ~next = join ~iteration previous next

  let reached_fixpoint ~iteration:_ ~previous ~next = next <= previous

  let strip_for_callsite model = model
end)

include SimpleAnalysis.Register (struct
  let initialize_models
      ~scheduler:_
      ~static_analysis_configuration:_
      ~environment:_
      ~callables:_
      ~stubs:_
    =
    AnalysisResult.empty_initialize_result


  let analyze ~environment:_ ~callable:_ ~qualifier:_ ~define:_ ~existing:_ = "some result", 5

  let report
      ~scheduler:_
      ~static_analysis_configuration:_
      ~environment:_
      ~filename_lookup:_
      ~callables:_
      ~skipped_overrides:_
      ~fixpoint_timer:_
      ~fixpoint_iterations:_
    =
    []
end)

let test_simple_analysis _ =
  match AnalysisKind.analysis_by_name SimpleAnalysis.name with
  | None -> assert_failure "Lookup of analysis module failed."
  | Some analysis_kind ->
      let (AnalysisResult.Analysis { analysis; _ }) =
        AnalysisResult.get_abstract_analysis analysis_kind
      in
      let module Analysis = (val analysis) in
      assert_equal (Analysis.empty_model |> Analysis.show_call_model) "0";
      assert_equal (Analysis.obscure_model |> Analysis.show_call_model) "-1";
      ()


let () = "interproceduralRegistration" >::: ["simple_analysis" >:: test_simple_analysis] |> Test.run
