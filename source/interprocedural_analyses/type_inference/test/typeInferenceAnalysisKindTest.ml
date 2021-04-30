(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
module TypeAnalysis = Analysis
open Interprocedural
open Test

let setup_scratch_project ~context ?(sources = []) () = ScratchProject.setup ~context sources

let setup_environment scratch_project =
  let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
    scratch_project |> ScratchProject.build_global_environment
  in
  global_environment


let static_analysis_configuration { ScratchProject.configuration; _ } =
  {
    Configuration.StaticAnalysis.result_json_path = None;
    dump_call_graph = false;
    verify_models = false;
    configuration;
    rule_filter = None;
    find_missing_flows = None;
    dump_model_query_results = false;
    use_cache = false;
  }


let analyses = [TypeInference.Analysis.abstract_kind]

let assert_summaries ~expected summaries =
  let json_printer jsons = String.concat ~sep:"\n" (List.map ~f:Yojson.Safe.to_string jsons) in
  let expected = List.map ~f:Yojson.Safe.from_string expected in
  assert_equal ~printer:json_printer ~msg:"json summaries" expected summaries


let test_fixpoint_wiring context =
  let callable_of_string name : Callable.t = name |> Reference.create |> Callable.create_function in
  let targets = List.map ["fun_a"; "fun_b"; "fun_c"] ~f:callable_of_string in
  let step = Fixpoint.{ epoch = 1; iteration = 0 } in
  let scratch_project = setup_scratch_project ~context () in
  let environment =
    setup_environment scratch_project
    |> TypeAnalysis.TypeEnvironment.create
    |> TypeAnalysis.TypeEnvironment.read_only
  in
  let _ = Analysis.one_analysis_pass ~step ~analyses ~environment ~callables:targets in
  let report =
    let static_analysis_configuration = static_analysis_configuration scratch_project in
    Analysis.report_results
      ~scheduler:(Test.mock_scheduler ())
      ~static_analysis_configuration
      ~analyses
      ~filename_lookup:(fun _ -> None)
      ~callables:(targets |> Callable.Set.of_list)
      ~skipped_overrides:[]
      ~fixpoint_timer:(Timer.start ())
      ~fixpoint_iterations:None
  in
  assert_equal report []


let () =
  "typeInferenceAnalysisKindTest" >::: ["fixpoint_wiring" >:: test_fixpoint_wiring] |> Test.run
