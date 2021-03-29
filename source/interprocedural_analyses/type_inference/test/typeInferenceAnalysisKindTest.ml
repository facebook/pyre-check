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

let setup_environment ~context ?(sources = []) () =
  let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
    ScratchProject.setup ~context sources |> ScratchProject.build_global_environment
  in
  global_environment


let analyses = [TypeInference.Analysis.abstract_kind]

let assert_summaries ~expected summaries =
  let json_printer jsons = String.concat ~sep:"\n" (List.map ~f:Yojson.Safe.to_string jsons) in
  let expected = List.map ~f:Yojson.Safe.from_string expected in
  assert_equal ~printer:json_printer ~msg:"json summaries" expected summaries


let test_fixpoint_wiring context =
  let targets =
    List.map ~f:Reference.create ["fun_a"; "fun_b"; "fun_c"]
    |> List.map ~f:(fun name -> Callable.create_function name)
  in
  let step = Fixpoint.{ epoch = 1; iteration = 0 } in
  let environment =
    setup_environment ~context ()
    |> TypeAnalysis.TypeEnvironment.create
    |> TypeAnalysis.TypeEnvironment.read_only
  in
  let _ = Analysis.one_analysis_pass ~step ~analyses ~environment ~callables:targets in
  let externalized =
    List.concat_map
      ~f:
        (Analysis.externalize ~filename_lookup:(fun _ -> None) TypeInference.Analysis.abstract_kind)
      targets
  in
  assert_summaries
    externalized
    ~expected:
      [
        {| {"analysis":"type_inference","name":"fun_a (fun)","model":"TypeInferenceDomain.Bottom","result":null} |};
        {| {"analysis":"type_inference","name":"fun_b (fun)","model":"TypeInferenceDomain.Bottom","result":null} |};
        {| {"analysis":"type_inference","name":"fun_c (fun)","model":"TypeInferenceDomain.Bottom","result":null} |};
      ]


let () =
  "typeInferenceAnalysisKindTest" >::: ["fixpoint_wiring" >:: test_fixpoint_wiring] |> Test.run
