(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Analysis
open Interprocedural
open Test

let setup_scratch_project ~context ?(sources = []) () = ScratchProject.setup ~context sources

let setup_environment scratch_project =
  let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
    scratch_project |> ScratchProject.build_global_environment
  in
  global_environment


module ResultA = Interprocedural.AnalysisResult.Make (struct
  type result = string

  type call_model = int [@@deriving show]

  let name = "analysisA"

  let empty_model = 0

  let obscure_model = -1

  let join ~iteration:_ a b = a + b

  let widen ~iteration ~previous ~next = join ~iteration previous next

  let reached_fixpoint ~iteration:_ ~previous ~next = next <= previous

  let strip_for_callsite model = model
end)

module AnalysisA = ResultA.Register (struct
  let initialize_models
      ~scheduler:_
      ~static_analysis_configuration:_
      ~environment:_
      ~callables:_
      ~stubs:_
    =
    AnalysisResult.empty_initialize_result


  let analyze ~environment:_ ~callable:_ ~qualifier:_ ~define:_ ~existing:_ = "A", 5
end)

let analysis = AnalysisA.abstract_kind

let test_unknown_function_analysis context =
  let callable_of_string name = name |> Reference.create |> Target.create_function in
  let targets = List.map ["fun_a"; "fun_b"] ~f:callable_of_string in
  let scratch_project = setup_scratch_project ~context () in
  let environment =
    setup_environment scratch_project |> TypeEnvironment.create |> TypeEnvironment.read_only
  in
  let step = FixpointState.{ epoch = 1; iteration = 0 } in
  let _ = FixpointAnalysis.one_analysis_pass ~step ~analysis ~environment ~callables:targets in
  (* Make sure obscure models are correctly handled *)
  let check_obscure_model target =
    match FixpointState.get_model target with
    | None ->
        Format.sprintf "no model stored for target %s" (Target.show_pretty target) |> assert_failure
    | Some models ->
        assert_equal (AnalysisResult.get_model ResultA.kind models) (Some ResultA.obscure_model)
  in
  List.iter ~f:check_obscure_model targets


let check_meta_data ~step ~is_partial target =
  match FixpointState.get_meta_data target with
  | None ->
      Format.asprintf "no meta data stored for target %a" Target.pp_pretty target |> assert_failure
  | Some { is_partial = stored_is_partial; step = stored_step } ->
      let target_name = Target.show_pretty target in
      assert_equal
        is_partial
        stored_is_partial
        ~msg:(Format.sprintf "is_partial %s" target_name)
        ~printer:Bool.to_string;
      assert_equal
        step
        stored_step
        ~msg:(Format.sprintf "step %s" target_name)
        ~printer:FixpointState.show_step


let test_meta_data context =
  let targets =
    List.map ~f:Reference.create ["fun_a"; "fun_b"; "fun_c"] |> List.map ~f:Target.create_function
  in
  let step1 = FixpointState.{ epoch = 1; iteration = 0 } in
  let environment =
    setup_scratch_project ~context ()
    |> setup_environment
    |> TypeEnvironment.create
    |> TypeEnvironment.read_only
  in
  let _ =
    FixpointAnalysis.one_analysis_pass ~step:step1 ~analysis ~environment ~callables:targets
  in
  (* All obscure functions should reach fixpoint in 1st step *)
  let () = List.iter ~f:(check_meta_data ~step:step1 ~is_partial:false) targets in
  ()


let () =
  "interproceduralAnalysisTest"
  >::: ["obscure" >:: test_unknown_function_analysis; "meta_data" >:: test_meta_data]
  |> Test.run
