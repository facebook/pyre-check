(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
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
    maximum_trace_length = None;
  }


module ResultA = Interprocedural.Result.Make (struct
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
  let initialize_configuration ~static_analysis_configuration:_ = ()

  let initialize_models
      ~scheduler:_
      ~static_analysis_configuration:_
      ~environment:_
      ~functions:_
      ~stubs:_
    =
    { Result.initial_models = Callable.Map.empty; skip_overrides = Ast.Reference.Set.empty }


  let analyze ~environment:_ ~callable:_ ~qualifier:_ ~define:_ ~existing:_ = "A", 5

  let report
      ~scheduler:_
      ~static_analysis_configuration:_
      ~environment:_
      ~filename_lookup:_
      ~callables
      ~skipped_overrides:_
      ~fixpoint_timer:_
      ~fixpoint_iterations:_
    =
    let get_model callable : Yojson.Safe.json =
      let model =
        Fixpoint.get_model callable
        >>= Result.get_model ResultA.kind
        >>| (fun r -> `Int r)
        |> Option.value ~default:`Null
      in
      let result =
        Fixpoint.get_result callable
        |> Result.get_result ResultA.kind
        >>| (fun r -> `String r)
        |> Option.value ~default:`Null
      in
      `Assoc
        [
          "analysis", `String ResultA.name;
          "callable", `String (Callable.show callable);
          "model", model;
          "result", result;
        ]
    in
    callables |> Callable.Set.elements |> List.map ~f:get_model
end)

module ResultB = Interprocedural.Result.Make (struct
  type result = int

  type call_model = string [@@deriving show]

  let name = "analysisB"

  let empty_model = "empty"

  let obscure_model = "obscure"

  let join ~iteration:_ a b = a ^ b

  let widen ~iteration ~previous ~next = join ~iteration previous next

  let reached_fixpoint ~iteration:_ ~previous ~next = String.(next <= previous)

  let strip_for_callsite model = model
end)

module AnalysisB = ResultB.Register (struct
  let initialize_configuration ~static_analysis_configuration:_ = ()

  let initialize_models
      ~scheduler:_
      ~static_analysis_configuration:_
      ~environment:_
      ~functions:_
      ~stubs:_
    =
    { Result.initial_models = Callable.Map.empty; skip_overrides = Reference.Set.empty }


  let analyze ~environment:_ ~callable:_ ~qualifier:_ ~define:_ ~existing:_ = 7, "B"

  let report
      ~scheduler:_
      ~static_analysis_configuration:_
      ~environment:_
      ~filename_lookup:_
      ~callables
      ~skipped_overrides:_
      ~fixpoint_timer:_
      ~fixpoint_iterations:_
    =
    let get_model callable : Yojson.Safe.json =
      let model =
        Fixpoint.get_model callable
        >>= Result.get_model ResultB.kind
        >>| (fun r -> `String r)
        |> Option.value ~default:`Null
      in
      let result =
        Fixpoint.get_result callable
        |> Result.get_result ResultB.kind
        >>| (fun r -> `Int r)
        |> Option.value ~default:`Null
      in
      `Assoc
        [
          "analysis", `String ResultB.name;
          "callable", `String (Callable.show callable);
          "model", model;
          "result", result;
        ]
    in
    callables |> Callable.Set.elements |> List.map ~f:get_model
end)

let analyses = [AnalysisA.abstract_kind; AnalysisB.abstract_kind]

let assert_report ~expected report =
  let json_printer jsons = String.concat ~sep:"\n" (List.map ~f:Yojson.Safe.to_string jsons) in
  let sort_json json =
    json |> List.sort ~compare:String.compare |> List.map ~f:Yojson.Safe.from_string
  in
  let expected = sort_json expected in
  let report = report |> List.map ~f:Yojson.Safe.to_string |> sort_json in
  assert_equal ~printer:json_printer ~msg:"json report" expected report


let test_unknown_function_analysis context =
  let callable_of_string name = name |> Reference.create |> Callable.create_function in
  let targets = List.map ["fun_a"; "fun_b"] ~f:callable_of_string in
  let scratch_project = setup_scratch_project ~context () in
  let environment =
    setup_environment scratch_project
    |> TypeAnalysis.TypeEnvironment.create
    |> TypeAnalysis.TypeEnvironment.read_only
  in
  let step = Fixpoint.{ epoch = 1; iteration = 0 } in
  let _ = Analysis.one_analysis_pass ~step ~analyses ~environment ~callables:targets in
  (* Make sure obscure models are correctly handled *)
  let check_obscure_model target =
    match Fixpoint.get_model target with
    | None ->
        Format.sprintf "no model stored for target %s" (Callable.show target) |> assert_failure
    | Some models ->
        assert_equal (Result.get_model ResultA.kind models) (Some ResultA.obscure_model);
        assert_equal (Result.get_model ResultB.kind models) (Some ResultB.obscure_model)
  in
  List.iter ~f:check_obscure_model targets;
  (* Make sure result extraction works (this verifies a lot of the type magic) *)
  let report =
    let static_analysis_configuration = static_analysis_configuration scratch_project in
    Analysis.report_results
      ~scheduler:(Test.mock_scheduler ())
      ~static_analysis_configuration
      ~environment
      ~analyses
      ~filename_lookup:(fun _ -> None)
      ~callables:(targets |> Callable.Set.of_list)
      ~skipped_overrides:[]
      ~fixpoint_timer:(Timer.start ())
      ~fixpoint_iterations:None
  in
  assert_report
    report
    ~expected:
      [
        {| {"analysis":"analysisA","callable":"fun_a (fun)","model":-1,"result":null} |};
        {| {"analysis":"analysisA","callable":"fun_b (fun)","model":-1,"result":null} |};
        {| {"analysis":"analysisB","callable":"fun_a (fun)","model":"obscure","result":null} |};
        {| {"analysis":"analysisB","callable":"fun_b (fun)","model":"obscure","result":null} |};
      ]


let check_meta_data ~step ~is_partial target =
  match Fixpoint.get_meta_data target with
  | None ->
      Format.sprintf "no meta data stored for target %s" (Callable.show target) |> assert_failure
  | Some { is_partial = stored_is_partial; step = stored_step } ->
      let target_name = Callable.show target in
      assert_equal
        is_partial
        stored_is_partial
        ~msg:(Format.sprintf "is_partial %s" target_name)
        ~printer:Bool.to_string;
      assert_equal
        step
        stored_step
        ~msg:(Format.sprintf "step %s" target_name)
        ~printer:Fixpoint.show_step


let test_meta_data context =
  let targets =
    List.map ~f:Reference.create ["fun_a"; "fun_b"; "fun_c"] |> List.map ~f:Callable.create_function
  in
  let step1 = Fixpoint.{ epoch = 1; iteration = 0 } in
  let environment =
    setup_scratch_project ~context ()
    |> setup_environment
    |> TypeAnalysis.TypeEnvironment.create
    |> TypeAnalysis.TypeEnvironment.read_only
  in
  let _ = Analysis.one_analysis_pass ~step:step1 ~analyses ~environment ~callables:targets in
  (* All obscure functions should reach fixpoint in 1st step *)
  let () = List.iter ~f:(check_meta_data ~step:step1 ~is_partial:false) targets in
  ()


let () =
  "interproceduralAnalysisTest"
  >::: ["obscure" >:: test_unknown_function_analysis; "meta_data" >:: test_meta_data]
  |> Test.run
