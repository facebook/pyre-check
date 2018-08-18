(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Analysis
open Ast
open Interprocedural
open Statement


let configuration = Configuration.create ()


let environment ?(sources = []) ?(configuration = configuration) () =
  let _ = Test.parse "" in  (* Make sure Test module is loaded. *)
  let environment = Environment.Builder.create () in
  Service.Environment.populate (Environment.handler ~configuration environment) sources;
  Environment.handler ~configuration environment


let setup_environment ?(sources = []) () =
  let () = Scheduler.Daemon.check_entry_point () in
  let environment = environment ~configuration () in
  Service.Environment.populate environment sources;
  let environment = Environment.Builder.create () in
  Service.Environment.populate (Environment.handler ~configuration environment) sources


module ResultA = Interprocedural.Result.Make(struct
    type result = string
    type call_model = int

    let name = "analysisA"
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

    let summaries callable result_option model =
      let result_json = match result_option with
        | None -> `Null
        | Some result -> `String result
      in
      [
        `Assoc [
          "analysis", `String name;
          "name", `String (Callable.show callable);
          "model", `Int model;
          "result", result_json
        ]
      ]
  end)


module AnalysisA = ResultA.Register(struct
    let init ~types:_ ~functions:_ = ()

    let analyze _callable _body =
      "A", 5
  end)


module ResultB = Interprocedural.Result.Make(struct
    type result = int
    type call_model = string

    let name = "analysisB"
    let empty_model = "empty"
    let obscure_model = "obscure"

    let get_errors _ =
      []

    let show_call_model model =
      model

    let join ~iteration:_ a b =
      a ^ b

    let widen ~iteration ~previous ~next =
      join ~iteration previous next

    let reached_fixpoint ~iteration:_ ~previous ~next =
      next <= previous

    let summaries callable result_option model =
      let result_json = match result_option with
        | None -> `Null
        | Some result -> `Int result
      in
      [
        `Assoc [
          "analysis", `String name;
          "name", `String (Callable.show callable);
          "model", `String model;
          "result", result_json;
        ]
      ]
  end)


module AnalysisB = ResultB.Register(struct
    let init ~types:_ ~functions:_ = ()

    let analyze _callable _body =
      7, "B"
  end)


let analyses = [AnalysisA.abstract_kind; AnalysisB.abstract_kind]


let assert_summaries ~expected summaries =
  let json_printer jsons = String.concat ~sep:"\n" (List.map ~f:Yojson.Safe.to_string jsons) in
  let expected = List.map ~f:Yojson.Safe.from_string expected in
  assert_equal ~printer:json_printer ~msg:"json summaries" expected summaries


let test_unknown_function_analysis _ =
  let targets =
    List.map ~f:Access.create ["fun_a"; "fun_b"; "fun_c"]
    |> List.map ~f:(fun access -> Callable.make_real access)
  in
  let step = Fixpoint.{ epoch = 1; iteration = 0; } in
  let () = Analysis.one_analysis_pass step ~analyses ~callables:targets in
  let check_obscure_model target =
    match Fixpoint.get_model target with
    | None ->
        Format.sprintf "no model stored for target %s" (Callable.show target)
        |> assert_failure
    | Some models ->
        assert_equal (Result.get_model ResultA.kind models) (Some ResultA.obscure_model);
        assert_equal (Result.get_model ResultB.kind models) (Some ResultB.obscure_model)
  in
  let summaries = List.concat_map ~f:Analysis.summaries targets in
  List.iter ~f:check_obscure_model targets;
  assert_summaries summaries ~expected:[
    {| {"analysis":"analysisA","name":"`RealTarget (fun_a)","model":-1,"result":null} |};
    {| {"analysis":"analysisB","name":"`RealTarget (fun_a)","model":"obscure","result":null} |};
    {| {"analysis":"analysisA","name":"`RealTarget (fun_b)","model":-1,"result":null} |};
    {| {"analysis":"analysisB","name":"`RealTarget (fun_b)","model":"obscure","result":null} |};
    {| {"analysis":"analysisA","name":"`RealTarget (fun_c)","model":-1,"result":null} |};
    {| {"analysis":"analysisB","name":"`RealTarget (fun_c)","model":"obscure","result":null} |};
  ]


let check_meta_data ~step ~is_partial target =
  match Fixpoint.get_meta_data target with
  | None ->
      Format.sprintf "no meta data stored for target %s" (Callable.show target)
      |> assert_failure
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


let test_meta_data _ =
  let targets =
    List.map ~f:Access.create ["fun_a"; "fun_b"; "fun_c"]
    |> List.map ~f:Callable.make_real in
  let step1 = Fixpoint.{ epoch = 1; iteration = 0; } in
  let () = Analysis.one_analysis_pass step1 ~analyses ~callables:targets in
  (* All obscure functions should reach fixpoint in 1st step *)
  let () = List.iter ~f:(check_meta_data ~step:step1 ~is_partial:false) targets in
  ()


let () =
  setup_environment ();
  "interproceduralAnalysisTest">:::[
    "test_obscure">::test_unknown_function_analysis;
    "test_meta_data">::test_meta_data;
  ]
  |> run_test_tt_main
