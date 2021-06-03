(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
module TypeEnvironment = Analysis.TypeEnvironment
open Test
open Interprocedural

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
    maximum_tito_depth = None;
  }


let analyses = [TypeInference.Analysis.abstract_kind]

let fixpoint_result ~context ~callables ~sources =
  let scratch_project = ScratchProject.setup ~context ~infer:true sources in
  let filtered_callables = Callable.Set.of_list callables in
  let environment =
    setup_environment scratch_project |> TypeEnvironment.create |> TypeEnvironment.read_only
  in
  let scheduler = Test.mock_scheduler () in
  let static_analysis_configuration = static_analysis_configuration scratch_project in
  Analysis.initialize_configuration ~static_analysis_configuration analyses;
  Analysis.record_initial_models ~functions:callables ~stubs:[] Callable.Map.empty;
  let fixpoint_iterations =
    let iteration_limit = 1 in
    Some
      (Analysis.compute_fixpoint
         ~scheduler
         ~environment
         ~analyses
         ~dependencies:DependencyGraph.empty
         ~filtered_callables
         ~all_callables:callables
         iteration_limit)
  in
  Analysis.report_results
    ~scheduler
    ~static_analysis_configuration
    ~environment
    ~analyses
    ~filename_lookup:(fun _ -> None)
    ~callables:filtered_callables
    ~skipped_overrides:[]
    ~fixpoint_timer:(Timer.start ())
    ~fixpoint_iterations


let assert_json_equal ~context ~expected result =
  let expected = Yojson.Safe.from_string expected in
  assert_equal
    ~ctxt:context
    ~printer:Yojson.Safe.pretty_to_string
    ~msg:"GlobalResult json"
    expected
    result


let type_inference_integration_test context =
  let sources =
    [
      ( "test.py",
        {|
          x = 1 + 1

          class C:
              x = None

          def no_errors(x: int) -> int:
              return x

          def needs_return(y: int, x: int):
              return x
        |}
      );
    ]
  in
  let callables =
    let callable_of_string name : Callable.t =
      name |> Reference.create |> Callable.create_function
    in
    List.map
      ["test.no_errors"; "test.needs_return"; "test.$toplevel"; "test.C.$class_toplevel"]
      ~f:callable_of_string
  in
  let result = fixpoint_result ~context ~callables ~sources in
  assert_equal ~ctxt:context 1 (List.length result) ~msg:"Expected length-1 list for result";
  assert_json_equal
    ~context
    (List.hd_exn result)
    ~expected:
      {|
        {
          "globals": [
            {
              "name": "x",
              "location": { "qualifier": "test", "path": "test.py", "line": 2 },
              "annotation": "int"
            }
          ],
          "attributes": [
            {
              "parent": "C",
              "name": "x",
              "location": { "qualifier": "test", "path": "test.py", "line": 5 },
              "annotation": "None"
            }
          ],
          "defines": [
            {
              "name": "test.needs_return",
              "parent": null,
              "return": "int",
              "parameters": [
                { "name": "y", "annotation": "int", "value": null, "index": 0 },
                { "name": "x", "annotation": "int", "value": null, "index": 1 }
              ],
              "decorators": [],
              "location": { "qualifier": "test", "path": "test.py", "line": 10 },
              "async": false
            }
          ]
        }
    |}


let () =
  "typeInferenceAnalysisTest" >::: ["integration" >:: type_inference_integration_test] |> Test.run
