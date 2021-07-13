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


let static_analysis_configuration ~transform_configuration { ScratchProject.configuration; _ } =
  {
    Configuration.StaticAnalysis.result_json_path = None;
    dump_call_graph = false;
    verify_models = false;
    configuration = transform_configuration configuration;
    rule_filter = None;
    find_missing_flows = None;
    dump_model_query_results = false;
    use_cache = false;
    maximum_trace_length = None;
    maximum_tito_depth = None;
  }


let analysis = TypeInference.Analysis.abstract_kind

let fixpoint_result ~context ~sources ~callable_names ~transform_configuration =
  let callables =
    let callable_of_string name : Callable.t =
      name |> Reference.create |> Callable.create_function
    in
    callable_names |> List.map ~f:callable_of_string
  in
  let scratch_project = ScratchProject.setup ~context ~infer:true sources in
  let filtered_callables = Callable.Set.of_list callables in
  let environment =
    setup_environment scratch_project |> TypeEnvironment.create |> TypeEnvironment.read_only
  in
  let scheduler = Test.mock_scheduler () in
  let static_analysis_configuration =
    static_analysis_configuration ~transform_configuration scratch_project
  in
  Analysis.initialize_configuration ~static_analysis_configuration analysis;
  Analysis.record_initial_models ~functions:callables ~stubs:[] Callable.Map.empty;
  let fixpoint_iterations =
    let iteration_limit = 1 in
    Some
      (Analysis.compute_fixpoint
         ~scheduler
         ~environment
         ~analysis
         ~dependencies:DependencyGraph.empty
         ~filtered_callables
         ~all_callables:callables
         iteration_limit)
  in
  Analysis.report_results
    ~scheduler
    ~static_analysis_configuration
    ~environment
    ~analysis
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


let assert_fixpoint_result
    ~context
    ~sources
    ~callable_names
    ~expected
    ?(transform_configuration = fun configuration -> configuration)
    ()
  =
  let result = fixpoint_result ~context ~sources ~callable_names ~transform_configuration in
  assert_equal ~ctxt:context 1 (List.length result) ~msg:"Expected length-1 list for result";
  assert_json_equal ~context (List.hd_exn result) ~expected


let type_inference_serialization_test context =
  assert_fixpoint_result
    ~context
    ~sources:
      [
        ( "test.py",
          {|
          import functools

          x = 1 + 1

          class C:
              x = 1 + 1

          def no_errors(x: int) -> int:
              return x

          @functools.lru_cache(4)
          def needs_return(y: int, x: int):
              return x
        |}
        );
      ]
    ~callable_names:
      ["test.no_errors"; "test.needs_return"; "test.$toplevel"; "test.C.$class_toplevel"]
    ~expected:
      {|
        {
          "globals": [
            {
              "name": "x",
              "location": { "qualifier": "test", "path": "test.py", "line": 4 },
              "annotation": "int"
            }
          ],
          "attributes": [
            {
              "parent": "C",
              "name": "x",
              "location": { "qualifier": "test", "path": "test.py", "line": 7 },
              "annotation": "int"
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
              "decorators": [ "functools.lru_cache(4)" ],
              "location": { "qualifier": "test", "path": "test.py", "line": 13 },
              "async": false
            }
          ]
        }
    |}
    ()


let type_inference_duplicate_define_test context =
  (* This is a made-up example because overloads should have empty bodies, but normal libraries can
     implement similar decorator-based behavior that infer needs to handle. Note that whether we
     filter duplicates does not depend on whether there are duplicate _inference_ results; _any_
     duplicates should trigger us to skip _all_ inference results for a given name. *)
  assert_fixpoint_result
    ~context
    ~sources:
      [
        ( "test.py",
          {|
          import typing

          @typing.overload
          def f(x: int, y) -> int:
              y = "y"
              return x

          @typing.overload
          def f(x: str, y) -> str:
              y = "y"
              return x

          @typing.overload
          def f(x: float, y) -> float:
              y = "y"
              return x
          |}
        );
      ]
    ~callable_names:["test.f"]
    ~expected:
      {|
        {
          "globals": [],
          "attributes": [],
          "defines": []
        }
      |}
    ();
  (* The previous commit ensures we ignore duplicate defines when more than one has type inference
     results. This test verifies that even if just one of the defines has inference results, we
     still ignore it (because the codegen logic in python won't know how to annotate correctly even
     if there's only one stub to generate) *)
  assert_fixpoint_result
    ~context
    ~sources:
      [
        ( "test.py",
          {|
          import typing

          @typing.overload
          def f(x: int) -> int:
              return x

          @typing.overload
          def f(x: str) -> str:
              return x

          @typing.overload
          def f(x) -> float:
              return x
          |}
        );
      ]
    ~callable_names:["test.f"]
    ~expected:
      {|
        {
          "globals": [],
          "attributes": [],
          "defines": []
        }
      |}
    ();
  ()


let type_inference_suppress_test context =
  assert_fixpoint_result
    ~context
    ~sources:
      ["test.py", {|
          x = None

          class Foo:
            x = None
          |}]
    ~callable_names:["test.Foo.$class_toplevel"; "test.Foo.__init__"]
    ~expected:
      {|
        {
          "globals": [],
          "attributes": [],
          "defines": []
        }
      |}
    ()


let type_inference_attribute_widen_test context =
  assert_fixpoint_result
    ~context
    ~sources:
      [
        ( "test.py",
          {|
          class Foo:
            x = None
            def __init__(self) -> None:
              self.x = 1 + 1
          |}
        );
      ]
    ~callable_names:["test.Foo.$class_toplevel"; "test.Foo.__init__"]
    ~expected:
      {|
        {
          "globals": [],
          "attributes": [
            {
              "parent": "Foo",
              "name": "x",
              "location": { "qualifier": "test", "path": "test.py", "line": 3 },
              "annotation": "typing.Optional[int]"
            }
          ],
          "defines": []
        }
      |}
    ()


let type_inference_ignore_infer_test context =
  let assert_fixpoint_result =
    assert_fixpoint_result
      ~context
      ~sources:["test.py", {|
          x = 1 + 1
          |}]
      ~callable_names:["test.$toplevel"]
  in
  assert_fixpoint_result
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
          "attributes": [],
          "defines": []
        }
      |}
    ();
  assert_fixpoint_result
    ~transform_configuration:(fun configuration ->
      {
        configuration with
        Configuration.Analysis.ignore_infer =
          [PyrePath.create_relative ~root:configuration.local_root ~relative:"test.py"];
      })
    ~expected:
      {|
        {
          "globals": [],
          "attributes": [],
          "defines": []
        }
      |}
    ();
  ()


let () =
  "typeInferenceAnalysisTest"
  >::: [
         "serialization" >:: type_inference_serialization_test;
         "duplicates" >:: type_inference_duplicate_define_test;
         "suppress" >:: type_inference_suppress_test;
         "attribute_widen" >:: type_inference_attribute_widen_test;
         "ignore_infer" >:: type_inference_ignore_infer_test;
       ]
  |> Test.run
