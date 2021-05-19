(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Analysis
open Test
module Callable = Interprocedural.Callable

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


module Setup = struct
  let make_function name : Callable.real_target =
    name |> Reference.create |> Callable.create_function


  let find_target ~resolution target =
    let qualifier, define =
      match target |> Callable.get_module_and_definition ~resolution with
      | Some module_and_definition -> module_and_definition
      | None ->
          let all_defines =
            resolution
            |> GlobalResolution.unannotated_global_environment
            |> UnannotatedGlobalEnvironment.ReadOnly.all_defines
          in
          raise
            (Failure
               (Format.asprintf
                  "No such define %a in %s"
                  Callable.pp_real_target
                  target
                  (all_defines |> List.map ~f:Reference.show |> String.concat ~sep:",")))
    in
    qualifier, define


  let set_up_project ~context source =
    let project = ScratchProject.setup ~context ["test.py", source] ~infer:true in
    let static_analysis_configuration = static_analysis_configuration project in
    let environment =
      setup_environment project |> TypeEnvironment.create |> TypeEnvironment.read_only
    in
    let _ =
      TypeInference.Private.SharedMemory.register_configuration
        static_analysis_configuration.configuration
    in
    environment


  let run_inference ~context ~targets source =
    let environment = set_up_project ~context source in
    let resolution = environment |> TypeEnvironment.ReadOnly.global_resolution in
    let analyze target =
      let qualifier, define = find_target ~resolution target in
      let result, _ =
        TypeInference.Analysis.analyze
          ~callable:target
          ~environment
          ~qualifier
          ~define
          ~existing:None
      in
      target, result
    in
    targets |> List.map ~f:analyze
end

let check_inference_results ~context ~checks source =
  let targets, _ = List.unzip checks in
  let results = Setup.run_inference ~context ~targets source in
  let check_result (target, result) =
    let check = List.Assoc.find_exn ~equal:Callable.equal_real_target checks target in
    check result
  in
  results |> List.iter ~f:check_result


let test_inferred_returns context =
  let check_return_annotation ~define_name source =
    let check errors = assert_equal (List.length errors) 1 in
    check_inference_results ~context ~checks:[Setup.make_function define_name, check] source
  in
  check_return_annotation {|
      def foo(x: int):
        return x
    |} ~define_name:"test.foo"


let test_inferred_parameters context =
  let check_parameter_annotation ~define_name source =
    let check errors = assert_equal (List.length errors) 1 in
    check_inference_results ~context ~checks:[Setup.make_function define_name, check] source
  in
  check_parameter_annotation
    {|
      def foo(x) -> int:
        return x
    |}
    ~define_name:"test.foo"


let test_inferred_globals context =
  let check_global_annotation ~define_name source =
    let check errors = assert_equal (List.length errors) 1 in
    check_inference_results ~context ~checks:[Setup.make_function define_name, check] source
  in
  check_global_annotation {|
      x = None
    |} ~define_name:"test.$toplevel"


let test_inferred_attributes context =
  let check_attribute_annotation ~define_name source =
    let check errors = assert_equal (List.length errors) 1 in
    check_inference_results ~context ~checks:[Setup.make_function define_name, check] source;
    Memory.reset_shared_memory ()
  in
  check_attribute_annotation
    {|
      class C:
          x = None
    |}
    ~define_name:"test.C.$class_toplevel"


let () =
  "typeInferenceAnalysisTest"
  >::: [
         "test_inferred_returns" >:: test_inferred_returns;
         "test_inferred_parameters" >:: test_inferred_parameters;
         "test_inferred_globals" >:: test_inferred_globals;
         "test_inferred_attributes" >:: test_inferred_attributes;
       ]
  |> Test.run
