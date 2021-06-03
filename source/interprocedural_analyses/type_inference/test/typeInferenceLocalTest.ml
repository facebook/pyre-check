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
open TypeInference.Data
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
    maximum_tito_depth = None;
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

module Asserts = struct
  type type_option = Type.t option [@@deriving show, eq]

  let assert_inference_result ~source ~result ~details ~context ~expected ~actual =
    let msg =
      Format.asprintf
        {| Mismatch in inference result
         ---
         %a
         ---
         %s
         ---
         %s |}
        LocalResult.pp
        result
        source
        details
    in
    assert_equal ~ctxt:context ~cmp:equal_type_option ~printer:show_type_option ~msg expected actual


  let find_by_name name map =
    match name |> Reference.create |> SerializableReference.Map.find map with
    | Some value -> value
    | None ->
        raise
          (Failure
             (Format.asprintf
                "No such name %s in %s"
                name
                ( map
                |> SerializableReference.Map.keys
                |> List.map ~f:Reference.show
                |> String.concat ~sep:"," )))
end

let check_inference_results ~context ~checks source =
  let targets, _ = List.unzip checks in
  let results = Setup.run_inference ~context ~targets source in
  let check_result (target, result) =
    let check = List.Assoc.find_exn ~equal:Callable.equal_real_target checks target in
    check result
  in
  results |> List.iter ~f:check_result;
  Memory.reset_shared_memory ()


let test_inferred_returns context =
  let check_return_annotation ~define_name ~expected source =
    let check
        ( { LocalResult.define = { DefineAnnotation.return = { TypeAnnotation.inferred; _ }; _ }; _ }
        as result )
      =
      Asserts.assert_inference_result
        ~context
        ~result
        ~details:"Checking inferred return annotation"
        ~source
        ~expected:(Some expected)
        ~actual:inferred
    in
    check_inference_results ~context ~checks:[Setup.make_function define_name, check] source
  in
  check_return_annotation
    {|
      def foo(x: int):
        return x
    |}
    ~define_name:"test.foo"
    ~expected:(Type.Primitive "int")


let test_inferred_parameters context =
  let open DefineAnnotation.Parameters in
  let check_parameter_annotation ~define_name ~parameter_name ~expected source =
    let check ({ LocalResult.define = { DefineAnnotation.parameters; _ }; _ } as result) =
      let { Value.annotation = { TypeAnnotation.inferred; _ }; _ } =
        Asserts.find_by_name ("$parameter$" ^ parameter_name) parameters
      in
      Asserts.assert_inference_result
        ~context
        ~result
        ~details:("Checking inferred annotation for parameter " ^ parameter_name)
        ~source
        ~expected:(Some expected)
        ~actual:inferred
    in
    check_inference_results ~context ~checks:[Setup.make_function define_name, check] source
  in
  check_parameter_annotation
    {|
      def foo(x) -> int:
        return x
    |}
    ~define_name:"test.foo"
    ~parameter_name:"x"
    ~expected:(Type.Primitive "int")


let test_inferred_globals context =
  let check_global_annotation ~define_name ~global_name ~expected source =
    let open GlobalAnnotation in
    let check ({ LocalResult.globals; _ } as result) =
      let { Value.annotation = { TypeAnnotation.inferred; _ }; _ } =
        Asserts.find_by_name global_name globals
      in
      Asserts.assert_inference_result
        ~context
        ~result
        ~details:("Checking inferred annotation for global " ^ global_name)
        ~source
        ~expected:(Some expected)
        ~actual:inferred
    in
    check_inference_results ~context ~checks:[Setup.make_function define_name, check] source
  in
  check_global_annotation
    {|
      x = None
    |}
    ~define_name:"test.$toplevel"
    ~global_name:"test.$local_test$x"
    ~expected:Type.NoneType


let test_inferred_attributes context =
  let check_attribute_annotation ~define_name ~attribute_name ~expected source =
    let open AttributeAnnotation in
    let check ({ LocalResult.attributes; _ } as result) =
      let { Value.annotation = { TypeAnnotation.inferred; _ }; _ } =
        Asserts.find_by_name attribute_name attributes
      in
      Asserts.assert_inference_result
        ~context
        ~result
        ~details:("Checking inferred annotation for attribute " ^ attribute_name)
        ~source
        ~expected:(Some expected)
        ~actual:inferred
    in
    check_inference_results ~context ~checks:[Setup.make_function define_name, check] source
  in
  check_attribute_annotation
    {|
      class C:
          x = None
    |}
    ~define_name:"test.C.$class_toplevel"
    ~attribute_name:"test.C.x"
    ~expected:Type.NoneType


let () =
  "typeInferenceLocalTest"
  >::: [
         "test_inferred_returns" >:: test_inferred_returns;
         "test_inferred_parameters" >:: test_inferred_parameters;
         "test_inferred_globals" >:: test_inferred_globals;
         "test_inferred_attributes" >:: test_inferred_attributes;
       ]
  |> Test.run
