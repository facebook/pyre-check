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

let assert_json_equal ~context ~expected result =
  let expected = Yojson.Safe.from_string expected in
  assert_equal ~ctxt:context ~printer:Yojson.Safe.pretty_to_string expected result


let check_inference_results ~context ~target ~expected source =
  let results = Setup.run_inference ~context ~targets:[Setup.make_function target] source in
  assert_equal (List.length results) 1;
  let result = results |> List.hd_exn |> snd |> LocalResult.to_yojson in
  (* Filter out toplevel and __init__ defines, which are verbose and uninteresting *)
  let actual =
    if
      String.is_suffix target ~suffix:"__init__" or String.is_substring target ~substring:"toplevel"
    then
      match result with
      | `Assoc pairs ->
          `Assoc (pairs |> List.filter ~f:(fun (key, _) -> not (String.equal key "define")))
      | json -> json
    else
      result
  in
  assert_json_equal ~context ~expected actual


let test_inferred_returns context =
  let check_inference_results = check_inference_results ~context in
  check_inference_results
    {|
      def foo(x: int):
        return x
    |}
    ~target:"test.foo"
    ~expected:
      {|
        {
          "globals": [],
          "attributes": [],
          "define": {
            "name": "test.foo",
            "parent": null,
            "return": "int",
            "parameters": [
              { "name": "x", "annotation": "int", "value": null, "index": 0 }
            ],
            "decorators": [],
            "location": { "qualifier": "test", "path": "test.py", "line": 2 },
            "async": false
          },
          "abstract": false
        }
      |}


let test_inferred_parameters context =
  let check_inference_results = check_inference_results ~context in
  check_inference_results
    {|
      def foo(x) -> int:
        return x
    |}
    ~target:"test.foo"
    ~expected:
      {|
        {
          "globals": [],
          "attributes": [],
          "define": {
            "name": "test.foo",
            "parent": null,
            "return": "int",
            "parameters": [
              { "name": "x", "annotation": "int", "value": null, "index": 0 }
            ],
            "decorators": [],
            "location": { "qualifier": "test", "path": "test.py", "line": 2 },
            "async": false
          },
          "abstract": false
        }
      |}


let test_inferred_globals context =
  let check_inference_results = check_inference_results ~context in
  check_inference_results
    {|
      x = None
    |}
    ~target:"test.$toplevel"
    ~expected:
      {|
        {
          "globals": [
            {
              "name": "x",
              "location": { "qualifier": "test", "path": "test.py", "line": 2 },
              "annotation": "None"
            }
          ],
          "attributes": [],
          "abstract": false
        }
      |}


let test_inferred_attributes context =
  let check_inference_results = check_inference_results ~context in
  check_inference_results
    {|
      def foo() -> int:
        return 1
      class Foo:
        x = foo()
    |}
    ~target:"test.Foo.$class_toplevel"
    ~expected:
      {|
        {
          "globals": [],
          "attributes": [
            {
              "parent": "Foo",
              "name": "x",
              "location": { "qualifier": "test", "path": "test.py", "line": 5 },
              "annotation": "int"
            }
          ],
          "abstract": false
        }
      |};
  check_inference_results
    {|
      class Foo:
        x = 1 + 1
    |}
    ~target:"test.Foo.$class_toplevel"
    ~expected:
      {|
        {
          "globals": [],
          "attributes": [
            {
              "parent": "Foo",
              "name": "x",
              "location": { "qualifier": "test", "path": "test.py", "line": 3 },
              "annotation": "int"
            }
          ],
          "abstract": false
        }
      |};
  check_inference_results
    {|
      class Foo:
        def __init__(self) -> None:
          self.x = 1 + 1
    |}
    ~target:"test.Foo.__init__"
    ~expected:
      {|
        {
          "globals": [],
          "attributes": [
            {
              "parent": "Foo",
              "name": "x",
              "location": { "qualifier": "test", "path": "test.py", "line": 4 },
              "annotation": "int"
            }
          ],
          "abstract": false
        }
      |};
  check_inference_results
    {|
      class Foo:
        def __init__(self) -> None:
          self.x = self.foo()

        def foo(self) -> int:
          return 1
    |}
    ~target:"test.Foo.__init__"
    ~expected:
      {|
        {
          "globals": [],
          "attributes": [
            {
              "parent": "Foo",
              "name": "x",
              "location": { "qualifier": "test", "path": "test.py", "line": 4 },
              "annotation": "int"
            }
          ],
          "abstract": false
        }
      |};
  (* TODO(T84365830): Be more intelligent about inferring None type. *)
  check_inference_results
    {|
    class Foo:
      foo = None
    |}
    ~target:"test.Foo.$class_toplevel"
    ~expected:
      {|
        {
          "globals": [],
          "attributes": [
            {
              "parent": "Foo",
              "name": "foo",
              "location": { "qualifier": "test", "path": "test.py", "line": 3 },
              "annotation": "None"
            }
          ],
          "abstract": false
        }
      |};
  ()


let () =
  "typeInferenceLocalTest"
  >::: [
         "test_inferred_returns" >:: test_inferred_returns;
         "test_inferred_parameters" >:: test_inferred_parameters;
         "test_inferred_globals" >:: test_inferred_globals;
         "test_inferred_attributes" >:: test_inferred_attributes;
       ]
  |> Test.run
