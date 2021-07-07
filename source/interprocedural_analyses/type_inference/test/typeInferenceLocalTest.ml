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


  let set_up_project ~context code =
    let ({ ScratchProject.configuration; _ } as project) =
      ScratchProject.setup ~context ["test.py", code] ~infer:true
    in
    let environment =
      setup_environment project |> TypeEnvironment.create |> TypeEnvironment.read_only
    in
    environment, configuration


  let run_inference ~context ~target code =
    let environment, configuration = set_up_project ~context code in
    let global_resolution = environment |> TypeEnvironment.ReadOnly.global_resolution in
    let source =
      let ast_environment = GlobalResolution.ast_environment global_resolution in
      AstEnvironment.ReadOnly.get_processed_source ast_environment (Reference.create "test")
      |> fun option -> Option.value_exn option
    in
    let module_results =
      TypeInference.Local.infer_for_module ~configuration ~global_resolution ~source
    in
    let is_target { LocalResult.define = { name; _ }; _ } =
      Reference.equal name (Reference.create target)
    in
    module_results |> List.filter ~f:is_target |> List.hd_exn
end

let assert_json_equal ~context ~expected result =
  let expected = Yojson.Safe.from_string expected in
  assert_equal ~ctxt:context ~printer:Yojson.Safe.pretty_to_string expected result


let check_inference_results ~context ~target ~expected code =
  let result = Setup.run_inference ~context ~target code |> LocalResult.to_yojson in
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


let test_inferred_function_parameters context =
  let check_inference_results = check_inference_results ~context in
  check_inference_results
    {|
      def foo(x = 5) -> int:
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
              { "name": "x", "annotation": "int", "value": "5", "index": 0 }
            ],
            "decorators": [],
            "location": { "qualifier": "test", "path": "test.py", "line": 2 },
            "async": false
          },
          "abstract": false
        }
      |};
  check_inference_results
    {|
      def foo(x: typing.Any) -> None:
          x = 5
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
          "return": "None",
          "parameters": [
            { "name": "x", "annotation": "int", "value": null, "index": 0 }
          ],
          "decorators": [],
          "location": { "qualifier": "test", "path": "test.py", "line": 2 },
          "async": false
        },
        "abstract": false
      }
     |};
  check_inference_results
    {|
      def foo(x: typing.Any = 5) -> None:
          pass
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
          "return": "None",
          "parameters": [
            { "name": "x", "annotation": "int", "value": "5", "index": 0 }
          ],
          "decorators": [],
          "location": { "qualifier": "test", "path": "test.py", "line": 2 },
          "async": false
        },
        "abstract": false
      }
     |};
  ()


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
         "test_inferred_function_parameters" >:: test_inferred_function_parameters;
         "test_inferred_globals" >:: test_inferred_globals;
         "test_inferred_attributes" >:: test_inferred_attributes;
       ]
  |> Test.run
