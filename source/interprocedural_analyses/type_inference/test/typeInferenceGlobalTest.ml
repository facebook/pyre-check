(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Ast
open Test

let environment_data scratch_project =
  let { ScratchProject.BuiltGlobalEnvironment.global_environment; _ } =
    scratch_project |> ScratchProject.build_global_environment
  in
  global_environment, ScratchProject.configuration_of scratch_project


let type_inference_result ~context ~test_source =
  let scratch_project = ScratchProject.setup ~context ["test.py", test_source] in
  let global_environment, configuration = scratch_project |> environment_data in
  let scheduler = Test.mock_scheduler () in
  let global_result =
    Service.Infer.run_infer
      ~configuration
      ~scheduler
      ~filename_lookup:(fun _ -> None)
      ~paths_to_modify:None
      Service.Infer.{ global_environment; qualifiers = [Reference.create "test"] }
  in
  global_result


let assert_json_equal ~context ~expected result =
  let expected = Yojson.Safe.from_string expected in
  assert_equal
    ~ctxt:context
    ~printer:Yojson.Safe.pretty_to_string
    ~msg:"GlobalResult json"
    expected
    result


let assert_global_result ~context ~test_source ~expected () =
  let result = type_inference_result ~context ~test_source in
  result |> TypeInference.Data.GlobalResult.to_yojson |> assert_json_equal ~context ~expected


(* The "*" in the path is because we arent' using a full on v2 builder pattern. This is okay for
   unit tests since we know everything's path would be "test.py"

   The divergent handling of sqlalchemy.Integer in the parameter versus return position is a bug
   we'd like to fix - it is due to the given annotation being an expression whereas the inferred
   annotation is a type. The type is being dequalified. *)
let serialization_test context =
  assert_global_result
    ~context
    ~test_source:
      {|
      import functools
      from sqlalchemy import Integer

      x = 1 + 1

      class C:
          x = 1 + 1

      def no_errors(x: int) -> int:
          return x

      @functools.lru_cache(4)
      def needs_return(y: C, x: Integer):
          return (x, y)
    |}
    ~expected:
      {|
        {
          "globals": [
            {
              "name": "x",
              "location": { "qualifier": "test", "path": "*", "line": 5 },
              "annotation": "int"
            }
          ],
          "attributes": [
            {
              "parent": "test.C",
              "name": "x",
              "location": { "qualifier": "test", "path": "*", "line": 8 },
              "annotation": "int"
            }
          ],
          "defines": [
            {
              "name": "test.needs_return",
              "parent": null,
              "return": "typing.Tuple[sqlalchemy.Integer, test.C]",
              "parameters": [
                { "name": "y", "annotation": null, "value": null, "index": 0 },
                { "name": "x", "annotation": null, "value": null, "index": 1 }
              ],
              "location": { "qualifier": "test", "path": "*", "line": 14 },
              "async": false
            }
          ]
        }
      |}
    ()


let duplicate_define_test context =
  (* This is a made-up example because overloads should have empty bodies, but normal libraries can
     implement similar decorator-based behavior that infer needs to handle. Note that whether we
     filter duplicates does not depend on whether there are duplicate _inference_ results; _any_
     duplicates should trigger us to skip _all_ inference results for a given name. *)
  assert_global_result
    ~context
    ~test_source:
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
  assert_global_result
    ~context
    ~test_source:
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


let suppress_test context =
  assert_global_result
    ~context
    ~test_source:{|
      x = None

      class Foo:
          x = None
      |}
    ~expected:
      {|
        {
          "globals": [],
          "attributes": [],
          "defines": []
        }
      |}
    ()


let attribute_widen_test context =
  assert_global_result
    ~context
    ~test_source:
      {|
      class Foo:
          x = None
          def __init__(self) -> None:
              self.x = 1 + 1
      |}
    ~expected:
      {|

        {
          "globals": [],
          "attributes": [
            {
              "parent": "test.Foo",
              "name": "x",
              "location": { "qualifier": "test", "path": "*", "line": 3 },
              "annotation": "typing.Optional[int]"
            }
          ],
          "defines": []
        }
      |}
    ()


let () =
  "typeInferenceAnalysisTest"
  >::: [
         "serialization" >:: serialization_test;
         "duplicates" >:: duplicate_define_test;
         "suppress" >:: suppress_test;
         "attribute_widen" >:: attribute_widen_test;
       ]
  |> Test.run
