(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Taint
open TestHelper
open Pyre

let test_to_json _ =
  let assert_json ~expected error =
    assert_equal
      ~printer:Yojson.Safe.pretty_to_string
      ~cmp:Yojson.Safe.equal
      (Yojson.Safe.from_string expected)
      (ModelVerificationError.to_json error)
  in
  assert_json
    ~expected:
      {|
        {
          "description": "`foo.bar` is not part of the environment, no module `foo` in search path.",
          "line": 1,
          "column": 2,
          "stop_line": 3,
          "stop_column": 4,
          "path": "/a/b.pysa",
          "code": 6
        }
        |}
    {
      ModelVerificationError.kind =
        ModelVerificationError.NotInEnvironment { module_name = "foo"; name = "foo.bar" };
      location =
        {
          Ast.Location.start = { Ast.Location.line = 1; column = 2 };
          stop = { Ast.Location.line = 3; column = 4 };
        };
      path = Some (PyrePath.create_absolute "/a/b.pysa");
    };
  assert_json
    ~expected:
      {|
        {
          "description": "ModelQuery `get_foo` output no models.",
          "line": 1,
          "column": 2,
          "stop_line": 3,
          "stop_column": 4,
          "path": "/a/b.pysa",
          "code": 41
        }
        |}
    {
      ModelVerificationError.kind = ModelVerificationError.NoOutputFromModelQuery "get_foo";
      location =
        {
          Ast.Location.start = { Ast.Location.line = 1; column = 2 };
          stop = { Ast.Location.line = 3; column = 4 };
        };
      path = Some (PyrePath.create_absolute "/a/b.pysa");
    }


let test_invalid_model_query context =
  let configuration = TaintConfiguration.default in
  let error_message =
    try
      let _ =
        initialize
          ~models_source:
            {|
        ModelQuery(
          name = "invalid_model_query",
          find = "functions",
          where = Decorator(arguments.contains("1"), name.matches("d")),
          model = Returns(TaintSource[Test])
        )
      |}
          ~context
          ~taint_configuration:configuration
          {|
      def foo(x):
          ...
      |}
      in
      "no failure"
    with
    | ModelVerificationError.ModelVerificationErrors errors ->
        List.hd errors >>| ModelVerificationError.display |> Option.value ~default:"no failure"
  in
  assert_equal ~printer:ident "ModelQuery `invalid_model_query` output no models." error_message


let () =
  "model_verification_error"
  >::: ["to_json" >:: test_to_json; "invalid_model_query" >:: test_invalid_model_query]
  |> Test.run
