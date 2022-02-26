(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Taint

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
    }


let () = "model_verification_error" >::: ["to_json" >:: test_to_json] |> Test.run
