(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Server
open Protocol

let test_type_query_json _ =
  let open TypeQuery in
  let assert_serializes response json =
    assert_equal
      ~printer:Yojson.Safe.to_string
      (response_to_yojson response)
      (Yojson.Safe.from_string json)
  in
  assert_serializes (Error "message") {|{"error": "message"}|};
  assert_serializes
    (Response (FoundAttributes [{ name = "name"; annotation = Analysis.Type.integer }]))
    {|{"response": {"attributes": [{"name": "name", "annotation": "int"}]}}|};
  assert_serializes
    (Response
       (FoundMethods
          [ {
              name = "method";
              parameters = [Analysis.Type.integer];
              return_annotation = Analysis.Type.string;
            } ]))
    {|
      {
       "response": {
         "methods": [
           {
             "name": "method",
             "parameters": ["int"],
             "return_annotation": "str"
           }
         ]
       }
      }
    |};
  assert_serializes (Response (Type Analysis.Type.integer)) {|{"response": {"type": "int"}}|};
  assert_serializes
    (Response (Superclasses [Analysis.Type.integer; Analysis.Type.string]))
    {|{"response": {"superclasses": ["int", "str"]}}|}


let () = "serverProtocol" >::: ["type_query_json" >:: test_type_query_json] |> Test.run
