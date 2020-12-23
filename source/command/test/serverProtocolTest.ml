(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Server
open Test

let test_type_query_json _ =
  let assert_serializes response json =
    assert_equal
      ~printer:Yojson.Safe.to_string
      (Query.Response.to_yojson response)
      (Yojson.Safe.from_string json)
  in
  assert_serializes (Error "message") {|{"error": "message"}|};
  assert_serializes
    (Single
       (FoundAttributes
          [{ name = "name"; annotation = Analysis.Type.integer; kind = Regular; final = true }]))
    {|{"response": {"attributes": [{"name": "name", "annotation": "int", "kind": "regular", "final": true}]}}|};
  assert_serializes
    (Single
       (FoundAttributes
          [{ name = "name"; annotation = Analysis.Type.integer; kind = Property; final = false }]))
    {|{"response": {"attributes": [{"name": "name", "annotation": "int", "kind": "property", "final": false}]}}|};
  assert_serializes (Single (Type Analysis.Type.integer)) {|{"response": {"type": "int"}}|};
  assert_serializes
    (Single
       (Superclasses
          [{ Query.Response.Base.class_name = !&"test.C"; superclasses = [!&"int"; !&"str"] }]))
    {|{"response": [{"test.C": ["int", "str"]}]}|};
  assert_serializes
    (Single
       (Superclasses
          [
            { Query.Response.Base.class_name = !&"test.C"; superclasses = [!&"int"; !&"str"] };
            { Query.Response.Base.class_name = !&"test.D"; superclasses = [!&"int"; !&"bool"] };
          ]))
    {|
      {
        "response": [
          {
            "test.C": ["int","str"]
          },
          {
            "test.D": ["int","bool"]
          }
        ]
      }
    |};
  assert_serializes
    (Batch
       [Single (Type Analysis.Type.integer); Single (Type Analysis.Type.string); Error "message"])
    {|{"response":[{"response":{"type":"int"}},{"response":{"type":"str"}},{"error":"message"}]}|}


let () = "serverProtocol" >::: ["type_query_json" >:: test_type_query_json] |> Test.run
