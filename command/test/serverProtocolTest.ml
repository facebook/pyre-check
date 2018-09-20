(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Server
open Protocol
open Request
open Test


let test_flatten _ =
  let mock name = File.create (mock_path name) in
  let requests =
    [
      TypeCheckRequest
        (TypeCheckRequest.create ~update_environment_with:[mock "a.py"] ~check:[mock "a.py"] ());
      TypeCheckRequest
        (TypeCheckRequest.create
           ~update_environment_with:[mock "a.py"; mock "b.py"]
           ~check:[mock "a.py"; mock "b.py"]
           ());
      TypeCheckRequest
        (TypeCheckRequest.create ~check:[mock "b.py"; mock "c.py"] ());
      RageRequest 1234;
      LanguageServerProtocolRequest "{}";
    ]
  in
  match flatten requests with
  | [
    TypeCheckRequest { TypeCheckRequest.update_environment_with; check };
    RageRequest 1234;
    LanguageServerProtocolRequest "{}";
  ] ->
      let get_path path =
        File.path path
        |> PyrePath.relative
        |> Option.value ~default:"NONE"
      in
      assert_equal
        (List.map ~f:get_path update_environment_with |> List.sort ~compare:String.compare)
        ["a.py"; "b.py"];
      assert_equal
        (List.map ~f:get_path check |> List.sort ~compare:String.compare)
        ["a.py"; "b.py"; "c.py"]
  | _ ->
      assert_unreached ()


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
    (Response
       (FoundAttributes
          [{ name = "name"; annotation = Analysis.Type.integer }]))
    {|{"response": {"attributes": [{"name": "name", "annotation": "int"}]}}|};
  assert_serializes
    (Response
       (FoundMethods
          [{
            name = "method";
            parameters = [Analysis.Type.integer];
            return_annotation = Analysis.Type.string;
          }]))
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
  assert_serializes
    (Response (Type Analysis.Type.integer))
    {|{"response": {"type": "int"}}|};
  assert_serializes
    (Response (Superclasses [Analysis.Type.integer; Analysis.Type.string]))
    {|{"response": {"superclasses": ["int", "str"]}}|}


let () =
  "serverProtocol">:::[
    "flatten">::test_flatten;
    "type_query_json">::test_type_query_json;
  ]
  |> Test.run
