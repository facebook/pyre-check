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


let () =
  "serverProtocol">:::[
    "flatten">::test_flatten
  ]
  |> run_test_tt_main
