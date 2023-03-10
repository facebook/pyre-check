(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
module Request = CodeNavigationServer.Testing.Request
module Response = CodeNavigationServer.Testing.Response

let position line column = { Ast.Location.line; column }

let range start_line start_column stop_line stop_column =
  { Ast.Location.start = position start_line start_column; stop = position stop_line stop_column }


let register_client ~client_id =
  ScratchProject.ClientConnection.assert_response
    ~request:Request.(Command (Command.RegisterClient { client_id }))
    ~expected:Response.Ok


let dispose_client ~client_id =
  ScratchProject.ClientConnection.assert_response
    ~request:Request.(Command (Command.DisposeClient { client_id }))
    ~expected:Response.Ok


let open_file ?content ~client_id ~path =
  ScratchProject.ClientConnection.assert_response
    ~request:Request.(Command (Command.FileOpened { path; content; client_id }))
    ~expected:Response.Ok


let close_file ~client_id ~path =
  ScratchProject.ClientConnection.assert_response
    ~request:Request.(Command (Command.FileClosed { path; client_id }))
    ~expected:Response.Ok


let assert_type_error_count ~client_id ~path ~expected client =
  let%lwt raw_response =
    ScratchProject.ClientConnection.send_request
      client
      Request.(Query (Query.GetTypeErrors { path; client_id }))
  in
  match Yojson.Safe.from_string raw_response with
  | `List [`String "TypeErrors"; `List errors] ->
      assert_equal
        ~ctxt:(ScratchProject.ClientConnection.get_context client)
        ~cmp:Int.equal
        ~printer:Int.to_string
        expected
        (List.length errors);
      Lwt.return_unit
  | _ as json ->
      let message =
        Format.sprintf "Expected type error response but got: `%s`" (Yojson.Safe.to_string json)
      in
      assert_failure message


let assert_type_error_count_for_path ~client_id ~path ~expected client =
  assert_type_error_count client ~client_id ~expected ~path
