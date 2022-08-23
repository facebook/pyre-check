(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
module Request = CodeNavigationServer.Testing.Request

let test_no_op_server context =
  ScratchProject.setup ~context ~include_typeshed_stubs:false []
  |> ScratchProject.test_server_with ~f:(fun _ -> Lwt.return_unit)


let test_invalid_request context =
  let test_invalid_request client =
    let%lwt raw_response = ScratchProject.Client.send_raw_request client "derp" in
    match Yojson.Safe.from_string raw_response with
    | `List (`String "Error" :: _) -> Lwt.return_unit
    | _ as json ->
        let message =
          Format.sprintf "Expected error response but got: `%s`" (Yojson.Safe.to_string json)
        in
        assert_failure message
  in
  ScratchProject.setup ~context ~include_typeshed_stubs:false []
  |> ScratchProject.test_server_with ~f:test_invalid_request


let test_stop_request context =
  let test_stop_request client =
    let%lwt _ = ScratchProject.Client.send_request client Request.Stop in
    (* Consuming the stop request would make the server terminate itself immeidately, bypassing this
       eternal-wait. *)
    let wait_forever, _ = Lwt.wait () in
    wait_forever
  in
  ScratchProject.setup ~context ~include_typeshed_stubs:false []
  |> ScratchProject.test_server_with ~f:test_stop_request


let () =
  "basic_test"
  >::: [
         "no_op_server" >:: OUnitLwt.lwt_wrapper test_no_op_server;
         "invalid_request" >:: OUnitLwt.lwt_wrapper test_invalid_request;
         "stop_request" >:: OUnitLwt.lwt_wrapper test_stop_request;
       ]
  |> Test.run
