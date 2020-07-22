(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Newserver
open NewServerTest

let test_basic client =
  let open Lwt.Infix in
  (* Test if the `GetInfo` request works properly. *)
  let request = Request.GetInfo in
  RequestHandler.process_request ~state:(Client.current_server_state client) request
  >>= fun (_, expected) -> Client.assert_response client ~request ~expected


let test_basic context =
  ScratchProject.setup ~context ~include_helper_builtins:false ["test.py", "x: int = 42"]
  |> ScratchProject.test_server_with ~f:test_basic


let () = "basic_test" >::: ["basic" >:: OUnitLwt.lwt_wrapper test_basic] |> Test.run
