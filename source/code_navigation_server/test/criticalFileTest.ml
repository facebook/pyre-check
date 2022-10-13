(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open OUnit2
module Request = CodeNavigationServer.Testing.Request

(* NOTE(grievejia): Moving this test to another file that contains another server stop-related test
   seems to cause spurious test failures. I'm guessing this has something to do with how OUnit tests
   runs. All tests in the same file are run in the same process. And since we rely on SIGINT for
   server termination, perhaps the main Lwt event loop is unable to cleanly handle the re-entrance
   of it.

   Putting the test in a fresh file is my current workaround. *)
let test_critical_file_update_request context =
  let project =
    ScratchProject.setup
      ~context
      ~include_typeshed_stubs:false
      ~critical_files:[Server.CriticalFile.Extension "derp"]
      ["test.derp", ""]
  in
  let root = ScratchProject.source_root_of project in
  let test_path = PyrePath.create_relative ~root ~relative:"test.derp" in
  let test_critical_file_update client =
    let%lwt _ =
      ScratchProject.ClientConnection.send_request
        client
        Request.(
          Command
            (Command.FileUpdate
               [
                 FileUpdateEvent.{ kind = Kind.CreatedOrChanged; path = PyrePath.absolute test_path };
               ]))
    in
    (* Consuming the update request would make the server terminate itself immeidately, bypassing
       this eternal-wait. *)
    let wait_forever, _ = Lwt.wait () in
    wait_forever
  in
  ScratchProject.test_server_with_one_connection project ~f:test_critical_file_update


let () =
  "critical_file_test"
  >::: ["critical_file_update_request" >:: OUnitLwt.lwt_wrapper test_critical_file_update_request]
  |> Test.run
