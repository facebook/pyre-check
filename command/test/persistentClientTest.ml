(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Server
open Network
open OUnit2

let test_persistent_client_connect context =
  let local_root = bracket_tmpdir context |> Pyre.Path.create_absolute in
  let set_up _ =
    Format.pp_set_formatter_out_channel Format.err_formatter (Out_channel.create "/dev/null");
    CommandTest.start_server ~local_root () |> ignore;
    Server.Operations.connect
      ~retries:5
      ~configuration:(CommandTest.mock_server_configuration ~local_root ()).configuration
  in
  let tear_down client_socket _ =
    Unix.close client_socket;
    Commands.Stop.stop ~local_root:(Path.absolute local_root) |> ignore
  in
  let client_socket = bracket set_up tear_down context in
  Socket.write client_socket (Protocol.Request.ClientConnectionRequest Protocol.Persistent);
  assert_equal
    ~cmp:( = )
    (Socket.read client_socket)
    (Protocol.ClientConnectionResponse Protocol.Persistent)


let () =
  CommandTest.run_command_tests
    "persistent_client"
    ["persistent_client_connect", test_persistent_client_connect]
