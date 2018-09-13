(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open OUnit2
open Core

open Pyre
open Server


let test_saved_state context =
  let open Server.Protocol in
  (* Set up a directory for the server to run in. *)
  let local_root =
    bracket_tmpdir context
    |> Path.create_absolute
  in
  let write_content ~filename content =
    Out_channel.write_all
      (Path.create_relative ~root:local_root ~relative:filename
       |> Path.absolute)
      ~data:(Test.trim_extra_indentation content)
  in
  write_content
    ~filename:"a.py"
    {|
      class C:
        pass
      x = C()
    |};

  let configuration = Configuration.create ~local_root () in
  let saved_state_path =
    Path.create_relative ~root:local_root ~relative:"saved_state"
    |> Path.absolute
  in

  (* Spawn a server that saves its state on initialization. *)
  let server_configuration =
    ServerConfiguration.create
      ~save_state_to:saved_state_path
      configuration
  in
  let _ = Commands.Server.start server_configuration in

  (* Wait until the server initializes before stopping. *)
  let socket = Operations.connect ~retries:3 ~configuration in
  Network.Socket.write socket (Request.FlushTypeErrorsRequest);
  let _ = Network.Socket.read socket in
  CommandTest.stop_server server_configuration;

  (* A saved state was created. *)
  assert_equal `Yes (Sys.file_exists saved_state_path);

  (* No server is running. *)
  assert_raises
    Operations.ConnectionFailure
    (fun () -> Operations.connect ~retries:1 ~configuration);

  (* A server loads from the saved state successfully. *)
  let server_configuration =
    ServerConfiguration.create ~load_state_from:saved_state_path configuration
  in
  (* Query the new server for environment information. *)
  let _ = Commands.Server.start server_configuration in

  let socket = Operations.connect ~retries:3 ~configuration in
  Network.Socket.write socket (Commands.Query.parse_query ~root:local_root "type(a.x)");
  let response = Network.Socket.read socket in

  CommandTest.stop_server server_configuration;

  let expected_response =
      TypeQueryResponse
        (TypeQuery.Response
           (TypeQuery.Type (Analysis.Type.primitive "a.C")))
  in
  (* The server loaded from a saved state has the information we expect. *)
  assert_equal expected_response response

let () =
  CommandTest.run_command_tests
    "saved_state"
    [
      "saved_state", test_saved_state;
    ]
