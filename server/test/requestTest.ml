(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Test
open Server


let mock_server_state ?(errors = File.Handle.Table.create ()) () =
  let environment =
    let environment =
      let configuration = Configuration.create () in
      let environment = Analysis.Environment.Builder.create () in
      Service.Environment.populate
        (Analysis.Environment.handler ~configuration environment)
        [];
      environment
    in
    Analysis.Environment.handler ~configuration environment
  in
  add_defaults_to_environment environment;
  {
    State.deferred_requests = [];
    environment;
    errors;
    handles = File.Handle.Set.empty;
    last_request_time = Unix.time ();
    last_integrity_check = Unix.time ();
    lookups = String.Table.create ();
    lock = Mutex.create ();
    connections = ref {
        State.socket = Unix.openfile ~mode:[Unix.O_RDONLY] "/dev/null";
        persistent_clients = Unix.File_descr.Table.create ();
        file_notifiers = [];
        watchman_pid = None;
      };
    scheduler = Scheduler.mock ();
  }


let test_handle_client_shutdown_request _ =
  let assert_response id expected_response =
    let state = mock_server_state () in
    let actual_response =
      Request.handle_client_shutdown_request ~state ~id
      |> snd
      |> function
      | Some (Protocol.LanguageServerProtocolResponse response) -> response
      | _ -> failwith "Unexpected response."
    in
    let expected_response =
      expected_response
      |> Yojson.Safe.from_string
      |> Yojson.Safe.to_string
    in
    assert_equal ~cmp:String.equal ~printer:Fn.id expected_response actual_response
  in
  assert_response 0 {|{"jsonrpc":"2.0","id":0,"result":null}|};
  assert_response 2 {|{"jsonrpc":"2.0","id":2,"result":null}|}


let () =
  Log.initialize_for_tests ();
  "request">:::
  [
    "test_handle_client_shutdown_request">::test_handle_client_shutdown_request;
  ]
  |> run_test_tt_main
