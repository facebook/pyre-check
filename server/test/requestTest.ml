(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Pyre
open Server
open Test


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


let test_handle_type_query_request _ =
  let assert_response request expected_response =
    let state = mock_server_state () in
    let actual_response =
      let local_root = Path.current_working_directory () in
      Request.handle_type_query_request ~state ~local_root ~request
      |> function
      | Protocol.TypeQueryResponse response ->
          Protocol.TypeQuery.response_to_yojson response
          |> Yojson.Safe.to_string
      | _ ->
          failwith "Unexpected response."
    in
    let expected_response =
      expected_response
      |> Yojson.Safe.from_string
      |> Yojson.Safe.to_string
    in
    assert_equal ~cmp:String.equal ~printer:Fn.id expected_response actual_response
  in

  assert_response
    (Protocol.TypeQuery.Join ((parse_single_expression "int"), (parse_single_expression "float")))
    {|{"response":{"type":"float"}}|};
  assert_response
    (Protocol.TypeQuery.NormalizeType (parse_single_expression "yerp"))
    {|{"error":"Type `yerp` was not found in the type order."}|}


let test_handle_display_type_errors_request _ =
  let assert_response ~paths ~errors ~expected_errors =
    let error_map errors =
      let entry (path, errors) =
        let errors =
          let error identifier =
            let location = { Location.Instantiated.any with Location.path } in
            {
              Analysis.Error.location;
              kind = Analysis.Error.UndefinedName (Expression.Access.create identifier);
              define = +empty_define;
            }
          in
          List.map errors ~f:error
        in
        File.Handle.create path, errors
      in
      List.map errors ~f:entry
    in
    let state = mock_server_state ~errors:(error_map errors |> File.Handle.Table.of_alist_exn) () in
    let actual_errors =
      let local_root = Path.current_working_directory () in
      let files = List.map paths ~f:(fun path -> mock_path path |> File.create) in
      Request.handle_display_type_errors_request ~state ~local_root ~files
      |> snd
      |> function
      | Some (Protocol.TypeCheckResponse response) -> response
      | _ -> failwith "Unexpected response."
    in
    let expected_errors = error_map expected_errors in
    let equal =
      let equal left right =
        File.Handle.equal (fst left) (fst right) &&
        List.equal (snd left) (snd right) ~equal:Analysis.Error.equal
      in
      List.equal ~equal
    in
    let printer errors =
      let show (handle, errors) =
        let errors =
          List.map errors ~f:Analysis.Error.show
          |> String.concat ~sep:", "
        in
        Format.asprintf "%a: [%s]" File.Handle.pp handle errors
      in
      List.map errors ~f:show
      |> String.concat ~sep:"\n"
    in
    assert_equal ~cmp:equal ~printer expected_errors actual_errors
  in

  assert_response ~paths:[] ~errors:[] ~expected_errors:[];
  (* Empty request returns all errors. *)
  assert_response
    ~paths:[]
    ~errors:["one.py", ["one"]; "two.py", ["two"]]
    ~expected_errors:["one.py", ["one"]; "two.py", ["two"]];
  assert_response
    ~paths:["one.py"]
    ~errors:["one.py", ["one"]; "two.py", ["two"]]
    ~expected_errors:["one.py", ["one"]; "two.py", []]


let () =
  Log.initialize_for_tests ();
  "request">:::
  [
    "test_handle_client_shutdown_request">::test_handle_client_shutdown_request;
    "test_handle_type_query_request">::test_handle_type_query_request;
    "test_handle_display_type_errors_request">::test_handle_display_type_errors_request;
  ]
  |> run_test_tt_main
