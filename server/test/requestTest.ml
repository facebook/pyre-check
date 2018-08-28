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
        typeshed_stubs;
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


let assert_errors_equal ~actual_errors ~expected_errors =
  let actual_errors =
    let errors (handle, errors) =
      File.Handle.show handle,
      List.map errors ~f:(Analysis.Error.description ~detailed:false)
    in
    List.map actual_errors ~f:errors
  in
  let equal =
    let equal left right =
      String.equal (fst left) (fst right) &&
      List.equal
        (snd left |> List.sort ~compare:String.compare)
        (snd right |> List.sort ~compare:String.compare)
        ~equal:String.equal
    in
    List.equal ~equal
  in
  let printer errors =
    let show (path, errors) = Format.asprintf "%s: [%s]" path (String.concat errors ~sep:", ") in
    List.map errors ~f:show
    |> String.concat ~sep:"\n"
  in
  assert_equal ~cmp:equal ~printer expected_errors actual_errors


let test_handle_display_type_errors_request _ =
  let assert_response ~paths ~errors ~expected_errors =
    let actual_errors =
      let state =
        let serialized_errors errors =
          let entry (path, errors) =
            let errors =
              let error undefined =
                let location = { Location.Instantiated.any with Location.path } in
                {
                  Analysis.Error.location;
                  kind = Analysis.Error.UndefinedName (Expression.Access.create undefined);
                  define = +empty_define;
                }
              in
              List.map errors ~f:error
            in
            File.Handle.create path, errors
          in
          List.map errors ~f:entry
        in
        mock_server_state
          ~errors:(serialized_errors errors |> File.Handle.Table.of_alist_exn)
          ()
      in
      let local_root = Path.current_working_directory () in
      let files = List.map paths ~f:(fun path -> mock_path path |> File.create) in
      Request.handle_display_type_errors_request ~state ~local_root ~files
      |> snd
      |> function
      | Some (Protocol.TypeCheckResponse response) -> response
      | _ -> failwith "Unexpected response."
    in
    let expected_errors =
      let expected_error (path, undefined_globals) =
        let undefined_global global =
          Format.asprintf
            "Undefined name [18]: Global name `%s` is undefined."
            global
        in
        path,
        List.map undefined_globals ~f:undefined_global
      in
      List.map expected_errors ~f:expected_error
    in
    assert_errors_equal ~actual_errors ~expected_errors
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


let test_handle_type_check_request context =
  let assert_response ~check ~expected_errors =
    let assert_response _ =
      let actual_errors =
        let check =
          let file (path, content) =
            let content = trim_extra_indentation content in
            let file = File.create ~content:(Some content) (mock_path path) in
            File.write file;
            file
          in
          List.map check ~f:file
        in
        let request = { Protocol.TypeCheckRequest.update_environment_with = check; check } in
        let state = mock_server_state () in
        let configuration =
          Configuration.create ~project_root:(Path.current_working_directory ()) ()
        in
        Request.handle_type_check_request ~state ~configuration ~request
        |> snd
        |> function
        | Some (Protocol.TypeCheckResponse response) -> response
        | _ -> failwith "Unexpected response."
      in
      assert_errors_equal ~actual_errors ~expected_errors
    in
    OUnit2.with_bracket_chdir context (OUnit2.bracket_tmpdir context) assert_response
  in

  assert_response ~check:[] ~expected_errors:[];
  assert_response
    ~check:[
      "test.py",
      {|
        def foo() -> int:
          return 'asdf'
      |};
    ]
    ~expected_errors:["test.py", ["Incompatible return type [7]: Expected `int` but got `str`."]]


let () =
  Log.initialize_for_tests ();
  Scheduler.mock () |> ignore;
  "request">:::
  [
    "test_handle_client_shutdown_request">::test_handle_client_shutdown_request;
    "test_handle_type_query_request">::test_handle_type_query_request;
    "test_handle_display_type_errors_request">::test_handle_display_type_errors_request;
    "test_handle_type_check_request">::test_handle_type_check_request;
  ]
  |> run_test_tt_main
