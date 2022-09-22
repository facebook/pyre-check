(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Lwt.Infix
open Server
open ServerTest

let test_basic client =
  let server_properties = Client.get_server_properties client in
  let test_path, test2_path =
    let { ServerProperties.configuration = { Configuration.Analysis.project_root = root; _ }; _ } =
      server_properties
    in
    ( PyrePath.create_relative ~root ~relative:"test.py",
      PyrePath.create_relative ~root ~relative:"test2.py" )
  in
  (* Test if the `GetInfo` request works properly. *)
  let expected = RequestHandler.create_info_response server_properties in
  Client.send_raw_request client "[\"GetInfo\"]"
  >>= fun actual ->
  Client.assert_response_equal client ~expected ~actual;

  (* Test if we can get the initial type errors. *)
  let error_in_test =
    Analysis.AnalysisError.Instantiated.of_yojson
      (`Assoc
        [
          "line", `Int 3;
          "column", `Int 2;
          "stop_line", `Int 3;
          "stop_column", `Int 14;
          "path", `String (PyrePath.absolute test_path);
          "code", `Int 7;
          "name", `String "Incompatible return type";
          "description", `String "Incompatible return type [7]: Expected `str` but got `int`.";
          ( "long_description",
            `String
              "Incompatible return type [7]: Expected `str` but got `int`.\n\
               Type `str` expected on line 3, specified on line 2." );
          ( "concise_description",
            `String "Incompatible return type [7]: Expected `str` but got `int`." );
          "define", `String "test.foo";
        ])
    |> Result.ok_or_failwith
  in
  let error_in_test2 =
    Analysis.AnalysisError.Instantiated.of_yojson
      (`Assoc
        [
          "line", `Int 2;
          "column", `Int 0;
          "stop_line", `Int 2;
          "stop_column", `Int 3;
          "path", `String (PyrePath.absolute test2_path);
          "code", `Int 9;
          "name", `String "Incompatible variable type";
          ( "description",
            `String
              "Incompatible variable type [9]: bar is declared to have type `str` but is used as \
               type `int`." );
          ( "long_description",
            `String
              "Incompatible variable type [9]: bar is declared to have type `str` but is used as \
               type `int`." );
          ( "concise_description",
            `String "Incompatible variable type [9]: bar has type `str`; used as `int`." );
          "define", `String "test2.$toplevel";
        ])
    |> Result.ok_or_failwith
  in
  (* Query all type errors. *)
  Client.assert_response
    client
    ~request:(Request.DisplayTypeError [])
    ~expected:(Response.TypeErrors [error_in_test; error_in_test2])
  >>= fun () ->
  (* Query type errors for `test.py`. *)
  Client.assert_response
    client
    ~request:(Request.DisplayTypeError [PyrePath.absolute test_path])
    ~expected:(Response.TypeErrors [error_in_test])
  >>= fun () ->
  (* Sending `IncrementalUpdate` without the corresponding filesystem should have no impact on the
     type errors. *)
  Client.assert_response
    client
    ~request:(Request.IncrementalUpdate [PyrePath.absolute test_path])
    ~expected:Response.Ok
  >>= fun () ->
  Client.assert_response
    client
    ~request:(Request.DisplayTypeError [])
    ~expected:(Response.TypeErrors [error_in_test; error_in_test2])
  >>= fun () ->
  (* Actually test incrementally changes on `test.py`. *)
  let new_test_content =
    Test.trim_extra_indentation {|
       def foo(x: int) -> int:
         return x
    |}
  in
  File.create ~content:new_test_content test_path |> File.write;
  Client.assert_response
    client
    ~request:(Request.IncrementalUpdate [PyrePath.absolute test_path])
    ~expected:Response.Ok
  >>= fun () ->
  Client.assert_response
    client
    ~request:(Request.DisplayTypeError [])
    ~expected:(Response.TypeErrors [error_in_test2])


let test_basic context =
  ScratchProject.setup
    ~context
    ~include_helper_builtins:false
    [
      "test.py", {|
          def foo(x: int) -> str:
            return x + 1
        |};
      "test2.py", {|
          bar: str = 42
        |};
    ]
  |> ScratchProject.test_server_with ~f:test_basic


let test_subscription context =
  let input_channel, output_channel = Lwt_io.pipe () in
  let subscription =
    Subscription.create
      ~subscription_request:(Subscription.Request.SubscribeToTypeErrors "foo")
      ~output_channel
      ()
  in
  assert_equal
    ~ctxt:context
    ~cmp:String.equal
    ~printer:Fn.id
    "foo"
    (Subscription.name_of subscription);

  Subscription.send ~response:Response.Ok subscription
  >>= fun () ->
  Lwt_io.read_line input_channel
  >>= fun actual_response ->
  let expected_response =
    Subscription.Response.to_yojson { name = "foo"; body = Response.Ok } |> Yojson.Safe.to_string
  in
  assert_equal ~ctxt:context ~cmp:String.equal ~printer:Fn.id expected_response actual_response;
  Lwt.return_unit


exception FakeLwtIOException

let test_subscription_output_channel_error_dropped _ =
  (* Fake an output channel that always raise exception. *)
  let output_channel = Lwt_io.make ~mode:Lwt_io.Output (fun _ _ _ -> raise FakeLwtIOException) in
  let subscription =
    Subscription.create
      ~subscription_request:(Subscription.Request.SubscribeToTypeErrors "foo")
      ~output_channel
      ()
  in
  (* This invocation should raise no error, even if sending any data to the output channel would
     fail. *)
  Subscription.send ~response:Response.Ok subscription


let watchman_version = "fake_watchman_version"

let watchman_initial_clock = "fake:clock:0"

let watchman_update_response ~root file_names =
  `Assoc
    [
      "is_fresh_instance", `Bool false;
      "files", `List (List.map file_names ~f:(fun name -> `String name));
      "root", `String root;
      "version", `String watchman_version;
      "clock", `String "fake:clock:update";
      "since", `String watchman_initial_clock;
    ]


let test_watchman_integration ~watchman_mailbox client =
  (* Test if we can get the initial type errors. *)
  let global_root =
    Client.get_server_properties client
    |> fun { ServerProperties.configuration = { Configuration.Analysis.project_root; _ }; _ } ->
    project_root
  in
  let test_path = PyrePath.create_relative ~root:global_root ~relative:"test.py" in
  let initial_error =
    Analysis.AnalysisError.Instantiated.of_yojson
      (`Assoc
        [
          "line", `Int 3;
          "column", `Int 2;
          "stop_line", `Int 3;
          "stop_column", `Int 14;
          "path", `String (PyrePath.absolute test_path);
          "code", `Int 7;
          "name", `String "Incompatible return type";
          "description", `String "Incompatible return type [7]: Expected `str` but got `int`.";
          ( "long_description",
            `String
              "Incompatible return type [7]: Expected `str` but got `int`.\n\
               Type `str` expected on line 3, specified on line 2." );
          ( "concise_description",
            `String "Incompatible return type [7]: Expected `str` but got `int`." );
          "define", `String "test.foo";
        ])
    |> Result.ok_or_failwith
  in
  Client.assert_response
    client
    ~request:(Request.DisplayTypeError [])
    ~expected:(Response.TypeErrors [initial_error])
  >>= fun () ->
  (* Update an existing file and send a watchman response. *)
  let new_test_content =
    Test.trim_extra_indentation {|
       def foo(x: int) -> int:
         return x + 1
    |}
  in
  File.create ~content:new_test_content test_path |> File.write;
  Lwt_mvar.put
    watchman_mailbox
    (watchman_update_response ~root:(PyrePath.absolute global_root) ["test.py"])
  >>= fun () ->
  (* Test if the server correctly update the type errors. *)
  Client.assert_response
    client
    ~request:(Request.DisplayTypeError [])
    ~expected:(Response.TypeErrors [])
  >>= fun () ->
  (* Add a new file and send a watchman response. *)
  let test2_path = PyrePath.create_relative ~root:global_root ~relative:"test2.py" in
  let test2_content = "bar: str = 42" in
  File.create ~content:test2_content test2_path |> File.write;
  Lwt_mvar.put
    watchman_mailbox
    (watchman_update_response ~root:(PyrePath.absolute global_root) ["test2.py"])
  >>= fun () ->
  (* Test if the server correctly update the type errors. *)
  let new_error =
    Analysis.AnalysisError.Instantiated.of_yojson
      (`Assoc
        [
          "line", `Int 1;
          "column", `Int 0;
          "stop_line", `Int 1;
          "stop_column", `Int 3;
          "path", `String (PyrePath.absolute test2_path);
          "code", `Int 9;
          "name", `String "Incompatible variable type";
          ( "description",
            `String
              "Incompatible variable type [9]: bar is declared to have type `str` but is used as \
               type `int`." );
          ( "long_description",
            `String
              "Incompatible variable type [9]: bar is declared to have type `str` but is used as \
               type `int`." );
          ( "concise_description",
            `String "Incompatible variable type [9]: bar has type `str`; used as `int`." );
          "define", `String "test2.$toplevel";
        ])
    |> Result.ok_or_failwith
  in
  Client.assert_response
    client
    ~request:(Request.DisplayTypeError [])
    ~expected:(Response.TypeErrors [new_error])
  >>= fun () ->
  (* Remove a file and send a watchman response. *)
  PyrePath.remove test2_path;
  Lwt_mvar.put
    watchman_mailbox
    (watchman_update_response ~root:(PyrePath.absolute global_root) ["test2.py"])
  >>= fun () ->
  (* Test if the server correctly update the type errors. *)
  Client.assert_response
    client
    ~request:(Request.DisplayTypeError [])
    ~expected:(Response.TypeErrors [])


let test_watchman_integration context =
  (* We use a mailbox variable in this test to get a more precise control of when a given watchman
     response can reach the Pyre serer. *)
  let watchman_mailbox = Lwt_mvar.create_empty () in
  let mock_watchman =
    let initialize_stage = ref 0 in
    let send _ = Lwt.return_unit in
    let receive () =
      match !initialize_stage with
      | 0 ->
          let watchman_watch_response =
            `Assoc ["version", `String watchman_version; "watcher", `String "fake_watcher"]
          in
          initialize_stage := 1;
          Lwt.return_some watchman_watch_response
      | 1 ->
          let watchman_subscribe_response =
            `Assoc ["version", `String watchman_version; "clock", `String watchman_initial_clock]
          in
          initialize_stage := 2;
          Lwt.return_some watchman_subscribe_response
      | _ -> Lwt_mvar.take watchman_mailbox >>= Lwt.return_some
    in
    Watchman.Raw.create_for_testing ~send ~receive ()
  in
  ScratchProject.setup
    ~context
    ~include_helper_builtins:false
    ~watchman:mock_watchman
    ["test.py", {|
          def foo(x: int) -> str:
            return x + 1
        |}]
  |> ScratchProject.test_server_with ~f:(test_watchman_integration ~watchman_mailbox)


let test_watchman_failure context =
  let mock_watchman =
    let send _ = Lwt.return_unit in
    let receive () = failwith "Intentional watchman failure" in
    Watchman.Raw.create_for_testing ~send ~receive ()
  in
  ScratchProject.setup
    ~context
    ~include_typeshed_stubs:false
    ~include_helper_builtins:false
    ~watchman:mock_watchman
    []
  |> ScratchProject.test_server_with ~expect_server_error:true ~f:(fun _ -> Lwt.return_unit)


let test_on_server_socket_ready context =
  (* Test `on_server_socket_ready` gets correctly invoked before `on_start`. *)
  let established_flag = ref false in
  ScratchProject.setup ~context ~include_typeshed_stubs:false ~include_helper_builtins:false []
  |> ScratchProject.test_server_with
       ~on_server_socket_ready:(fun _ ->
         established_flag := true;
         Lwt.return_unit)
       ~f:(fun _ ->
         assert_bool "Established flag should have been set to `true`" !established_flag;
         Lwt.return_unit)


let test_subscription_responses_with_type_errors client =
  let {
    ServerProperties.socket_path;
    configuration = { Configuration.Analysis.project_root; _ };
    _;
  }
    =
    Client.get_server_properties client
  in
  let { ServerState.subscriptions; _ } = Client.current_server_state client in
  let test_path = PyrePath.create_relative ~root:project_root ~relative:"test.py" in
  let error =
    Analysis.AnalysisError.Instantiated.of_yojson
      (`Assoc
        [
          "line", `Int 3;
          "column", `Int 2;
          "stop_line", `Int 3;
          "stop_column", `Int 14;
          "path", `String (PyrePath.absolute test_path);
          "code", `Int 7;
          "name", `String "Incompatible return type";
          "description", `String "Incompatible return type [7]: Expected `str` but got `int`.";
          ( "long_description",
            `String
              "Incompatible return type [7]: Expected `str` but got `int`.\n\
               Type `str` expected on line 3, specified on line 2." );
          ( "concise_description",
            `String "Incompatible return type [7]: Expected `str` but got `int`." );
          "define", `String "test.foo";
        ])
    |> Result.ok_or_failwith
  in
  (* Create two subscriptions: one that listens for type errors, one that does not *)
  Client.subscribe
    client
    ~subscription:(Subscription.Request.SubscribeToTypeErrors "foo")
    ~expected_response:(Response.TypeErrors [error])
  >>= fun () ->
  (* Verifies that we've managed to record the subscriptions in the server state. *)
  assert_bool
    "Subscription `foo` recorded"
    (ServerState.Subscriptions.get subscriptions ~name:"foo" |> Option.is_some);
  (* Open another connection to the started server and send an incremental update message -- we
     can't reuse the connection from `client` for this update message since that connection has
     already been used to receive subscriptions. *)
  let socket_address = Lwt_unix.ADDR_UNIX (PyrePath.absolute socket_path) in
  let send_incremental_update (_, output_channel) =
    Request.IncrementalUpdate [PyrePath.absolute test_path]
    |> Request.to_yojson
    |> Yojson.Safe.to_string
    |> Lwt_io.write_line output_channel
  in
  Lwt_io.with_connection socket_address send_incremental_update
  (* We should get rebuild and recheck notifications *)
  >>= fun () ->
  Client.assert_subscription_response
    client
    ~expected:
      { Subscription.Response.name = "foo"; body = Response.(StatusUpdate ServerStatus.Rebuilding) }
  >>= fun () ->
  Client.assert_subscription_response
    client
    ~expected:
      { Subscription.Response.name = "foo"; body = Response.(StatusUpdate ServerStatus.Rechecking) }
  (* Only the type errors subscription should get type errors *)
  >>= fun () ->
  Client.assert_subscription_response
    client
    ~expected:{ Subscription.Response.name = "foo"; body = Response.TypeErrors [error] }


let test_subscription_responses_no_type_errors client =
  let {
    ServerProperties.socket_path;
    configuration = { Configuration.Analysis.project_root; _ };
    _;
  }
    =
    Client.get_server_properties client
  in
  let { ServerState.subscriptions; _ } = Client.current_server_state client in
  let test_path = PyrePath.create_relative ~root:project_root ~relative:"test.py" in
  (* Create two subscriptions: one that listens for type errors, one that does not *)
  Client.subscribe
    client
    ~subscription:(Subscription.Request.SubscribeToStateChanges "foo")
    ~expected_response:Response.Ok
  >>= fun () ->
  (* Verifies that we've managed to record the subscriptions in the server state. *)
  assert_bool
    "Subscription `foo` recorded"
    (ServerState.Subscriptions.get subscriptions ~name:"foo" |> Option.is_some);
  (* Open another connection to the started server and send an incremental update message -- we
     can't reuse the connection from `client` for this update message since that connection has
     already been used to receive subscriptions. *)
  let socket_address = Lwt_unix.ADDR_UNIX (PyrePath.absolute socket_path) in
  let send_incremental_update (_, output_channel) =
    Request.IncrementalUpdate [PyrePath.absolute test_path]
    |> Request.to_yojson
    |> Yojson.Safe.to_string
    |> Lwt_io.write_line output_channel
  in
  Lwt_io.with_connection socket_address send_incremental_update
  (* We should get rebuild and recheck notifications *)
  >>= fun () ->
  Client.assert_subscription_response
    client
    ~expected:
      { Subscription.Response.name = "foo"; body = Response.(StatusUpdate ServerStatus.Rebuilding) }
  >>= fun () ->
  Client.assert_subscription_response
    client
    ~expected:
      { Subscription.Response.name = "foo"; body = Response.(StatusUpdate ServerStatus.Rechecking) }
  (* Send a second update (this is the easiest way to verify that there is no type errors response *)
  >>= fun () ->
  Lwt_io.with_connection socket_address send_incremental_update
  >>= fun () ->
  Client.assert_subscription_response
    client
    ~expected:
      { Subscription.Response.name = "foo"; body = Response.(StatusUpdate ServerStatus.Rebuilding) }
  >>= fun () ->
  Client.assert_subscription_response
    client
    ~expected:
      { Subscription.Response.name = "foo"; body = Response.(StatusUpdate ServerStatus.Rechecking) }


let test_subscription_responses context =
  let sources =
    ["test.py", {|
          def foo(x: int) -> str:
            return x + 1
        |}]
  in
  ScratchProject.setup ~context ~include_helper_builtins:false sources
  |> ScratchProject.test_server_with ~f:test_subscription_responses_with_type_errors
  >>= fun () ->
  ScratchProject.setup ~context ~include_helper_builtins:false sources
  |> ScratchProject.test_server_with ~f:test_subscription_responses_no_type_errors


let () =
  "basic_test"
  >::: [
         "basic" >:: OUnitLwt.lwt_wrapper test_basic;
         "subscription" >:: OUnitLwt.lwt_wrapper test_subscription;
         "subscription_output_channel_error_dropped"
         >:: OUnitLwt.lwt_wrapper test_subscription_output_channel_error_dropped;
         "subscription_response" >:: OUnitLwt.lwt_wrapper test_subscription_responses;
         "watchman_integration" >:: OUnitLwt.lwt_wrapper test_watchman_integration;
         "watchman_failure" >:: OUnitLwt.lwt_wrapper test_watchman_failure;
         "on_server_socket_ready" >:: OUnitLwt.lwt_wrapper test_on_server_socket_ready;
       ]
  |> Test.run
