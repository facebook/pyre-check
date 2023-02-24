(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
module Request = CodeNavigationServer.Testing.Request
module Response = CodeNavigationServer.Testing.Response
module Subscriptions = CodeNavigationServer.Testing.Subscriptions

let assert_int ~context ~expected actual =
  assert_equal ~ctxt:context ~cmp:Int.equal ~printer:Int.to_string expected actual


let assert_string_option ~context ~expected actual =
  assert_equal
    ~ctxt:context
    ~cmp:[%compare.equal: string option]
    ~printer:(fun input -> Sexp.to_string_hum ([%sexp_of: string option] input))
    expected
    actual


let test_subscription_registration context =
  let subscriptions = Subscriptions.create () in
  let input_channel0, output_channel0 = Lwt_io.pipe () in
  let input_channel1, output_channel1 = Lwt_io.pipe () in

  let identifier0 = Subscriptions.register subscriptions ~output_channel:output_channel0 in
  let identifier1 = Subscriptions.register subscriptions ~output_channel:output_channel1 in

  assert_bool
    "Identifiers of different subscriptions should be different"
    (not (Subscriptions.Identifier.equal identifier0 identifier1));
  assert_int ~context ~expected:2 (Subscriptions.count subscriptions);

  let%lwt () = Subscriptions.broadcast_raw ~message:(lazy "foo") subscriptions in
  let%lwt input0 = Lwt_io.read_line_opt input_channel0 in
  assert_string_option ~context ~expected:(Some "foo") input0;
  let%lwt input1 = Lwt_io.read_line_opt input_channel1 in
  assert_string_option ~context ~expected:(Some "foo") input1;

  Subscriptions.unregister subscriptions ~identifier:identifier1;
  assert_int ~context ~expected:1 (Subscriptions.count subscriptions);

  let%lwt () = Subscriptions.broadcast_raw ~message:(lazy "bar") subscriptions in
  (* Close both channels so subsequent readlines won't wait for channel closure. *)
  let%lwt () = Lwt_io.close output_channel0 in
  let%lwt () = Lwt_io.close output_channel1 in
  let%lwt input0 = Lwt_io.read_line_opt input_channel0 in
  assert_string_option ~context ~expected:(Some "bar") input0;
  let%lwt input1 = Lwt_io.read_line_opt input_channel1 in
  assert_string_option ~context ~expected:None input1;

  Subscriptions.unregister subscriptions ~identifier:identifier0;

  (* Test message won't be constructed when collection is empty *)
  let sentinel = ref None in
  let message =
    lazy
      (sentinel := Some "derp";
       "derp")
  in
  let%lwt () = Subscriptions.broadcast_raw subscriptions ~message in
  assert_string_option ~context ~expected:None !sentinel;

  Lwt.return_unit


let test_server_subscription_establish context =
  let f connection =
    let%lwt response =
      ScratchProject.ClientConnection.send_request
        connection
        Request.(Subscription Subscription.Subscribe)
    in
    ScratchProject.ClientConnection.assert_response_equal
      connection
      ~expected:Response.Ok
      ~actual:response;
    Lwt.return_unit
  in
  ScratchProject.setup ~context ~include_typeshed_stubs:false []
  |> ScratchProject.test_server_with_one_connection ~f


let test_server_subscription_busy_file_update context =
  let project = ScratchProject.setup ~context ~include_typeshed_stubs:false ["test.py", ""] in
  let root = ScratchProject.source_root_of project in
  let test_path = PyrePath.create_relative ~root ~relative:"test.py" in

  (* This is used to synchronize between subscriber and mutator *)
  let mailbox = Lwt_mvar.create_empty () in
  let subscriber connection =
    let%lwt _ =
      ScratchProject.ClientConnection.send_request
        connection
        Request.(Subscription Subscription.Subscribe)
    in
    let%lwt () = Lwt_mvar.put mailbox "subscribed" in
    let%lwt () =
      ScratchProject.ClientConnection.assert_subscription_response
        ~expected:Response.(ServerStatus Status.BusyBuilding)
        connection
    in
    let%lwt () =
      ScratchProject.ClientConnection.assert_subscription_response
        ~expected:Response.(ServerStatus (Status.BusyChecking { overlay_id = None }))
        connection
    in
    let%lwt () =
      ScratchProject.ClientConnection.assert_subscription_response
        ~expected:Response.(ServerStatus Status.Idle)
        connection
    in
    Lwt.return_unit
  in
  let mutator connection =
    let%lwt _ = Lwt_mvar.take mailbox in
    let%lwt _ =
      ScratchProject.ClientConnection.send_request
        connection
        Request.(
          Command
            (Command.FileUpdate
               [
                 FileUpdateEvent.{ kind = Kind.CreatedOrChanged; path = PyrePath.absolute test_path };
               ]))
    in
    Lwt.return_unit
  in
  ScratchProject.test_server_with
    project
    ~style:ScratchProject.ClientConnection.Style.Concurrent
    ~clients:[subscriber; mutator]


let test_server_subscription_busy_local_update context =
  let project = ScratchProject.setup ~context ~include_typeshed_stubs:false ["test.py", ""] in
  let root = ScratchProject.source_root_of project in
  let test_path = PyrePath.create_relative ~root ~relative:"test.py" in

  (* This is used to ensure mutator always run after subscription is established *)
  let mailbox = Lwt_mvar.create_empty () in
  let subscriber connection =
    let%lwt _ =
      ScratchProject.ClientConnection.send_request
        connection
        Request.(Subscription Subscription.Subscribe)
    in
    let%lwt () = Lwt_mvar.put mailbox "subscribed" in
    let%lwt () =
      ScratchProject.ClientConnection.assert_subscription_response
        ~expected:Response.(ServerStatus (Status.BusyChecking { overlay_id = Some "foo" }))
        connection
    in
    let%lwt () =
      ScratchProject.ClientConnection.assert_subscription_response
        ~expected:Response.(ServerStatus Status.Idle)
        connection
    in
    Lwt.return_unit
  in
  let mutator connection =
    let%lwt _ = Lwt_mvar.take mailbox in
    let%lwt _ =
      ScratchProject.ClientConnection.send_request
        connection
        Request.(
          Command
            (Command.LocalUpdate
               {
                 path = PyrePath.absolute test_path;
                 content = Some "reveal_type(42)";
                 overlay_id = "foo";
               }))
    in
    Lwt.return_unit
  in
  ScratchProject.test_server_with
    project
    ~style:ScratchProject.ClientConnection.Style.Concurrent
    ~clients:[subscriber; mutator]


let test_server_subscription_stop context =
  let project = ScratchProject.setup ~context ~include_typeshed_stubs:false [] in
  let socket_address = ScratchProject.socket_address_of project in
  (* This is used to make sure both subscriber and stopper run after server has started. *)
  let subscriber_mailbox = Lwt_mvar.create_empty () in
  let stopper_mailbox = Lwt_mvar.create_empty () in
  let subscriber (input_channel, output_channel) =
    let%lwt () =
      Request.(Subscription Subscription.Subscribe)
      |> Request.to_yojson
      |> Yojson.Safe.to_string
      |> Lwt_io.write_line output_channel
    in
    (* This should be the initial `Ok` response *)
    let%lwt _ = Lwt_io.read_line input_channel in
    (* This should be the `Stop` status update *)
    let%lwt stop_message = Lwt_io.read_line input_channel in
    match Yojson.Safe.from_string stop_message with
    | `List [`String "ServerStatus"; `List [`String "Stop"; _]] -> Lwt.return_unit
    | _ -> Format.sprintf "Expected a stop message but got: %s" stop_message |> assert_failure
  in
  let stopper (_, output_channel) =
    let%lwt () =
      Request.Command Request.Command.Stop
      |> Request.to_yojson
      |> Yojson.Safe.to_string
      |> Lwt_io.write_line output_channel
    in
    Lwt.return_unit
  in
  Lwt.join
    [
      (* We can't use the [test_server_with ~style:Concurrent ~clients] API in this test: that API
         implicitly assumes the lifetime of all clients must not go beyond the lifetime of the
         server. That assumption is not true in this test, as we are trying to observe server
         behavior AFTER it gets stopped and goes down. *)
      ScratchProject.test_server_with_one_connection project ~f:(fun _ ->
          let%lwt () = Lwt_mvar.put subscriber_mailbox "initialized" in
          let%lwt () = Lwt_mvar.put stopper_mailbox "initialized" in
          let wait_forever, _ = Lwt.wait () in
          wait_forever);
      (let%lwt _ = Lwt_mvar.take subscriber_mailbox in
       Lwt_io.with_connection socket_address subscriber);
      (let%lwt _ = Lwt_mvar.take stopper_mailbox in
       Lwt_io.with_connection socket_address stopper);
    ]


let () =
  "subscription_test"
  >::: [
         "subscription_registration" >:: OUnitLwt.lwt_wrapper test_subscription_registration;
         "server_subscription_establish" >:: OUnitLwt.lwt_wrapper test_server_subscription_establish;
         "server_subscription_busy_file_update"
         >:: OUnitLwt.lwt_wrapper test_server_subscription_busy_file_update;
         "server_subscription_busy_local_update"
         >:: OUnitLwt.lwt_wrapper test_server_subscription_busy_local_update;
         "server_subscription_stop" >:: OUnitLwt.lwt_wrapper test_server_subscription_stop;
       ]
  |> Test.run
