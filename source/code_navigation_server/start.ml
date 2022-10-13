(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core

let parse_json message =
  try Result.Ok (Yojson.Safe.from_string message) with
  | Yojson.Json_error message ->
      let message = Format.sprintf "Cannot parse JSON. %s" message in
      Result.Error message


let handle_invalid_request ~output_channel message =
  Log.info "Invalid request: %s" message;
  let response = Response.Error (Response.ErrorKind.InvalidRequest message) in
  LwtInputOutput.write_line_ignoring_errors ~output_channel (Response.to_string response)


let handle_subscription
    ~server:{ RequestHandler.ServerInternal.subscriptions; _ }
    ~input_channel
    ~output_channel
    _
  =
  Log.info "Processing subscription request";
  let identifier = Subscriptions.register ~output_channel subscriptions in

  (* Initial response acknowledge the subscription *)
  Log.info "Subscription established";
  let%lwt () =
    LwtInputOutput.write_line_ignoring_errors ~output_channel (Response.to_string Response.Ok)
  in

  (* Block on reading the input channel so we could dispose the subscription immediately when it's
     closed.

     NOTE(grievejia): Another option is to close [input_channel] immediately, and clean up
     subscription later when we try to send something to a given channel and run into an error
     (usually indicating that the corresponding output channel is closed). Slightly prefer this way
     as it feels more explicit. *)
  let rec block_until_disconnect () =
    match%lwt Lwt_io.read_line_opt input_channel with
    | None ->
        Subscriptions.unregister ~identifier subscriptions;
        Log.info "Subscription removed";
        Lwt.return_unit
    | Some _ -> block_until_disconnect ()
  in
  block_until_disconnect ()


let handle_command_and_stop_if_necessary ~server command =
  match%lwt RequestHandler.handle_command ~server command with
  | Result.Error reason -> Server.Stop.stop_waiting_server reason
  | Result.Ok response -> Lwt.return response


let handle_query ~server ~output_channel query =
  Log.info "Processing query `%a`" Sexp.pp (Request.Query.sexp_of_t query);
  let%lwt response = RequestHandler.handle_query ~server query in
  let raw_response = Response.to_string response in
  Log.info "Query processed. Response: `%s`" raw_response;
  LwtInputOutput.write_line_ignoring_errors ~output_channel raw_response


let handle_command ~server ~output_channel command =
  Log.info "Processing command `%a`" Sexp.pp (Request.Command.sexp_of_t command);
  let%lwt response = handle_command_and_stop_if_necessary ~server command in
  let raw_response = Response.to_string response in
  Log.info "Command processed. Response: `%s`" raw_response;
  LwtInputOutput.write_line_ignoring_errors ~output_channel raw_response


let handle_connection ~server _client_address (input_channel, output_channel) =
  Log.info "Connection established";
  let handle_line () =
    match%lwt Lwt_io.read_line_opt input_channel with
    | None -> Lwt.return_unit
    | Some message -> (
        Log.info "Processing message `%s`" message;
        match parse_json message with
        | Result.Error message -> handle_invalid_request ~output_channel message
        | Result.Ok json -> (
            match Request.of_yojson json with
            | Result.Error _ -> handle_invalid_request ~output_channel "Unrecognized request JSON"
            | Result.Ok (Request.Query query) -> handle_query ~server ~output_channel query
            | Result.Ok (Request.Command command) -> handle_command ~server ~output_channel command
            | Result.Ok (Request.Subscription subscription) ->
                handle_subscription ~server ~input_channel ~output_channel subscription))
  in
  let on_uncaught_exception exn =
    Log.warning "Uncaught exception: %s" (Exn.to_string exn);
    Server.Stop.(stop_waiting_server (Reason.UncaughtException exn))
  in
  let%lwt () = Lwt.catch handle_line on_uncaught_exception in
  Log.info "Connection closed";
  Lwt.return_unit


let on_watchman_update ~server paths =
  let create_file_update_event path =
    let kind =
      if PyrePath.file_exists path then
        Request.FileUpdateEvent.Kind.CreatedOrChanged
      else
        Request.FileUpdateEvent.Kind.Deleted
    in
    { Request.FileUpdateEvent.kind; path = PyrePath.absolute path }
  in
  let update_command = Request.Command.FileUpdate (List.map paths ~f:create_file_update_event) in
  (* File watcher does not care about the content of the the response. *)
  let%lwt _ = handle_command_and_stop_if_necessary ~server update_command in
  Lwt.return_unit


let initialize_shared_memory environment_controls =
  Analysis.EnvironmentControls.configuration environment_controls
  |> Memory.get_heap_handle
  |> ignore


let initialize_server_state environment_controls =
  initialize_shared_memory environment_controls;
  let environment =
    Analysis.ErrorsEnvironment.create environment_controls |> Analysis.OverlaidEnvironment.create
  in
  { State.environment }


let broadcast_server_stop_and_fail ~subscriptions ~message exn =
  let%lwt () =
    Subscriptions.broadcast
      subscriptions
      ~response:(lazy Response.(ServerStatus (Status.Stop { message })))
  in
  Lwt.fail exn


let start_server
    ~on_started
    { StartOptions.environment_controls; socket_path; source_paths; watchman; critical_files; _ }
  =
  let ({ Configuration.Analysis.extensions; _ } as configuration) =
    Analysis.EnvironmentControls.configuration environment_controls
  in
  (* Watchman connection needs to be up before server can start -- otherwise we risk missing
     filesystem updates during server establishment. *)
  let%lwt watchman_subscriber =
    Server.Start.get_optional_watchman_subscriber ~critical_files ~extensions ~source_paths watchman
  in
  let properties = Server.ServerProperties.create ~socket_path ~critical_files ~configuration () in
  let state = Server.ExclusiveLock.create (initialize_server_state environment_controls) in
  let subscriptions = Subscriptions.create () in
  let server = { RequestHandler.ServerInternal.properties; state; subscriptions } in
  let after_server_starts () =
    Log.info "Code navigation server has started listening on socket `%a`" PyrePath.pp socket_path;
    let waiters =
      let server_waiter () = on_started properties state in
      let watchman_waiter subscriber =
        let%lwt () = Server.Watchman.Subscriber.listen ~f:(on_watchman_update ~server) subscriber in
        let message = "Lost subscription connection to watchman" in
        broadcast_server_stop_and_fail
          ~subscriptions
          ~message
          (Server.Watchman.SubscriptionError message)
      in
      let signal_waiters =
        [
          (* We rely on SIGINT for normal server shutdown. *)
          Server.Start.wait_for_signal [Signal.int] ~on_caught:(fun _ ->
              let stop_reason = Server.Stop.get_last_server_stop_reason () in
              broadcast_server_stop_and_fail
                ~subscriptions
                ~message:
                  (Option.value_map
                     stop_reason
                     ~f:Server.Stop.Reason.message_of
                     ~default:"Code navigation server stopped for unknown reason")
                (Server.Start.ServerStopped stop_reason));
          (* Getting these signals usually indicates something serious went wrong. *)
          Server.Start.wait_for_signal
            [Signal.abrt; Signal.term; Signal.quit; Signal.segv]
            ~on_caught:(fun signal ->
              broadcast_server_stop_and_fail
                ~subscriptions
                ~message:
                  (Format.sprintf
                     "Code navigation server interrupted by signal `%s`"
                     (Signal.to_string signal))
                (Server.Start.ServerInterrupted signal));
        ]
      in
      List.concat_no_order
        [
          [server_waiter ()];
          signal_waiters;
          Option.map watchman_subscriber ~f:watchman_waiter |> Option.to_list;
        ]
    in
    Lwt.choose waiters
  in
  let after_server_stops () =
    Log.info "Code navigation server is going down. Cleaning up...";
    Lwt.return_unit
  in
  LwtSocketServer.SocketAddress.create_from_path socket_path
  |> LwtSocketServer.with_server ~handle_connection:(handle_connection ~server) ~f:(fun () ->
         Lwt.finalize after_server_starts after_server_stops)
