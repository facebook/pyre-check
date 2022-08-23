(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

let handle_connection ~server_properties:_ _client_address (input_channel, output_channel) =
  Log.info "Connection established";
  let rec handle_line () =
    match%lwt Lwt_io.read_line_opt input_channel with
    | None ->
        Log.info "Connection closed";
        Lwt.return_unit
    | Some raw_request ->
        Log.info "Processing request `%s`" raw_request;
        let%lwt response = RequestHandler.handle_raw_request raw_request in
        let raw_response = Response.to_string response in
        Log.info "Request processed. Response: `%s`" raw_response;
        let on_io_exception exn =
          Log.warning "Exception occurred while sending responses: %s" (Exn.to_string exn);
          Lwt.return_unit
        in
        let%lwt () =
          Lwt.catch (fun () -> Lwt_io.write_line output_channel raw_response) on_io_exception
        in
        handle_line ()
  in
  let on_uncaught_exception exn =
    Log.warning "Uncaught exception: %s" (Exn.to_string exn);
    Lwt.return_unit
  in
  Lwt.catch handle_line on_uncaught_exception


let with_server ~on_started { StartOptions.environment_controls; socket_path; critical_files; _ } =
  let configuration = Analysis.EnvironmentControls.configuration environment_controls in
  let server_properties =
    Server.ServerProperties.create ~socket_path ~critical_files ~configuration ()
  in
  let after_server_starts () =
    Log.info "Code navigation server has started listening on socket `%a`" PyrePath.pp socket_path;
    let waiters =
      let server_waiter () = on_started server_properties in
      let signal_waiters =
        [
          (* We rely on SIGINT for normal server shutdown. *)
          Server.Start.wait_for_signal [Signal.int] ~on_caught:(fun _ ->
              Lwt.fail Server.Start.ServerStopped);
          (* Getting these signals usually indicates something serious went wrong. *)
          Server.Start.wait_for_signal
            [Signal.abrt; Signal.term; Signal.quit; Signal.segv]
            ~on_caught:(fun signal -> Lwt.fail (Server.Start.ServerInterrupted signal));
        ]
      in
      server_waiter () :: signal_waiters
    in
    Lwt.choose waiters
  in
  let after_server_stops () =
    Log.info "Code navigation server is going down. Cleaning up...";
    Lwt.return_unit
  in
  LwtSocketServer.SocketAddress.create_from_path socket_path
  |> LwtSocketServer.with_server
       ~handle_connection:(handle_connection ~server_properties)
       ~f:(fun () -> Lwt.finalize after_server_starts after_server_stops)


let start_server ~on_started ~on_exception start_options =
  Lwt.catch (fun () -> with_server ~on_started start_options) on_exception
