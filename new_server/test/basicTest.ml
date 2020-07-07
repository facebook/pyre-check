(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Newserver

let with_temporary_directory ~f () =
  let temporary_directory = Filename.temp_dir "pyre_test" "" in
  Exn.protect
    ~f:(fun () -> f temporary_directory)
    ~finally:(fun () -> Unix.remove temporary_directory)


let test_client ~server_state (input_channel, output_channel) =
  let open Lwt.Infix in
  let assert_response ~expected request =
    (* Send the request to the server. *)
    Request.to_yojson request
    |> Yojson.Safe.to_string
    |> Lwt_io.write_line output_channel
    >>= fun _ ->
    (* Receive a response from the server *)
    Lwt_io.read_line input_channel
    >>= fun actual_message ->
    let expected_message = Response.to_yojson expected |> Yojson.Safe.to_string in
    ( if not (String.equal expected_message actual_message) then
        let message =
          Format.asprintf
            "Unexpected response from the server.\nExpected: %s\nActual: %s"
            expected_message
            actual_message
        in
        failwith message );
    Lwt.return_unit
  in
  (* Test if the `GetInfo` request works properly. *)
  let request = Request.GetInfo in
  RequestHandler.process_request ~state:server_state request
  >>= fun (_, expected) ->
  assert_response request ~expected
  >>= fun () ->
  Log.info "Test passed!";
  Lwt.return_unit


let basic_test log_path =
  let server_configuration = { ServerConfiguration.log_path } in
  (* Start the server first *)
  Start.start_server server_configuration ~f:(function
      | None -> failwith "Server fails to start."
      | Some server_state ->
          (* Open a connection to the started server and send some test messages. *)
          let socket_address =
            let { ServerState.socket_path; _ } = !server_state in
            Lwt_unix.ADDR_UNIX (Pyre.Path.absolute socket_path)
          in
          Lwt_io.with_connection socket_address (test_client ~server_state:!server_state))


let () =
  with_temporary_directory () ~f:(fun log_path ->
      let log_path = Pyre.Path.create_absolute log_path in
      Lwt_main.run (basic_test log_path))
