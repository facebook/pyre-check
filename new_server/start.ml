(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
module Path = Pyre.Path

(* Socket paths in most Unixes are limited to a length of +-100 characters, whereas `log_path` might
   exceed that limit. We have to work around this by shortening the original log path into
   `/tmp/pyre_server_XXX.sock`, where XXX is obtained by computing an MD5 hash of `log_path`. *)
(* Note that creating socket path this way implicitly assumes that `log_path` uniquely determines
   server instances. *)
let socket_path_of log_path =
  let socket_directory = Path.create_absolute ~follow_symbolic_links:false Filename.temp_dir_name in
  let log_path_digest = Path.absolute log_path |> Digest.string |> Digest.to_hex in
  Path.create_relative
    ~root:socket_directory
    ~relative:(Format.sprintf "pyre_server_%s.sock" log_path_digest)


let handle_request ~state request_string =
  let open Lwt.Infix in
  let result =
    try
      let json = Yojson.Safe.from_string request_string in
      match Request.of_yojson json with
      | Result.Error _ -> Lwt.return (state, Response.Error "Malformed JSON request")
      | Result.Ok request -> RequestHandler.process_request ~state request
    with
    | Yojson.Json_error message -> Lwt.return (state, Response.Error message)
  in
  result
  >|= fun (server_state, response) ->
  server_state, Response.to_yojson response |> Yojson.Safe.to_string


let handle_connection ~server_state _client_address (input_channel, output_channel) =
  let open Lwt in
  let server_state = Lazy.force server_state in
  (* Raw request messages are processed line-by-line. *)
  let rec handle_line () =
    Lwt_io.read_line_opt input_channel
    >>= function
    | None ->
        Log.info "Connection closed";
        return_unit
    | Some message ->
        let on_uncaught_server_exception exn =
          Log.info "Uncaught server exception: %s" (Exn.to_string exn);
          Stop.stop_waiting_server ()
        in
        catch (fun () -> handle_request ~state:!server_state message) on_uncaught_server_exception
        >>= fun (new_state, response) ->
        server_state := new_state;
        Lwt_io.write_line output_channel response >>= handle_line
  in
  handle_line ()


let initialize_server_state { ServerConfiguration.log_path } =
  Log.info "Initializing server state...";
  let state = ref { ServerState.socket_path = socket_path_of log_path } in
  Log.info "Server state initialized.";
  state


let with_server ~f ({ ServerConfiguration.log_path } as server_configuration) =
  let open Lwt in
  let socket_path = socket_path_of log_path in
  let server_state =
    (* We do not want the expensive server initialization to happen before we start to listen on the
       socket. Hence the use of `lazy` here to delay the initialization. *)
    lazy (initialize_server_state server_configuration)
  in
  Lwt_io.establish_server_with_client_address
    (Lwt_unix.ADDR_UNIX (Path.absolute socket_path))
    (handle_connection ~server_state)
  >>= fun server ->
  Log.info "Server has started listening on socket `%a`" Path.pp socket_path;
  (* Force the server state initialization to run immediately so the computation does not need to
     happen inside request handlers. *)
  let server_state = Lazy.force server_state in
  (* Make sure that the server gets properly cleaned up no matter what `f` does. *)
  let server_destructor () =
    Log.info "Server is going down. Cleaning up...";
    Lwt_io.shutdown_server server
  in
  finalize (fun () -> f server_state) server_destructor


(* Create a promise that only gets fulfilled when given unix signals are received. *)
let wait_on_signals fatal_signals =
  let open Lwt in
  let waiter, resolver = wait () in
  List.iter fatal_signals ~f:(fun signal ->
      let signal = Signal.to_caml_int signal in
      Lwt_unix.on_signal signal (wakeup resolver) |> ignore);
  waiter
  >>= fun signal ->
  Log.info "Server interrupted with signal %d" signal;
  return_unit


let start_server ~f server_configuration =
  let open Lwt in
  let on_server_startup_failure exn =
    Log.error "Cannot start Pyre server.";
    Log.error "%s" (Exn.to_string exn);
    f None
  in
  catch
    (fun () -> with_server server_configuration ~f:(fun server_state -> f (Some server_state)))
    on_server_startup_failure


let start_server_and_wait server_configuration =
  let open Lwt in
  start_server server_configuration ~f:(function
      | None -> return_unit
      | Some _ -> wait_on_signals [Signal.int])
