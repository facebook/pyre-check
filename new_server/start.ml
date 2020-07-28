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


let request_from_string request_string =
  try
    let json = Yojson.Safe.from_string request_string in
    match Request.of_yojson json with
    | Result.Error _ -> Result.Error "Malformed JSON request"
    | Result.Ok request -> Result.Ok request
  with
  | Yojson.Json_error message -> Result.Error message


let handle_request ~server_state request =
  let open Lwt.Infix in
  let server_state = Lazy.force server_state in
  let on_uncaught_server_exception exn =
    Log.info "Uncaught server exception: %s" (Exn.to_string exn);
    let () =
      let { ServerState.server_configuration; _ } = !server_state in
      StartupNotification.produce_for_configuration
        ~server_configuration
        "Restarting Pyre server due to unexpected crash"
    in
    Stop.stop_waiting_server ()
  in
  Lwt.catch
    (fun () -> RequestHandler.process_request ~state:!server_state request)
    on_uncaught_server_exception
  >>= fun (new_state, response) ->
  server_state := new_state;
  Lwt.return response


let handle_connection ~server_state _client_address (input_channel, output_channel) =
  let open Lwt.Infix in
  (* Raw request messages are processed line-by-line. *)
  let rec handle_line () =
    Lwt_io.read_line_opt input_channel
    >>= function
    | None ->
        Log.info "Connection closed";
        Lwt.return_unit
    | Some message ->
        let response =
          match request_from_string message with
          | Result.Error message -> Lwt.return (Response.Error message)
          | Result.Ok request -> handle_request ~server_state request
        in
        response
        >>= fun response ->
        Response.to_yojson response
        |> Yojson.Safe.to_string
        |> Lwt_io.write_line output_channel
        >>= handle_line
  in
  handle_line ()


let initialize_server_state ({ ServerConfiguration.log_path; _ } as server_configuration) =
  Log.info "Initializing server state...";
  let configuration = ServerConfiguration.analysis_configuration_of server_configuration in
  let { Service.Check.environment; errors } =
    Scheduler.with_scheduler ~configuration ~f:(fun scheduler ->
        Service.Check.check
          ~scheduler
          ~configuration
          ~call_graph_builder:(module Analysis.Callgraph.DefaultBuilder))
  in
  let error_table =
    let table = Ast.Reference.Table.create () in
    let add_error error =
      let key = Analysis.AnalysisError.path error in
      Hashtbl.add_multi table ~key ~data:error
    in
    List.iter errors ~f:add_error;
    table
  in
  let state =
    ref
      {
        ServerState.socket_path = socket_path_of log_path;
        server_configuration;
        configuration;
        type_environment = environment;
        error_table;
      }
  in
  Log.info "Server state initialized.";
  state


let with_server ~f ({ ServerConfiguration.log_path; _ } as server_configuration) =
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


let start_server ~on_started ~on_exception server_configuration =
  let open Lwt in
  catch (fun () -> with_server server_configuration ~f:on_started) on_exception


let start_server_and_wait server_configuration =
  let open Lwt in
  start_server
    server_configuration
    ~on_started:(fun _ -> wait_on_signals [Signal.int])
    ~on_exception:(fun exn ->
      Log.error "Exception thrown from Pyre server.";
      Log.error "%s" (Exn.to_string exn);
      return_unit)
