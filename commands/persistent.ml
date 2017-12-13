(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open LanguageServerProtocol
open Pyre

module Time = Core_kernel.Time_ns.Span
module Socket = PyreSocket

let run_command version project_root () =
  let project_root = Path.create_absolute project_root in
  let configuration = Configuration.create ~project_root ?version () in
  let connect_to_server () =
    let server_socket =
      try
        Server.connect
          ~retries:3
          ~configuration
      with
      | Server.ConnectionFailure ->
          exit 1
    in
    Socket.write server_socket (Protocol.Request.ClientConnectionRequest Protocol.Persistent);
    (match Socket.read server_socket with
     | (Protocol.ClientConnectionResponse Protocol.Persistent) -> ()
     | _ ->
         let message = "Unexpected json response when attempting persistent connection" in
         Log.info "%s" message;
         failwith message);
    server_socket
  in
  let display_nuclide_message message =
    ShowMessage.create LanguageServerProtocolTypes.ShowMessageParams.InfoMessage message
    |> ShowMessage.to_yojson
    |> write_message Out_channel.stdout
  in

  (* Log stderr to file *)
  let log_path, link_path =
    let persistent_client_directory =
      Configuration.pyre_root configuration
      |> Path.append ~element:"persistent"
      |> Path.absolute
    in
    persistent_client_directory ^/ "client_" ^ (Uuid.create () |> Uuid.to_string),
    persistent_client_directory ^/ "client.log"
  in
  Unix.handle_unix_error (fun () -> Unix.mkdir_p (Filename.dirname log_path));
  Format.pp_set_formatter_out_channel
    Format.err_formatter
    (Out_channel.create log_path);
  (* Create a link for the most recent persistent client *)
  (try Unix.unlink link_path with | Unix.Unix_error _ -> ());
  Unix.symlink ~src:log_path ~dst:link_path;

  let server_socket = connect_to_server () in

  (* Read the initialize request *)
  LanguageServerProtocol.read_message In_channel.stdin
  >>| InitializeRequest.of_yojson
  >>| begin function
    | Ok request ->
        Log.info "Request method: %s" request.InitializeRequest.method_;
        request.InitializeRequest.id
    | Error error ->
        let message =
          Format.sprintf
            "Could not parse initialize request message for record field: %s"
            error in
        Log.info "%s" message;
        failwith message
  end
  (* Write the initialize response *)
  >>| InitializeResponse.default
  >>| InitializeResponse.to_yojson
  >>| LanguageServerProtocol.write_message Out_channel.stdout |> ignore;

  (* Get all initial errors *)
  Socket.write
    server_socket
    (Protocol.Request.TypeCheckRequest { Protocol.files = []; check_dependents = false });

  let rec listen server_socket () =
    let read =
      Unix.select
        ~read:[server_socket; Unix.stdin]
        ~write:[]
        ~except:[]
        ~timeout:(`After (Time.of_sec 1.0))
        ()
      |> fun { Unix.Select_fds.read; _ } -> read
    in
    let process_socket socket =
      let process_server_socket () =
        (match Socket.read socket with
         | Protocol.LanguageServerProtocolResponse response ->
             LanguageServerProtocol.write_message
               Out_channel.stdout
               (Yojson.Safe.from_string response)
         | Protocol.ClientExitResponse Protocol.Persistent ->
             Log.info "Received stop request, exiting.";
             Unix.close server_socket;
             exit 0
         | _ -> ())
      in
      let process_stdin_socket () =
        (LanguageServerProtocol.read_message In_channel.stdin
         >>| (fun message ->
             Protocol.Request.LanguageServerProtocolRequest (Yojson.Safe.to_string message))
         >>| Socket.write server_socket)
        |> ignore
      in
      if socket = Unix.stdin then
        process_stdin_socket ()
      else
        try
          process_server_socket ()
        with
        (* If the server closed and we're still alive, restart it. *)
        | End_of_file ->
            display_nuclide_message "Pyre: Lost connection to server, exiting...";
            Log.info "Stopping server due to an end-of-file while reading";
            Server.stop (Path.absolute project_root) ();
            exit 0
    in
    try
      List.iter ~f:process_socket read;
      listen server_socket ()
    with Unix.Unix_error _ ->
      Unix.close server_socket;
      exit 0
  in
  listen server_socket ()


let command =
  Command.basic
    ~summary:"Opens a persistent connection to the server (IDE integration)"
    Command.Spec.(
      empty
      +> flag "-version" (optional string) ~doc:"Pyre version"
      +> anon (maybe_with_default "." ("project-root" %: string)))
    run_command
