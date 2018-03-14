(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open LanguageServer.Protocol
open Pyre

module Time = Core_kernel.Time_ns.Span
module Protocol = ServerProtocol
module Socket = CommandSocket

let run_command version log_identifier source_root () =
  let source_root = Path.create_absolute source_root in
  let configuration = Configuration.create ~source_root ~log_identifier ?version () in
  let connect_to_server () =
    let server_socket =
      try
        ServerOperations.connect
          ~retries:3
          ~configuration
      with
      | ServerOperations.ConnectionFailure ->
          exit 1
      | ServerOperations.VersionMismatch { ServerOperations.server_version; client_version } ->
          Log.error
            "Exiting due to version mismatch. \
             The server version is %s, but the client was called with %s"
            server_version
            client_version;
          exit 1
    in
    Socket.write server_socket (Protocol.Request.ClientConnectionRequest Protocol.Persistent);
    begin
      match Socket.read server_socket with
      | (Protocol.ClientConnectionResponse Protocol.Persistent) -> ()
      | _ ->
          let message = "Unexpected json response when attempting persistent connection" in
          Log.info "%s" message;
          failwith message
    end;
    server_socket
  in
  let display_nuclide_message message =
    ShowMessage.create LanguageServer.Types.ShowMessageParams.InfoMessage message
    |> ShowMessage.to_yojson
    |> write_message Out_channel.stdout
  in

  (* Log stderr to file *)
  let log_path =
    let persistent_client_directory =
      Configuration.pyre_root configuration
      |> Path.append ~element:"persistent"
      |> Path.absolute
    in
    persistent_client_directory ^/ "client.log"
  in
  Unix.handle_unix_error (fun () -> Unix.mkdir_p (Filename.dirname log_path));
  let log_path = Log.rotate log_path in
  Format.pp_set_formatter_out_channel
    Format.err_formatter
    (Out_channel.create log_path);

  let server_socket = connect_to_server () in

  (* Read the initialize request *)
  LanguageServer.Protocol.read_message In_channel.stdin
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
  >>| LanguageServer.Protocol.write_message Out_channel.stdout |> ignore;

  Statistics.event ~flush:true ~name:"persistent_client_launch" ~configuration ();
  (* Get all initial errors *)
  Socket.write
    server_socket
    (Protocol.Request.TypeCheckRequest Protocol.TypeCheckRequest.empty);

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
        match Socket.read socket with
        | Protocol.LanguageServerProtocolResponse response ->
            LanguageServer.Protocol.write_message
              Out_channel.stdout
              (Yojson.Safe.from_string response)
        | Protocol.ClientExitResponse Protocol.Persistent ->
            Log.info "Received stop request, exiting.";
            Unix.close server_socket;
            exit 0
        | _ -> ()
      in
      let process_stdin_socket () =
        (LanguageServer.Protocol.read_message In_channel.stdin
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
            Server.stop (Path.absolute source_root) ();
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
  Command.basic_spec
    ~summary:"Opens a persistent connection to the server (IDE integration)"
    Command.Spec.(
      empty
      +> flag "-version" (optional string) ~doc:"VERSION Pyre version"
      +> flag
        "-log-identifier"
        (optional_with_default "" string)
        ~doc:"IDENTIFIER Add given identifier to logged samples."
      +> anon (maybe_with_default "." ("source-root" %: string)))
    run_command
