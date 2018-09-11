(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open LanguageServer.Protocol
open Pyre
open Network
open Server

module Time = Core_kernel.Time_ns.Span


type reason = string
type exit_code = int
exception ClientExit of reason * exit_code

let communicate server_socket =
  let display_nuclide_message message =
    ShowMessage.create LanguageServer.Types.ShowMessageParams.InfoMessage message
    |> ShowMessage.to_yojson
    |> write_message Out_channel.stdout
  in
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

  Statistics.event ~flush:true ~name:"persistent_client_launch" ();
  (* Get all initial errors *)
  Socket.write
    server_socket
    Protocol.Request.FlushTypeErrorsRequest;

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
            raise (ClientExit ("explicit stop request", 0))
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
        | End_of_file ->
            display_nuclide_message "Pyre: Lost connection to server, exiting...";
            raise (ClientExit ("unable to process socket", 0))
    in
    begin
      try
        List.iter ~f:process_socket read
      with Unix.Unix_error (error, name, parameters) ->
        Log.log_unix_error (error, name, parameters);
        Unix.close server_socket;
        raise (ClientExit ("Unix error while processing sockets", 0))
    end;
    listen server_socket ()
  in
  listen server_socket ()


let run_command expected_version log_identifier local_root () =
  let local_root = Path.create_absolute local_root in
  let configuration =
    Configuration.create
      ~local_root
      ~log_identifier
      ?expected_version
      ()
  in
  (fun () ->
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
    Format.pp_set_formatter_out_channel Format.err_formatter (Out_channel.create log_path);
    Version.log_version_banner ();

    try
      let server_socket =
        let server_socket =
          try
            Server.Operations.connect
              ~retries:3
              ~configuration
          with
          | Server.Operations.ConnectionFailure ->
              raise (ClientExit ("connection failure", 1))
          | Server.Operations.VersionMismatch {
              Server.Operations.server_version;
              expected_version;
            } ->
              Log.error
                "Exiting due to version mismatch. \
                 The server version is %s, but the client was called with %s"
                server_version
                expected_version;
              raise (ClientExit ("version mismatch", 1))
        in
        Socket.write server_socket (Protocol.Request.ClientConnectionRequest Protocol.Persistent);
        begin
          match Socket.read server_socket with
          | (Protocol.ClientConnectionResponse Protocol.Persistent) -> ()
          | _ ->
              let message = "Unexpected json response when attempting persistent connection" in
              Log.info "%s" message;
              raise (ClientExit ("unexpected json response", 1))
        end;
        server_socket
      in
      communicate server_socket
    with
    | ClientExit (reason, exit_code) ->
        Statistics.event
          ~flush:true
          ~name:"client exit"
          ~integers:["exit code", exit_code]
          ~normals:["reason", reason]
          ();
        exit exit_code
    | uncaught_exception ->
        Statistics.event
          ~section:`Error
          ~flush:true
          ~name:"uncaught exception"
          ~normals:[
            "exception", Exn.to_string uncaught_exception;
            "exception backtrace", Printexc.get_backtrace ();
            "exception origin", "persistent";
          ]
          ();
        raise uncaught_exception)
  |> Scheduler.run_process ~configuration


let command =
  Command.basic_spec
    ~summary:"Opens a persistent connection to the server (IDE integration)"
    Command.Spec.(
      empty
      +> flag
        "-expected-binary-version"
        (optional string)
        ~doc:"VERSION When connecting to a server, this is version we are expecting to connect to."
      +> flag
        "-log-identifier"
        (optional_with_default "" string)
        ~doc:"IDENTIFIER Add given identifier to logged samples."
      +> anon (maybe_with_default "." ("source-root" %: string)))
    run_command
