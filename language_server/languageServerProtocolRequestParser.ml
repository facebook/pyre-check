(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


open ServerProtocol
open Request
open Pyre


let parse ~root ~check_on_save request =
  let open LanguageServerProtocolTypes in
  let log_method_error method_name =
    Log.error
      "Error for method %s: %s does not have required parameters"
      method_name
      (Yojson.Safe.pretty_to_string request)
  in
  let uri_to_contained_relative_path ~root ~uri =
    let to_relative_path ~root ~path =
      String.chop_prefix ~prefix:(root ^ "/") path
      |> Option.value ~default:path
    in
    String.chop_prefix ~prefix:"file://" uri
    >>= (fun path ->
        if String.is_prefix ~prefix:root path then
          Some (to_relative_path ~root ~path)
        else
          None)
    |> Option.value ~default:uri
  in
  let handle_request request_method =
    match request_method with
    | "textDocument/definition" ->
        begin
          match TextDocumentDefinitionRequest.of_yojson request with
          | Ok {
              TextDocumentDefinitionRequest.parameters = Some {
                  TextDocumentPositionParams.textDocument = {
                    TextDocumentIdentifier.uri;
                    _;
                  };
                  position = { Position.line; character };
                };
              id;
              _;
            } ->
              let path =
                uri_to_contained_relative_path
                  ~root:(Path.absolute root)
                  ~uri
              in
              Some (GetDefinitionRequest {
                  DefinitionRequest.id;
                  path;
                  position = { Ast.Location.line; column = character };
                })
          | Ok _ -> None
          | Error yojson_error -> Log.dump "%s" yojson_error; None
        end
    | "textDocument/didClose" ->
        begin
          match DidCloseTextDocument.of_yojson request with
          | Ok {
              DidCloseTextDocument.parameters = Some {
                  DidCloseTextDocumentParams.textDocument = {
                    TextDocumentIdentifier.uri;
                    _;
                  };
                  _
                };
              _;
            } ->
              Log.log
                ~section:`Server
                "Closed file %s"
                (uri_to_contained_relative_path
                   ~root:(Path.absolute root)
                   ~uri);
              None
          | Ok _ -> log_method_error request_method; None
          | Error yojson_error -> Log.log ~section:`Server "Error: %s" yojson_error; None
        end

    | "textDocument/didOpen" ->
        begin
          match DidOpenTextDocument.of_yojson request with
          | Ok {
              DidOpenTextDocument.parameters = Some {
                  DidOpenTextDocumentParams.textDocument = {
                    TextDocumentItem.uri;
                    _;
                  };
                  _;
                };
              _;
            } ->
              let file =
                uri_to_contained_relative_path
                  ~root:(Path.absolute root)
                  ~uri
                |> (fun relative ->
                    Path.create_relative ~root ~relative)
                |> File.create
              in
              Log.log ~section:`Server "Opened file %a" File.pp file;
              Some (DisplayTypeErrors [file])
          | Ok _ -> log_method_error request_method; None
          | Error yojson_error -> Log.log ~section:`Server "Error: %s" yojson_error; None
        end

    | "textDocument/didSave" ->
        if check_on_save then
          begin
            match DidSaveTextDocument.of_yojson request with
            | Ok {
                DidSaveTextDocument.parameters = Some {
                    DidSaveTextDocumentParams.textDocument = {
                      TextDocumentIdentifier.uri;
                      _;
                    };
                    text;
                  };
                _;
              } ->
                let file =
                  uri_to_contained_relative_path
                    ~root:(Path.absolute root)
                    ~uri
                  |> (fun relative ->
                      Path.create_relative ~root ~relative)
                  |> File.create ~content:text
                in
                Some
                  (TypeCheckRequest {
                      TypeCheckRequest.update_environment_with = [file]; check = [file]; })
            | Ok _ -> log_method_error request_method; None
            | Error yojson_error -> Log.log ~section:`Server "Error: %s" yojson_error; None
          end
        else
          begin
            Log.log ~section:`Server "Explicitly ignoring didSave request";
            None
          end

    | "textDocument/hover" ->
        begin
          match HoverRequest.of_yojson request with
          | Ok {
              HoverRequest.parameters = Some {
                  TextDocumentPositionParams.textDocument = {
                    TextDocumentIdentifier.uri;
                    _;
                  };
                  position = { Position.line; character };
                };
              id;
              _;
            } ->
              let path =
                uri_to_contained_relative_path
                  ~root:(Path.absolute root)
                  ~uri
              in
              Some (HoverRequest {
                  DefinitionRequest.id;
                  path;
                  position = { Ast.Location.line = line + 1; column = character + 1};
                })
          | Ok _ -> None
          | Error yojson_error -> Log.log ~section:`Server "Error: %s" yojson_error; None
        end

    | "shutdown" ->
        begin
          match ShutdownRequest.of_yojson request with
          | Ok { ShutdownRequest.id; _ } -> Some (ClientShutdownRequest id)
          | Error yojson_error -> Log.log ~section:`Server "Error: %s" yojson_error; None
        end

    | "exit" -> Some (ClientExitRequest Persistent)
    | "telemetry/rage" ->
        begin
          match RageRequest.of_yojson request with
          | Ok { RageRequest.id; _ } -> Some (Request.RageRequest id)
          | Error yojson_error -> Log.log ~section:`Server "Error: %s" yojson_error; None
        end
    | unmatched_method ->
        Log.log ~section:`Server "Unhandled %s" unmatched_method; None
  in
  try
    let request_method = Yojson.Safe.Util.member "method" request in
    handle_request (Yojson.Safe.Util.to_string request_method)
  with Yojson.Safe.Util.Type_error _ -> None
