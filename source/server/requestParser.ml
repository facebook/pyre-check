(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Protocol
open Protocol.Request
open Pyre

let parse_and_translate
    ~configuration:({ Configuration.Analysis.perform_autocompletion; _ } as configuration)
    ~state:{ State.symlink_targets_to_sources; _ }
    ~request
  =
  let open LanguageServer.Types in
  let translate_path ~configuration ~symlink_targets_to_sources path =
    let search_paths = Configuration.Analysis.search_path configuration in
    match SearchPath.search_for_path ~search_paths path with
    | Some SearchPath.{ relative_path; _ } -> Some (Path.Relative relative_path)
    | None ->
        Hashtbl.find symlink_targets_to_sources (Path.absolute path)
        >>= fun path ->
        SearchPath.search_for_path ~search_paths path
        >>| fun SearchPath.{ relative_path; _ } -> Path.Relative relative_path
  in
  let string_to_path string_path = Path.create_absolute string_path in
  let strings_to_scratch_paths strings =
    List.map strings ~f:string_to_path
    |> List.filter_map ~f:(translate_path ~configuration ~symlink_targets_to_sources)
  in
  let uri_to_path ~uri =
    Path.from_uri uri >>= fun path -> translate_path ~configuration ~symlink_targets_to_sources path
  in
  let log_method_error method_name =
    Log.error
      "Error for method %s: %s does not have required parameters"
      method_name
      (Yojson.Safe.pretty_to_string request)
  in
  let to_pyre_position { LanguageServer.Types.Position.line; character } =
    (* The LSP protocol starts a file at line 0, column 0. Pyre starts a file at line 1, column 0. *)
    { Ast.Location.line = line + 1; column = character }
  in
  let process_request request_method =
    match request_method with
    | "textDocument/definition" -> (
        match TextDocumentDefinitionRequest.of_yojson request with
        | Ok
            {
              TextDocumentDefinitionRequest.parameters =
                Some
                  {
                    TextDocumentPositionParameters.textDocument = { TextDocumentIdentifier.uri; _ };
                    position;
                  };
              id;
              _;
            } ->
            uri_to_path ~uri
            >>| fun path ->
            GetDefinitionRequest
              { DefinitionRequest.id; path; position = to_pyre_position position }
        | Ok _ -> None
        | Error yojson_error ->
            Log.log ~section:`Server "Error: %s" yojson_error;
            None)
    | "textDocument/didClose" -> (
        match DidCloseTextDocument.of_yojson request with
        | Ok
            {
              DidCloseTextDocument.parameters =
                Some
                  {
                    DidCloseTextDocumentParameters.textDocument = { TextDocumentIdentifier.uri; _ };
                    _;
                  };
              _;
            } ->
            uri_to_path ~uri
            >>| fun path ->
            Log.log ~section:`Server "Closed file %a" Path.pp path;
            CloseDocument path
        | Ok _ ->
            log_method_error request_method;
            None
        | Error yojson_error ->
            Log.log ~section:`Server "Error: %s" yojson_error;
            None)
    | "textDocument/didOpen" -> (
        match DidOpenTextDocument.of_yojson request with
        | Ok
            {
              DidOpenTextDocument.parameters =
                Some { DidOpenTextDocumentParameters.textDocument = { TextDocumentItem.uri; _ }; _ };
              _;
            } ->
            uri_to_path ~uri
            >>| fun path ->
            Log.log ~section:`Server "Opened file %a" Path.pp path;
            OpenDocument path
        | Ok _ ->
            log_method_error request_method;
            None
        | Error yojson_error ->
            Log.log ~section:`Server "Error: %s" yojson_error;
            None)
    | "textDocument/didChange" -> (
        match DidChangeTextDocument.of_yojson request with
        | Ok
            {
              DidChangeTextDocument.parameters =
                Some
                  {
                    DidChangeTextDocumentParameters.textDocument =
                      { VersionedTextDocumentIdentifier.uri; _ };
                    contentChanges = content_changes;
                  };
              _;
            } ->
            (* We only care about the last text update since we receive full text. *)
            Option.both
              (uri_to_path ~uri)
              (content_changes |> List.last >>| fun change -> change.text)
            >>| (fun (path, content) -> File.create ~content path)
            >>| fun file -> DocumentChange file
        | Ok _ ->
            log_method_error request_method;
            None
        | Error yojson_error ->
            Log.log ~section:`Server "Error: %s" yojson_error;
            None)
    | "textDocument/didSave" -> (
        match DidSaveTextDocument.of_yojson request with
        | Ok
            {
              DidSaveTextDocument.parameters =
                Some
                  {
                    DidSaveTextDocumentParameters.textDocument = { TextDocumentIdentifier.uri; _ };
                    _;
                  };
              _;
            } ->
            uri_to_path ~uri >>| fun path -> SaveDocument path
        | Ok _ ->
            log_method_error request_method;
            None
        | Error yojson_error ->
            Log.log ~section:`Server "Error: %s" yojson_error;
            None)
    | "textDocument/completion" -> (
        match CompletionRequest.of_yojson request with
        | Ok
            {
              CompletionRequest.parameters =
                Some { textDocument = { TextDocumentIdentifier.uri; _ }; position; _ };
              id;
              _;
            }
          when perform_autocompletion ->
            uri_to_path ~uri
            >>| fun path -> CompletionRequest { id; path; position = to_pyre_position position }
        | Ok _ -> None
        | Error yojson_error ->
            Log.log ~section:`Server "Error: %s" yojson_error;
            None)
    | "textDocument/hover" -> (
        match HoverRequest.of_yojson request with
        | Ok
            {
              HoverRequest.parameters =
                Some
                  {
                    TextDocumentPositionParameters.textDocument = { TextDocumentIdentifier.uri; _ };
                    position;
                  };
              id;
              _;
            } ->
            uri_to_path ~uri
            >>| fun path ->
            HoverRequest { DefinitionRequest.id; path; position = to_pyre_position position }
        | Ok _ -> None
        | Error yojson_error ->
            Log.log ~section:`Server "Error: %s" yojson_error;
            None)
    | "textDocument/codeAction" -> (
        match CodeActionRequest.of_yojson request with
        | Ok
            {
              CodeActionRequest.parameters =
                Some
                  {
                    CodeActionParameters.textDocument = { TextDocumentIdentifier.uri; _ };
                    context = { diagnostics; _ };
                    _;
                  };
              id;
              _;
            } ->
            uri_to_path ~uri >>| fun path -> CodeActionRequest { id; uri; diagnostics; path }
        | Ok _ -> None
        | Error yojson_error ->
            Log.log ~section:`Server "Error: %s" yojson_error;
            None)
    | "workspace/executeCommand" -> (
        match ExecuteCommandRequest.of_yojson request with
        | Ok
            {
              ExecuteCommandRequest.parameters = Some { ExecuteCommandParameters.arguments; _ };
              id;
              _;
            } ->
            Some (ExecuteCommandRequest { id; arguments })
        | Ok _ -> None
        | Error yojson_error ->
            Log.log ~section:`Server "Error: %s" yojson_error;
            None)
    | "updateFiles" -> (
        match UpdateFiles.of_yojson request with
        | Ok { UpdateFiles.parameters = Some { files; invalidated = targets; _ }; _ } ->
            let files = strings_to_scratch_paths files in
            if not (List.is_empty targets) then (
              let targets = strings_to_scratch_paths targets |> List.map ~f:Path.show in
              Log.info "Invalidate %d symlinks" (List.length targets);
              List.iter targets ~f:(Hashtbl.remove symlink_targets_to_sources));
            Some (TypeCheckRequest files)
        | Ok _ -> None
        | Error yojson_error ->
            Log.log ~section:`Server "Error: %s" yojson_error;
            None)
    | "displayTypeErrors" -> (
        match LanguageServer.Types.DisplayTypeErrors.of_yojson request with
        | Ok { LanguageServer.Types.DisplayTypeErrors.parameters = Some { files }; _ } ->
            let files = strings_to_scratch_paths files in
            Some (DisplayTypeErrors files)
        | Ok _ -> Some (DisplayTypeErrors [])
        | Error yojson_error ->
            Log.log ~section:`Server "Error: %s" yojson_error;
            None)
    | "shutdown" -> (
        match ShutdownRequest.of_yojson request with
        | Ok { ShutdownRequest.id; _ } -> Some (ClientShutdownRequest id)
        | Error yojson_error ->
            Log.log ~section:`Server "Error: %s" yojson_error;
            None)
    | "window/showStatus" -> (
        match LanguageServer.Types.ShowStatusRequest.of_yojson request with
        | Ok { parameters = Some inner_parameters; _ } -> Some (ShowStatusRequest inner_parameters)
        | Ok _ ->
            Log.log ~section:`Server "Error: ShowStatusRequest - No parameters found";
            None
        | Error yojson_error ->
            Log.log ~section:`Server "Error: %s" yojson_error;
            None)
    | "exit" -> Some (ClientExitRequest Persistent)
    | "telemetry/rage" -> (
        match RageRequest.of_yojson request with
        | Ok { id; _ } -> Some (RageRequest id)
        | Error yojson_error ->
            Log.log ~section:`Server "Error: %s" yojson_error;
            None)
    | "initialize" -> (
        match InitializeRequest.of_yojson request with
        | Ok request ->
            Log.info "Request method: %s" request.InitializeRequest.method_;
            Some (InitializeRequest request.InitializeRequest.id)
        | Error error ->
            Log.log
              ~section:`Server
              "Error: Could not parse initialize request message for record field: %s"
              error;
            None)
    | "initialized" -> Some InitializedRequest
    | unmatched_method ->
        Log.log ~section:`Server "Unhandled %s" unmatched_method;
        None
  in
  try
    let request_method = Yojson.Safe.Util.member "method" request in
    process_request (Yojson.Safe.Util.to_string request_method)
  with
  | Yojson.Safe.Util.Type_error _ -> None
