(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Analysis
open Types

module Range = struct
  include Types.Range

  let create
      ~start:{ Ast.Location.line = start_line; column = start_column }
      ~stop:{ Ast.Location.line = stop_line; column = stop_column }
    =
    {
      start = { Position.line = start_line - 1; character = start_column };
      end_ = { Position.line = stop_line - 1; character = stop_column };
    }
end

module InitializeRequest = Types.InitializeRequest
module CodeActionOptions = Types.CodeActionOptions
module TextDocumentDefinitionRequest = Types.TextDocumentDefinitionRequest

module PublishDiagnostics = struct
  include Types.PublishDiagnostics

  let of_errors path errors =
    let diagnostic_of_error error =
      let { Ast.Location.WithPath.start; stop; _ } = TypeCheck.Error.Instantiated.location error in
      Diagnostic.
        {
          range = Range.create ~start ~stop;
          severity = Some DiagnosticSeverity.Error;
          code = None;
          source = Some "Pyre";
          message = TypeCheck.Error.Instantiated.description error;
        }
    in
    {
      jsonrpc = "2.0";
      method_ = "textDocument/publishDiagnostics";
      parameters =
        Some
          {
            PublishDiagnosticsParameters.uri = Path.Absolute path |> Path.uri;
            diagnostics = List.map ~f:diagnostic_of_error errors;
          };
    }


  let clear_diagnostics_for_uri ~uri =
    {
      jsonrpc = "2.0";
      method_ = "textDocument/publishDiagnostics";
      parameters = Some { PublishDiagnosticsParameters.uri; diagnostics = [] };
    }


  let uri { parameters; _ } =
    match parameters with
    | Some { PublishDiagnosticsParameters.uri; _ } -> uri
    | None -> failwith "Malformed request"
end

module ApplyWorkspaceEdit = struct
  include Types.ApplyWorkspaceEdit

  let create ~id edit =
    { jsonrpc = "2.0"; method_ = "workspace/applyEdit"; id; parameters = Some { edit } }
end

module DidSaveTextDocument = struct
  include Types.DidSaveTextDocument

  let create ?(root = Path.current_working_directory ()) path content =
    try
      Ok
        {
          jsonrpc = "2.0";
          method_ = "textDocument/didSave";
          parameters =
            Some
              {
                DidSaveTextDocumentParameters.textDocument =
                  {
                    TextDocumentIdentifier.uri =
                      Path.create_relative ~root ~relative:path |> Path.real_path |> Path.uri;
                    version = None;
                  };
                text = content;
              };
        }
    with
    | Unix.Unix_error _ ->
        Format.sprintf "Valid path does not exist for file %s." path |> Or_error.error_string
end

module ShowMessage = struct
  include Types.ShowMessage

  let create messageType content =
    {
      jsonrpc = "2.0";
      method_ = "window/showMessage";
      parameters =
        Some
          {
            ShowMessageParameters.messageType = ShowMessageParameters.messageTypeNumber messageType;
            message = content;
          };
    }
end

module ShowStatus = struct
  include Types.ShowStatus

  let create ~message_type ~content ~short_message ~progress =
    {
      jsonrpc = "2.0";
      method_ = "window/showStatus";
      parameters =
        Some
          {
            ShowStatusParameters.type_ = ShowMessageParameters.messageTypeNumber message_type;
            progress;
            message = content;
            actions = None;
            shortMessage = short_message;
          };
      id = Int (Int.of_float (Unix.time ()));
    }
end

module InitializeResponse = struct
  include Types.InitializeResponse

  type features = {
    autocomplete: bool;
    click_to_fix: bool;
    hover: bool;
    go_to_definition: bool;
  }

  let default id ~server_uuid ~features:{ autocomplete; click_to_fix; hover; go_to_definition } =
    let open TextDocumentSyncOptions in
    {
      jsonrpc = "2.0";
      id;
      result =
        Some
          InitializeResult.
            {
              capabilities =
                InitializeResult.ServerCapabilities.
                  {
                    text_document_sync =
                      Some
                        {
                          open_close = Some true;
                          change = Some (Kind.get_change Kind.Full);
                          will_save = None;
                          will_save_wait_until = None;
                          save = Some { SaveOptions.include_text = Some false };
                        };
                    hover_provider = Some hover;
                    completion_provider =
                      Option.some_if
                        autocomplete
                        {
                          CompletionOptions.resolve_provider = Some false;
                          trigger_characters = Some ["."];
                        };
                    signature_help_provider = None;
                    definition_provider = Some go_to_definition;
                    references_provider = None;
                    document_highlight_provider = None;
                    document_symbol_provider = None;
                    workspace_symbol_provider = None;
                    code_action_provider =
                      Option.some_if
                        click_to_fix
                        { CodeActionOptions.codeActionKinds = ["refactor.rewrite"] };
                    code_lens_provider = None;
                    document_formatting_provider = None;
                    document_range_formatting_provider = None;
                    document_on_type_formatting_provider = None;
                    rename_provider = None;
                    document_link_provider = None;
                    execute_command_provider =
                      Some
                        ExecuteCommandOptions.{ commands = ["add_pyre_annotation_" ^ server_uuid] };
                    experimental = None;
                    rage_provider = Some true;
                  };
            };
      error = None;
    }
end

module ShutdownResponse = struct
  include Types.ShutdownResponse

  let default id = { jsonrpc = "2.0"; id; result = None; error = None }
end

module CompletionResponse = struct
  include Types.CompletionResponse

  let create ~id ~items = { jsonrpc = "2.0"; id; result = Some items; error = None }
end

module TextDocumentDefinitionResponse = struct
  include Types.TextDocumentDefinitionResponse

  let create_with_result ~id result = { jsonrpc = "2.0"; id; result; error = None }

  let create_empty ~id = create_with_result ~id None

  let create ~id ~start ~stop ~path =
    let uri ~path =
      try Some (Path.real_path path |> Path.uri) with
      | Unix.Unix_error _ -> None
    in
    let result =
      uri ~path
      >>| (fun uri -> { Location.uri; range = Range.create ~start ~stop })
      |> Option.to_list
    in
    create_with_result ~id (Some result)
end

module HoverResponse = struct
  include Types.HoverResponse

  type hover_result = {
    location: Ast.Location.t;
    contents: string;
  }

  let create ~id ~result =
    {
      jsonrpc = "2.0";
      id;
      result =
        (result
        >>| fun { location = { Ast.Location.start; stop; _ }; contents } ->
        {
          HoverResult.contents = { language = "python"; value = contents };
          range = Some (Range.create ~start ~stop);
        });
      error = None;
    }


  let create_empty ~id = create ~id ~result:None
end

module CodeActionResponse = struct
  include Types.CodeActionResponse

  let create ~id ~code_actions = { jsonrpc = "2.0"; id; result = Some code_actions; error = None }

  let create_empty ~id = create ~id ~code_actions:[]
end

module RageResponse = struct
  include Types.RageResponse

  let create ~items ~id = { jsonrpc = "2.0"; id; result = Some items; error = None }
end

let format_message string =
  let length = String.length string in
  Format.sprintf "Content-Length: %d\r\n\r\n%s" length string


let to_message json = format_message (Yojson.Safe.to_string json)

let write_message channel json =
  Log.log
    ~section:`Server
    "LSP message sent:@.%s@."
    (Yojson.Safe.prettify (Yojson.Safe.to_string json));
  Out_channel.output_string channel (to_message json);
  Out_channel.flush channel


let write_adapter_message channel message =
  Log.log ~section:`Server "LSP message sent to adapter:@.%s@" message;
  Out_channel.output_string channel (format_message message);
  Out_channel.flush channel


let read_message channel =
  let parse_content_length line =
    String.chop_prefix ~prefix:"Content-Length:" line
    >>| String.lstrip
    >>= fun length ->
    try Some (Int.of_string length) with
    | Failure _ -> None
  in
  let read_content length =
    let content_buffer = Buffer.create 10 in
    let rec skip_header () =
      match In_channel.input_line channel with
      | Some line when not (String.is_empty line) -> skip_header ()
      | other -> other
    in
    skip_header () (* ignore any other header lines *)
    >>= fun _ ->
    In_channel.input_buffer channel content_buffer ~len:length
    >>| fun _ -> Buffer.contents content_buffer
  in
  let check_end_of_file = function
    | None -> raise End_of_file
    | input -> input
  in
  In_channel.input_line channel
  |> check_end_of_file
  >>= parse_content_length
  |> function
  | None -> None (* if we can't parse header, but it wasn't because of EOF, return None *)
  | Some content_length ->
      read_content content_length
      |> check_end_of_file
      >>| fun json_string ->
      Log.log ~section:`Server "LSP message received:@.%s@." (Yojson.Safe.prettify json_string);
      Yojson.Safe.from_string json_string
