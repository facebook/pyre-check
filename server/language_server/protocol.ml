(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre
open Analysis
open Types


module Range = struct
  include Types.Range

  let create
      ~start:{ Ast.Location.line = start_line; column = start_column }
      ~stop:{ Ast.Location.line = stop_line; column = stop_column } =
    {
      start = { Position.line = start_line - 1; character = start_column };
      end_ = { Position.line = stop_line - 1; character = stop_column };
    }
end

module InitializeRequest = Types.InitializeRequest

module TextDocumentDefinitionRequest = Types.TextDocumentDefinitionRequest

module PublishDiagnostics = struct
  include Types.PublishDiagnostics

  let of_errors ~configuration handle errors =
    let diagnostic_of_error error =
      let { Ast.Location.start; stop; _ } =
        TypeCheck.Error.location error in
      Diagnostic.({
          range = Range.create ~start ~stop;
          severity = Some DiagnosticSeverity.Error;
          code = None;
          source = Some "Pyre";
          message = TypeCheck.Error.description error ~detailed:true;
        })
    in
    let failed_response =
      Format.asprintf "Valid path does not exist for %s." (File.Handle.show handle)
      |> Or_error.error_string
    in
    try
      let path =
        File.Handle.to_path ~configuration handle
      in
      match path with
      | Some path ->
          Ok {
            jsonrpc = "2.0";
            method_ = "textDocument/publishDiagnostics";
            parameters = Some {
                PublishDiagnosticsParams.uri =
                  path
                  |> Path.real_path
                  |> Path.uri;
                diagnostics = List.map ~f:diagnostic_of_error errors;
              };
          }
      | None ->
          failed_response
    with
    | Unix.Unix_error _ ->
        failed_response
end


module DidSaveTextDocument = struct
  include Types.DidSaveTextDocument


  let create ?(root = Path.current_working_directory ()) path content =
    try
      Ok {
        jsonrpc = "2.0";
        method_ = "textDocument/didSave";
        parameters = Some {
            DidSaveTextDocumentParams.textDocument = {
              TextDocumentIdentifier.uri =
                Path.create_relative ~root ~relative:path
                |> Path.real_path
                |> Path.uri;
              version = None;
            };
            text = content;
          }
      }
    with
    | Unix.Unix_error _ ->
        Format.sprintf "Valid path does not exist for file %s." path
        |> Or_error.error_string
end


module ShowMessage = struct
  include Types.ShowMessage

  let create messageType content =
    {
      jsonrpc = "2.0";
      method_ = "window/showMessage";
      parameters = Some {
          ShowMessageParams.messageType = ShowMessageParams.messageTypeNumber messageType;
          message = content;
        };
    }
end


module InitializeResponse = struct
  include Types.InitializeResponse

  let default id =
    let open TextDocumentSyncOptions in
    {
      jsonrpc = "2.0";
      id;
      result = Some InitializeResult.({
          capabilities = InitializeResult.ServerCapabilities.({
              text_document_sync = Some {
                  open_close = Some true;
                  change = Some (Kind.get_change Kind.None_);
                  will_save = None;
                  will_save_wait_until = None;
                  save = Some { SaveOptions.include_text = Some false };
                };
              hover_provider = Some true;
              completion_provider = None;
              signature_help_provider = None;
              definition_provider = Some true;
              references_provider = None;
              document_highlight_provider = None;
              document_symbol_provider = None;
              workspace_symbol_provider = None;
              code_action_provider = None;
              code_lens_provider = None;
              document_formatting_provider = None;
              document_range_formatting_provider = None;
              document_on_type_formatting_provider = None;
              rename_provider = None;
              document_link_provider = None;
              execute_command_provider = None;
              experimental = None;
              rage_provider = Some true;
            });
        });
      error = None;
    }
end


module ShutdownResponse = struct
  include Types.ShutdownResponse

  let default id =
    {
      jsonrpc = "2.0";
      id;
      result = None;
      error = None;
    }
end


module TextDocumentDefinitionResponse = struct
  include Types.TextDocumentDefinitionResponse

  let create ~configuration ~id ~location =
    let uri ~path =
      File.Handle.create path
      |> File.Handle.to_path ~configuration
      >>| Path.real_path
      >>| Path.uri
    in
    {
      jsonrpc = "2.0";
      id;
      result =
        Some
          (location
           >>= (fun { Ast.Location.start; stop; path } -> uri ~path
                 >>| fun uri -> { Location.uri; range = Range.create ~start ~stop })
           |> Option.to_list);
      error = None;
    }
end


module HoverResponse = struct
  include Types.HoverResponse

  type hover_result = {
    location: Ast.Location.Instantiated.t;
    contents: string;
  }

  let create ~id ~result =
    {
      jsonrpc = "2.0";
      id;
      result =
        result
        >>| (fun { location = { Ast.Location.start; stop; _ }; contents } ->
            {
              HoverResult.contents;
              range = Some (Range.create ~start ~stop);
            });
      error = None;
    }
end


module RageResponse = struct
  include Types.RageResponse

  let create ~items ~id =
    {
      jsonrpc = "2.0";
      id;
      result =
        Some items;
      error = None;
    }
end


let to_message json =
  let json_string = Yojson.Safe.to_string json in
  let length = String.length json_string in
  Format.sprintf
    "Content-Length: %d\r\n\
     \r\n\
     %s"
    length
    json_string


let write_message channel json =
  Log.info
    "Persistent client sends:@.%s@."
    (Yojson.Safe.prettify (Yojson.Safe.to_string json));
  Out_channel.output_string channel (to_message json);
  Out_channel.flush channel


let read_message channel =
  let parse_content_length line =
    String.chop_prefix ~prefix:"Content-Length:" line
    >>| String.lstrip
    >>| Int.of_string
  in

  let read_content length =
    let content_buffer = Buffer.create 10 in
    In_channel.input_line channel (* eat the new line *)
    >>= fun _ -> In_channel.input_buffer channel content_buffer ~len:length
    >>| fun _ -> Buffer.contents content_buffer
  in

  In_channel.input_line channel
  >>= parse_content_length
  >>= read_content
  >>| fun json_string ->
  Log.info "Persistent client received:@.%s@." (Yojson.Safe.prettify json_string);
  Yojson.Safe.from_string json_string
