(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open LanguageServer.Types
open LanguageServer.Protocol
open LanguageServer.RequestParser
open Pyre

module Parallel = Hack_parallel.Std
module Protocol = ServerProtocol


let test_language_server_protocol_message_format _ =
  let module GenericNotification =
    NotificationMessage.Make(struct
      type t
      let to_yojson _ = `Null
      let of_yojson _ = Error ""
    end) in
  let message =
    {
      GenericNotification.jsonrpc = "2.0";
      method_ = "foo";
      parameters = None
    }
    |> GenericNotification.to_yojson
    |> Yojson.Safe.sort
    |> to_message
  in
  let message_expect =
    "Content-Length: 32\r\n\
     \r\n\
     {\"jsonrpc\":\"2.0\",\"method\":\"foo\"}"
    |> Test.trim_extra_indentation
  in
  assert_equal
    ~printer:ident
    ~cmp:String.equal
    message_expect
    message


let test_initialize_request_parses _ =
  let assert_parses serialized =
    let request =
      Yojson.Safe.from_string serialized
      |> InitializeRequest.of_yojson
    in
    match request with
    | Ok _ ->
        ()
    | _ ->
        Test.assert_unreached ()
  in
  assert_parses {|
  {
     "jsonrpc": "2.0",
     "id": 0,
     "method": "initialize",
     "params": {
       "processId": 25669,
       "rootPath": "/Users/sinancepel/test",
       "rootUri": "file:///Users/sinancepel/test",
       "capabilities": {
         "workspace": {
           "applyEdit": true,
           "didChangeConfiguration": { "dynamicRegistration": true },
           "didChangeWatchedFiles": { "dynamicRegistration": true },
           "symbol": { "dynamicRegistration": true },
           "executeCommand": { "dynamicRegistration": true },
           "workspaceFolders": true,
           "configuration": true
         },
         "textDocument": {
           "synchronization": {
             "dynamicRegistration": true,
             "willSave": true,
             "willSaveWaitUntil": true,
             "didSave": true
           },
           "completion": {
             "dynamicRegistration": true,
             "completionItem": {
               "snippetSupport": true,
               "commitCharactersSupport": true
             }
           },
           "hover": { "dynamicRegistration": true },
           "signatureHelp": { "dynamicRegistration": true },
           "definition": { "dynamicRegistration": true },
           "references": { "dynamicRegistration": true },
           "documentHighlight": { "dynamicRegistration": true },
           "documentSymbol": { "dynamicRegistration": true },
           "codeAction": { "dynamicRegistration": true },
           "codeLens": { "dynamicRegistration": true },
           "formatting": { "dynamicRegistration": true },
           "rangeFormatting": { "dynamicRegistration": true },
           "onTypeFormatting": { "dynamicRegistration": true },
           "rename": { "dynamicRegistration": true },
           "documentLink": { "dynamicRegistration": true }
         }
       },
       "trace": "off",
       "workspaceFolders": [
         { "uri": "file:///Users/sinancepel/test", "name": "test" }
       ]
     }
  }
  |}


let test_initialize_response _ =
  assert_equal
    (InitializeResponse.default 1
     |> InitializeResponse.to_yojson
     |> Yojson.Safe.sort)
    (
      Format.sprintf
     {|
      {
        "id": 1,
        "jsonrpc": "2.0",
        "result": {
          "capabilities": {
            "definitionProvider": true,
            "hoverProvider": %s,
            "rageProvider": true,
            "textDocumentSync": { "change": 0, "openClose": true }
          }
        }
      }
     |}
     (Bool.to_string Experimental.type_hover_support_enabled)
     |> Yojson.Safe.from_string)


let test_language_server_protocol_read_message context =
  let set_up _ =
    Unix.pipe ()
    |> fun (in_, out_) ->
    let in_ = Unix.in_channel_of_descr in_ in
    let out_ = Unix.out_channel_of_descr out_ in
    in_, out_
  in

  let tear_down (in_,out_) _ =
    In_channel.close in_;
    Out_channel.close out_
  in

  let in_channel, out_channel = bracket set_up tear_down context in

  let message =
    "Content-Length: 32\r\n\
     \r\n\
     {\"jsonrpc\":\"2.0\",\"method\":\"foo\"}"
  in

  Out_channel.output_string out_channel message;
  Out_channel.flush out_channel;

  let actual =
    read_message in_channel
    >>| Yojson.Safe.to_string
  in

  let expected =
    `Assoc
      [
        ("jsonrpc", `String "2.0");
        ("method", `String "foo");
      ]
    |> Option.some
    >>| Yojson.Safe.to_string
  in

  assert_equal
    ~cmp:(Option.equal String.equal)
    expected
    actual


let test_language_server_protocol_deserialize_request _ =
  let module Request = RequestMessage.Make(struct
      type t = unit
      let of_yojson _ = Ok ()
    end) in

  let message =
    {|
      {
        "jsonrpc": "2.0",
        "id": 0,
        "method": "initialize",
        "params": {}
      }
    |}
    |> Test.trim_extra_indentation
    |> String.strip
  in

  let expected: Request.t =
    let open Request in
    {
      jsonrpc = "2.0";
      id = 0;
      method_ = "initialize";
      parameters = Some ();
    }
  in
  match Request.of_yojson (Yojson.Safe.from_string message) with
  | Ok actual ->
      assert_equal actual expected
  | Error _ ->
      assert_failure "Could not parse request of JSON"


let test_show_message_notification _ =
  let message =
    ShowMessage.create ShowMessageParams.ErrorMessage "error"
    |> ShowMessage.to_yojson
    |> Yojson.Safe.sort
    |> Yojson.Safe.pretty_to_string
  in
  let expected_message =
    `Assoc [
      "jsonrpc", `String "2.0";
      "method", `String "window/showMessage";
      "params", `Assoc [
        "message", `String "error";
        "type", `Int 1;
      ];
    ]
    |> Yojson.Safe.pretty_to_string
  in
  assert_equal ~printer:ident message expected_message


let test_did_save_notification context =
  let filename, _ = bracket_tmpfile ~suffix:".py" context in
  let linkname = filename ^ "link" in
  Unix.symlink ~src:filename ~dst:linkname;
  let message =
    DidSaveTextDocument.create ~root:(Path.create_absolute "/tmp") filename None
    |> Or_error.ok_exn
    |> DidSaveTextDocument.to_yojson
    |> Yojson.Safe.sort
    |> Yojson.Safe.pretty_to_string
  in
  let expected_message =
    `Assoc [
      "jsonrpc", `String "2.0";
      "method", `String "textDocument/didSave";
      "params", `Assoc [
        "text", `Null;
        "textDocument", `Assoc ["uri", `String (Format.sprintf "file://%s" filename)];
      ];
    ]
    |> Yojson.Safe.pretty_to_string
  in
  let link_message =
    DidSaveTextDocument.create ~root:(Path.create_absolute "/tmp") linkname None
    |> Or_error.ok_exn
    |> DidSaveTextDocument.to_yojson
    |> Yojson.Safe.sort
    |> Yojson.Safe.pretty_to_string
  in
  assert_equal ~printer:ident message expected_message;
  assert_equal ~printer:ident link_message expected_message


let test_language_server_definition_response _ =
  let message =
    TextDocumentDefinitionResponse.create
      ~root:(Path.current_working_directory ())
      ~id:1
      ~location:None
    |> TextDocumentDefinitionResponse.to_yojson
    |> Yojson.Safe.sort
    |> Yojson.Safe.pretty_to_string
  in
  let expected_message =
    `Assoc [
      "id", `Int 1;
      "jsonrpc", `String "2.0";
      "result", `List [];
    ]
    |> Yojson.Safe.pretty_to_string
  in
  assert_equal ~printer:ident expected_message message


let test_language_server_hover_request _ =
  let message =
    {|
      {
        "jsonrpc": "2.0",
        "id": 0,
        "method": "textDocument/hover",
        "params": {
          "textDocument": {
            "uri": "file:///some/uri.py"
          },
          "position": { "line": 3, "character": 5 }
        }
      }
    |}
    |> Test.trim_extra_indentation
    |> String.strip
  in

  let expected: HoverRequest.t =
    let open HoverRequest in
    let open TextDocumentPositionParams in
    let open TextDocumentIdentifier in
    let open Position in
    {
      jsonrpc = "2.0";
      id = 0;
      method_ = "textDocument/hover";
      parameters = Some {
          textDocument = {
            uri = "file:///some/uri.py";
          };
          position = {
            line = 3;
            character = 5;
          };
        };
    }
  in

  match HoverRequest.of_yojson (Yojson.Safe.from_string message) with
  | Ok actual ->
      assert_equal actual expected
  | Error _ ->
      assert_failure "Could not parse request of JSON"


let test_language_server_hover_response _ =
  let message =
    HoverResponse.create
      ~contents:"Hover response contents"
      ~id:1
      ~location:None
    |> HoverResponse.to_yojson
    |> Yojson.Safe.sort
    |> Yojson.Safe.pretty_to_string
  in
  let expected_message =
    `Assoc [
      "id", `Int 1;
      "jsonrpc", `String "2.0";
      "result", `Assoc [
        "contents", `String "Hover response contents";
        "range", `Null;
      ];
    ]
    |> Yojson.Safe.pretty_to_string
  in
  assert_equal ~printer:ident expected_message message


let test_request_parser context =
  let filename, _ = bracket_tmpfile ~suffix:".py" context in
  let save_message =
    DidSaveTextDocument.create ~root:(Path.create_absolute "/tmp") filename None
    |> Or_error.ok_exn
    |> DidSaveTextDocument.to_yojson
    |> Yojson.Safe.sort
  in
  let change_message =
    {
      DidChangeTextDocument.jsonrpc = "2.0";
      method_ = "textDocument/didChange";
      parameters = Some {
          DidChangeTextDocumentParams.textDocument = {
            VersionedTextDocumentIdentifier.uri = "file://" ^ (Filename.realpath filename);
            version = 1;
          };
          contentChanges = [];
        }
    }
    |> DidChangeTextDocument.to_yojson
  in

  assert_equal
    ~cmp:(Option.equal Protocol.Request.equal)
    (parse
       ~root:(PyrePath.create_absolute "/tmp")
       ~check_on_save:true
       save_message)
    (Some
       (Protocol.Request.TypeCheckRequest
          {
            Protocol.TypeCheckRequest.update_environment_with =
              [File.create (Path.create_absolute filename)];
            check = [File.create (Path.create_absolute filename)];
          }));
  assert_equal
    ~cmp:(Option.equal Protocol.Request.equal)
    (parse
       ~root:(PyrePath.create_absolute "/tmp")
       ~check_on_save:true
       change_message)
    None

let () =
  Log.initialize_for_tests ();
  "language_server">:::
  [
    "language_server_protocol_message_format">::test_language_server_protocol_message_format;
    "language_server_protocol_read_message">::test_language_server_protocol_read_message;
    "language_server_definition_response">::test_language_server_definition_response;
    "language_server_hover_request">::test_language_server_hover_request;
    "language_server_hover_response">::test_language_server_hover_response;
    "language_server_protocol_deserialize_request">::
    test_language_server_protocol_deserialize_request;
    "initialize_request_parses">::test_initialize_request_parses;
    "initialize_response">::test_initialize_response;
    "show_message_notification">::test_show_message_notification;
    "did_save_notification">::test_did_save_notification;
    "request_parser">::test_request_parser;
  ]
  |> run_test_tt_main
