(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open PyreCommand
open LanguageServer.Types
open LanguageServer.Protocol
open LanguageServer.RequestParser
open Pyre

module Parallel = Hack_parallel.Std


let files context =
  let root = Path.create_absolute Filename.temp_dir_name in
  let relative =
    bracket_tmpfile ~suffix:".py" context
    |> fst
    |> Path.create_absolute
    |> (fun path -> Path.get_relative_to_root ~root ~path)
    |> (fun relative -> Option.value_exn relative)
  in
  let absolute =
    Path.create_relative ~root ~relative
    |> Path.show
  in
  root, relative, absolute


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
    | Error location ->
        Log.error "Parse error at location: %s" location;
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
  |};
  assert_parses {|
  {
    "jsonrpc": "2.0",
    "id": 0,
    "method": "initialize",
    "params": {
      "processId": 2160986,
      "rootPath": "/repo/path",
      "rootUri": "file:///repo/path",
      "capabilities": {
        "workspace": {
          "applyEdit": true,
          "workspaceEdit": { "documentChanges": true },
          "didChangeConfiguration": { "dynamicRegistration": false },
          "didChangeWatchedFiles": { "dynamicRegistration": true },
          "symbol": { "dynamicRegistration": false },
          "executeCommand": { "dynamicRegistration": false }
        },
        "textDocument": {
          "synchronization": {
            "dynamicRegistration": false,
            "willSave": false,
            "willSaveWaitUntil": false,
            "didSave": true
          },
          "completion": {
            "dynamicRegistration": false,
            "completionItem": { "snippetSupport": true }
          },
          "hover": { "dynamicRegistration": false },
          "signatureHelp": { "dynamicRegistration": false },
          "references": { "dynamicRegistration": false },
          "documentHighlight": { "dynamicRegistration": false },
          "documentSymbol": { "dynamicRegistration": false },
          "formatting": { "dynamicRegistration": false },
          "rangeFormatting": { "dynamicRegistration": false },
          "onTypeFormatting": { "dynamicRegistration": false },
          "definition": { "dynamicRegistration": false },
          "codeAction": { "dynamicRegistration": false },
          "codeLens": { "dynamicRegistration": false },
          "documentLink": { "dynamicRegistration": false },
          "rename": { "dynamicRegistration": false }
        },
        "window": {
          "status": { "dynamicRegistration": false },
          "progress": { "dynamicRegistration": false },
          "actionRequired": { "dynamicRegistration": false }
        }
      },
      "initializationOptions": {},
      "trace": "verbose"
    }
  }
  |}


let test_initialize_response _ =
  assert_equal
    (InitializeResponse.default 1
     |> InitializeResponse.to_yojson
     |> Yojson.Safe.sort)
    ({|
      {
        "id": 1,
        "jsonrpc": "2.0",
        "result": {
          "capabilities": {
            "definitionProvider": true,
            "hoverProvider": true,
            "rageProvider": true,
            "textDocumentSync": {
              "change": 0,
              "openClose": true,
              "save": { "includeText": false }
            }
          }
        }
      }
     |}
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
  let root, relative, absolute = files context in
  let linkname = relative ^ "link" in
  Unix.symlink ~src:absolute ~dst:((Path.absolute root) ^/ linkname);
  let message =
    DidSaveTextDocument.create ~root relative None
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
        "textDocument", `Assoc ["uri", `String (Format.sprintf "file://%s" absolute)];
      ];
    ]
    |> Yojson.Safe.pretty_to_string
  in
  let link_message =
    DidSaveTextDocument.create ~root linkname None
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
            version = None;
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
      ~id:1
      ~result:(
        Some {
          HoverResponse.location = Ast.Location.Instantiated.any;
          contents = "Hover response contents"
        })
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
        "range", `Assoc [
          "end", `Assoc [
            "character", `Int (-1);
            "line", `Int (-2);
          ];
          "start", `Assoc [
            "character", `Int (-1);
            "line", `Int (-2);
          ];
        ];
      ];
    ]
    |> Yojson.Safe.pretty_to_string
  in
  assert_equal ~printer:ident expected_message message


let test_request_parser context =
  let root, relative, absolute = files context in
  let open_message =
    {
      DidOpenTextDocument.jsonrpc = "2.0";
      method_ = "textDocument/didOpen";
      parameters = Some {
          DidOpenTextDocumentParams.textDocument = {
            TextDocumentItem.uri = "file://" ^ absolute;
            languageId = "python";
            version = 2;
            text = "file content goes here";
          };
        }
    }
    |> DidOpenTextDocument.to_yojson
  in
  let close_message =
    {
      DidCloseTextDocument.jsonrpc = "2.0";
      method_ = "textDocument/didClose";
      parameters = Some {
          DidCloseTextDocumentParams.textDocument = {
            TextDocumentIdentifier.uri = "file://" ^ absolute;
            version = None;
          };
        }
    }
    |> DidCloseTextDocument.to_yojson
  in
  let save_message =
    DidSaveTextDocument.create ~root relative None
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
            VersionedTextDocumentIdentifier.uri = "file://" ^ absolute;
            version = 1;
          };
          contentChanges = [];
        }
    }
    |> DidChangeTextDocument.to_yojson
  in

  let assert_parsed_request_equals message request =
    assert_equal
      ~cmp:(Option.equal Protocol.Request.equal)
      ~printer:(function | Some request -> Protocol.Request.show request | _ -> "None")
      request
      (parse
         ~root:(PyrePath.create_absolute Filename.temp_dir_name)
         message);
  in

  assert_parsed_request_equals
    open_message
    (Some
       (Protocol.Request.OpenDocument
          (Path.create_absolute absolute |> File.create)));
  assert_parsed_request_equals
    close_message
    (Some
       (Protocol.Request.CloseDocument
          (Path.create_absolute absolute |> File.create)));
  assert_parsed_request_equals
    save_message
    (Some
       (Protocol.Request.SaveDocument
          (Path.create_absolute absolute |> File.create)));
  assert_parsed_request_equals
    change_message
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
