(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open LanguageServer
open Types
open Protocol
open Pyre
open Server
open CommandTest

let int_request_id id = LanguageServer.Types.RequestId.Int id

let string_request_id id = LanguageServer.Types.RequestId.String id

let test_language_server_protocol_message_format _ =
  let module GenericNotification = NotificationMessage.Make (struct
    type t

    let to_yojson _ = `Null

    let of_yojson _ = Error ""
  end)
  in
  let message =
    { GenericNotification.jsonrpc = "2.0"; method_ = "foo"; parameters = None }
    |> GenericNotification.to_yojson
    |> Yojson.Safe.sort
    |> to_message
  in
  let message_expect =
    "Content-Length: 32\r\n\r\n{\"jsonrpc\":\"2.0\",\"method\":\"foo\"}"
    |> Test.trim_extra_indentation
  in
  assert_equal ~printer:ident ~cmp:String.equal message_expect message


let test_initialize_request_parses _ =
  let assert_parses serialized =
    let request = Yojson.Safe.from_string serialized |> InitializeRequest.of_yojson in
    match request with
    | Ok _ -> ()
    | Error location ->
        Log.dump "Parse error at location: %s" location;
        Test.assert_unreached ()
  in
  assert_parses
    {|
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
  assert_parses
    {|
  {
    "jsonrpc": "2.0",
    "id": "abcd",
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
            "dynamicRegistration": false
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
  |};
  assert_parses
    {|
  {
   "jsonrpc": "2.0",
   "id": 0,
   "method": "initialize",
   "params": {
     "processId": null,
     "rootPath": "/test/directory",
     "rootUri": "file:///test/directory",
     "capabilities": {
       "workspace": {
         "applyEdit": true,
         "workspaceEdit": { "documentChanges": true },
         "didChangeConfiguration": { "dynamicRegistration": true },
         "didChangeWatchedFiles": { "dynamicRegistration": true },
         "symbol": {
           "dynamicRegistration": true,
           "symbolKind": {
             "valueSet": [
               1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
               19, 20, 21, 22, 23, 24, 25, 26
             ]
           }
         },
         "executeCommand": { "dynamicRegistration": true },
         "configuration": true,
         "workspaceFolders": true
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
           "contextSupport": true,
           "completionItem": {
             "snippetSupport": true,
             "commitCharactersSupport": true,
             "documentationFormat": [ "markdown", "plaintext" ]
           },
           "completionItemKind": {
             "valueSet": [
               1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
               19, 20, 21, 22, 23, 24, 25
             ]
           }
         },
         "hover": {
           "dynamicRegistration": true,
           "contentFormat": [ "markdown", "plaintext" ]
         },
         "signatureHelp": {
           "dynamicRegistration": true,
           "signatureInformation": {
             "documentationFormat": [ "markdown", "plaintext" ]
           }
         },
         "definition": { "dynamicRegistration": true },
         "references": { "dynamicRegistration": true },
         "documentHighlight": { "dynamicRegistration": true },
         "documentSymbol": {
           "dynamicRegistration": true,
           "symbolKind": {
             "valueSet": [
               1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
               19, 20, 21, 22, 23, 24, 25, 26
             ]
           }
         },
         "codeAction": { "dynamicRegistration": true },
         "codeLens": { "dynamicRegistration": true },
         "formatting": { "dynamicRegistration": true },
         "rangeFormatting": { "dynamicRegistration": true },
         "onTypeFormatting": { "dynamicRegistration": true },
         "rename": { "dynamicRegistration": true },
         "documentLink": { "dynamicRegistration": true },
         "typeDefinition": { "dynamicRegistration": true },
         "implementation": { "dynamicRegistration": true },
         "colorProvider": { "dynamicRegistration": true }
       }
     },
     "trace": "off",
     "workspaceFolders": [
       {
         "uri": "file:///test/directory",
         "name": "test"
       }
     ]
   }
  }
  |};
  assert_parses
    {|
  {
   "jsonrpc": "2.0",
   "id": "961810f9-ed94-4044-a7c8-82b4135925a7",
   "method": "initialize",
   "params": {
     "processId": null,
     "rootPath": "/test/directory",
     "rootUri": "file:///test/directory",
     "capabilities": {
       "workspace": {
         "applyEdit": true,
         "workspaceEdit": {
           "documentChanges": true,
           "resourceOperations": [ "create", "rename", "delete" ],
           "failureHandling": "textOnlyTransactional"
         },
         "didChangeConfiguration": { "dynamicRegistration": true },
         "didChangeWatchedFiles": { "dynamicRegistration": true },
         "symbol": {
           "dynamicRegistration": true,
           "symbolKind": {
             "valueSet": [
               1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
               19, 20, 21, 22, 23, 24, 25, 26
             ]
           }
         },
         "executeCommand": { "dynamicRegistration": true },
         "configuration": true,
         "workspaceFolders": true
       },
       "textDocument": {
         "publishDiagnostics": { "relatedInformation": true },
         "synchronization": {
           "dynamicRegistration": true,
           "willSave": true,
           "willSaveWaitUntil": true,
           "didSave": true
         },
         "completion": {
           "dynamicRegistration": true,
           "contextSupport": true,
           "completionItem": {
             "snippetSupport": true,
             "commitCharactersSupport": true,
             "documentationFormat": [ "markdown", "plaintext" ],
             "deprecatedSupport": true,
             "preselectSupport": true
           },
           "completionItemKind": {
             "valueSet": [
               1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
               19, 20, 21, 22, 23, 24, 25
             ]
           }
         },
         "hover": {
           "dynamicRegistration": true,
           "contentFormat": [ "markdown", "plaintext" ]
         },
         "signatureHelp": {
           "dynamicRegistration": true,
           "signatureInformation": {
             "documentationFormat": [ "markdown", "plaintext" ]
           }
         },
         "definition": { "dynamicRegistration": true },
         "references": { "dynamicRegistration": true },
         "documentHighlight": { "dynamicRegistration": true },
         "documentSymbol": {
           "dynamicRegistration": true,
           "symbolKind": {
             "valueSet": [
               1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
               19, 20, 21, 22, 23, 24, 25, 26
             ]
           },
           "hierarchicalDocumentSymbolSupport": true
         },
         "codeAction": {
           "dynamicRegistration": true,
           "codeActionLiteralSupport": {
             "codeActionKind": {
               "valueSet": [
                 "", "quickfix", "refactor", "refactor.extract",
                 "refactor.inline", "refactor.rewrite", "source",
                 "source.organizeImports"
               ]
             }
           }
         },
         "codeLens": { "dynamicRegistration": true },
         "formatting": { "dynamicRegistration": true },
         "rangeFormatting": { "dynamicRegistration": true },
         "onTypeFormatting": { "dynamicRegistration": true },
         "rename": { "dynamicRegistration": true, "prepareSupport": true },
         "documentLink": { "dynamicRegistration": true },
         "typeDefinition": { "dynamicRegistration": true },
         "implementation": { "dynamicRegistration": true },
         "colorProvider": { "dynamicRegistration": true },
         "foldingRange": {
           "dynamicRegistration": true,
           "rangeLimit": 5000,
           "lineFoldingOnly": true
         }
       },
       "window": { "status": { "dynamicRegistration": false } }
     },
     "trace": "verbose",
     "workspaceFolders": [
       {
         "uri":
           "file:///test/directory",
         "name": "test"
       }
     ]
   }
  }
  |};

  (* Handle non-existent fields. *)
  assert_parses
    {|
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
          "zorpThree": true
        },
        "textDocument": {
          "zorp": true
        },
        "window": {
          "zorpTwo": true
        },
        "unknownCapability": {}
      },
      "initializationOptions": {},
      "trace": "verbose"
    }
  }
  |}


let test_initialize_response _ =
  assert_equal
    ( InitializeResponse.default (int_request_id 1)
    |> InitializeResponse.to_yojson
    |> Yojson.Safe.sort )
    ( {|
      {
        "id": 1,
        "jsonrpc": "2.0",
        "result": {
          "capabilities": {
            "codeActionProvider": { "codeActionKind": [ "refactor.rewrite" ] },
            "definitionProvider": true,
            "executeCommandProvider": { "commands": [ "add_pyre_annotation" ] },
            "hoverProvider": true,
            "rageProvider": true,
            "textDocumentSync": {
              "change": 1,
              "openClose": true,
              "save": { "includeText": false }
            }
          }
        }
      }
     |}
    |> Yojson.Safe.from_string );
  assert_equal
    ( InitializeResponse.default (string_request_id "961810f9-ed94-4044-a7c8-82b4135925a7")
    |> InitializeResponse.to_yojson
    |> Yojson.Safe.sort )
    ( {|
      {
        "id": "961810f9-ed94-4044-a7c8-82b4135925a7",
        "jsonrpc": "2.0",
        "result": {
          "capabilities": {
            "codeActionProvider": { "codeActionKind": [ "refactor.rewrite" ] },
            "definitionProvider": true,
            "executeCommandProvider": { "commands": [ "add_pyre_annotation" ] },
            "hoverProvider": true,
            "rageProvider": true,
            "textDocumentSync": {
              "change": 1,
              "openClose": true,
              "save": { "includeText": false }
            }
          }
        }
      }
     |}
    |> Yojson.Safe.from_string )


let test_language_server_protocol_read_message context =
  let set_up _ =
    Unix.pipe ()
    |> fun (in_, out_) ->
    let in_ = Unix.in_channel_of_descr in_ in
    let out_ = Unix.out_channel_of_descr out_ in
    in_, out_
  in
  let tear_down (in_, out_) _ =
    In_channel.close in_;
    Out_channel.close out_
  in
  let in_channel, out_channel = bracket set_up tear_down context in
  let test_read message expected_output =
    Out_channel.output_string out_channel message;
    Out_channel.flush out_channel;
    let actual = read_message in_channel >>| Yojson.Safe.to_string in
    let expected = expected_output >>| Yojson.Safe.to_string in
    assert_equal ~cmp:(Option.equal String.equal) expected actual
  in
  let expected = Some (`Assoc ["jsonrpc", `String "2.0"; "method", `String "foo"]) in
  test_read "Content-Length: 32\r\n\r\n{\"jsonrpc\":\"2.0\",\"method\":\"foo\"}" expected;
  test_read
    "Content-Length: 32\r\n\
     Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n\
     \r\n\
     {\"jsonrpc\":\"2.0\",\"method\":\"foo\"}"
    expected;
  test_read
    "Content-Length: 32\r\n\
     Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n\
     Another-Header: yes\r\n\
     One-More-Header: sure\r\n\
     \r\n\
     {\"jsonrpc\":\"2.0\",\"method\":\"foo\"}"
    expected;
  test_read "Content-Longness: 32\r\n\r\n{\"jsonrpc\":\"2.0\",\"method\":\"foo\"}" None;
  test_read "Content-Length: 123abc456\r\n\r\n{\"jsonrpc\":\"2.0\",\"method\":\"foo\"}" None


let test_language_server_protocol_read_message_eof context =
  let set_up _ =
    Unix.pipe ()
    |> fun (in_, out_) ->
    let in_ = Unix.in_channel_of_descr in_ in
    let out_ = Unix.out_channel_of_descr out_ in
    in_, out_
  in
  let tear_down (in_, out_) _ =
    In_channel.close in_;
    Out_channel.close out_
  in
  let test_eof_raised message =
    let in_channel, out_channel = bracket set_up tear_down context in
    Out_channel.output_string out_channel message;
    Out_channel.flush out_channel;
    Out_channel.close out_channel;

    (* EOF *)
    assert_raises End_of_file (fun _ -> read_message in_channel)
  in
  test_eof_raised "";
  test_eof_raised "Content-Length: 32\r\n";
  test_eof_raised "Content-Length: 32\r\n\r\n";
  test_eof_raised "Content-Length: 32\r\n\r\n{\"jsonrpc\":\"2.0\",\"method\"";
  let _ =
    (* full message should not raise error *)
    let in_channel, out_channel = bracket set_up tear_down context in
    let message = "Content-Length: 32\r\n\r\n{\"jsonrpc\":\"2.0\",\"method\":\"foo\"}" in
    Out_channel.output_string out_channel message;
    Out_channel.flush out_channel;
    Out_channel.close out_channel;

    (* EOF *)
    read_message in_channel
  in
  ()


let test_language_server_protocol_deserialize_request _ =
  let module Request = RequestMessage.Make (struct
    type t = unit

    let of_yojson _ = Ok ()
  end)
  in
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
  let expected : Request.t =
    let open Request in
    { jsonrpc = "2.0"; id = int_request_id 0; method_ = "initialize"; parameters = Some () }
  in
  match Request.of_yojson (Yojson.Safe.from_string message) with
  | Ok actual -> assert_equal actual expected
  | Error _ -> assert_failure "Could not parse request of JSON"


let test_show_message_notification _ =
  let message =
    ShowMessage.create ShowMessageParameters.ErrorMessage "error"
    |> ShowMessage.to_yojson
    |> Yojson.Safe.sort
    |> Yojson.Safe.pretty_to_string
  in
  let expected_message =
    `Assoc
      [ "jsonrpc", `String "2.0";
        "method", `String "window/showMessage";
        "params", `Assoc ["message", `String "error"; "type", `Int 1] ]
    |> Yojson.Safe.pretty_to_string
  in
  assert_equal ~printer:ident message expected_message


let test_did_save_notification context =
  let source_name = "test_did_save_notification.py" in
  let { ScratchServer.configuration = { Configuration.Analysis.local_root; _ }; _ } =
    ScratchServer.start ~context [source_name, ""]
  in
  let source_path = Path.create_relative ~root:local_root ~relative:source_name in
  let link_name = "test_did_save_notification_link.py" in
  let link_root = bracket_tmpdir context |> Path.create_absolute in
  let link_path = Path.create_relative ~root:link_root ~relative:link_name in
  Unix.symlink ~target:(Path.absolute source_path) ~link_name:(Path.absolute link_path);
  let message =
    DidSaveTextDocument.create ~root:local_root source_name None
    |> Or_error.ok_exn
    |> DidSaveTextDocument.to_yojson
    |> Yojson.Safe.sort
    |> Yojson.Safe.pretty_to_string
  in
  let expected_message =
    `Assoc
      [ "jsonrpc", `String "2.0";
        "method", `String "textDocument/didSave";
        ( "params",
          `Assoc
            [ ( "textDocument",
                `Assoc ["uri", `String (Format.sprintf "file://%s" (Path.absolute source_path))] )
            ] ) ]
    |> Yojson.Safe.pretty_to_string
  in
  let link_message =
    DidSaveTextDocument.create ~root:link_root link_name None
    |> Or_error.ok_exn
    |> DidSaveTextDocument.to_yojson
    |> Yojson.Safe.sort
    |> Yojson.Safe.pretty_to_string
  in
  assert_equal ~printer:ident message expected_message;
  assert_equal ~printer:ident link_message expected_message


let test_language_server_definition_response context =
  let open Ast.Location in
  let local_root = bracket_tmpdir context |> Path.create_absolute in
  let assert_response ~id ~location ~expected =
    let message =
      let response =
        match location with
        | Some { start; stop; path } ->
            let path = Path.create_relative ~root:local_root ~relative:path in
            TextDocumentDefinitionResponse.create ~id ~start ~stop ~path
        | None -> TextDocumentDefinitionResponse.create_empty ~id
      in
      TextDocumentDefinitionResponse.to_yojson response |> Yojson.Safe.sort
    in
    let expected = Yojson.Safe.sort expected in
    assert_equal ~printer:Yojson.Safe.pretty_to_string expected message
  in
  assert_response
    ~id:(int_request_id 1)
    ~location:None
    ~expected:(`Assoc ["id", `Int 1; "jsonrpc", `String "2.0"; "result", `List []]);
  assert_response
    ~id:(string_request_id "abcd")
    ~location:None
    ~expected:(`Assoc ["id", `String "abcd"; "jsonrpc", `String "2.0"; "result", `List []]);

  let touch path = File.create ~content:"" path |> File.write in
  let file = Path.create_relative ~root:local_root ~relative:"a.py" in
  touch file;
  let stub = Path.create_relative ~root:local_root ~relative:"b.pyi" in
  touch stub;
  assert_response
    ~id:(int_request_id 1)
    ~location:
      (Some { path = "a.py"; start = { line = 1; column = 0 }; stop = { line = 2; column = 0 } })
    ~expected:
      (`Assoc
        [ "jsonrpc", `String "2.0";
          "id", `Int 1;
          ( "result",
            `List
              [ `Assoc
                  [ "uri", `String (Format.sprintf "file://%s" (Path.absolute file));
                    ( "range",
                      `Assoc
                        [ "start", `Assoc ["line", `Int 0; "character", `Int 0];
                          "end", `Assoc ["line", `Int 1; "character", `Int 0] ] ) ] ] ) ]);
  assert_response
    ~id:(string_request_id "abcd")
    ~location:
      (Some { path = "b.pyi"; start = { line = 1; column = 0 }; stop = { line = 2; column = 0 } })
    ~expected:
      (`Assoc
        [ "jsonrpc", `String "2.0";
          "id", `String "abcd";
          ( "result",
            `List
              [ `Assoc
                  [ "uri", `String (Format.sprintf "file://%s" (Path.absolute stub));
                    ( "range",
                      `Assoc
                        [ "start", `Assoc ["line", `Int 0; "character", `Int 0];
                          "end", `Assoc ["line", `Int 1; "character", `Int 0] ] ) ] ] ) ])


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
  let expected : HoverRequest.t =
    let open HoverRequest in
    let open TextDocumentPositionParameters in
    let open TextDocumentIdentifier in
    let open Position in
    {
      jsonrpc = "2.0";
      id = int_request_id 0;
      method_ = "textDocument/hover";
      parameters =
        Some
          {
            textDocument = { uri = "file:///some/uri.py"; version = None };
            position = { line = 3; character = 5 };
          };
    }
  in
  match HoverRequest.of_yojson (Yojson.Safe.from_string message) with
  | Ok actual -> assert_equal actual expected
  | Error _ -> assert_failure "Could not parse request of JSON"


let test_language_server_hover_response _ =
  let message =
    HoverResponse.create
      ~id:(int_request_id 1)
      ~result:
        (Some
           {
             HoverResponse.location = Ast.Location.Instantiated.any;
             contents = "Hover response contents";
           })
    |> HoverResponse.to_yojson
    |> Yojson.Safe.sort
    |> Yojson.Safe.pretty_to_string
  in
  let expected_message =
    `Assoc
      [ "id", `Int 1;
        "jsonrpc", `String "2.0";
        ( "result",
          `Assoc
            [ ( "contents",
                `Assoc ["language", `String "python"; "value", `String "Hover response contents"] );
              ( "range",
                `Assoc
                  [ "end", `Assoc ["character", `Int (-1); "line", `Int (-2)];
                    "start", `Assoc ["character", `Int (-1); "line", `Int (-2)] ] ) ] ) ]
    |> Yojson.Safe.pretty_to_string
  in
  assert_equal ~printer:ident expected_message message


let test_request_parser context =
  (* Test setup *)
  let file_handle = "filename.py" in
  let symlink_handle = "symlink.py" in
  let stub_handle = "stub.pyi" in
  let {
    ScratchServer.configuration;
    state = { Server.State.symlink_targets_to_sources; _ } as state;
    _;
  }
    =
    ScratchServer.start ~context [file_handle, ""; symlink_handle, ""; stub_handle, ""]
  in
  let { Configuration.Analysis.local_root; _ } = configuration in
  let alternative_root = bracket_tmpdir context |> Path.create_absolute in
  let file_path = Path.create_relative ~root:local_root ~relative:file_handle in
  let stub_path = Path.create_relative ~root:local_root ~relative:stub_handle in
  let symlink_source = Path.create_relative ~root:local_root ~relative:symlink_handle in
  let symlink_target = Path.create_relative ~root:alternative_root ~relative:"target.py" in
  (* Hackiness alert: Since `ScratchServer.start` does not support creating symlinks under
     `local_root`, we have to create a placeholder non-symlink earlier upon server start, then
     remove the file here and replace it with a real symlink. *)
  File.write (File.create ~content:"" symlink_target);
  Unix.unlink (Path.absolute symlink_source);
  Unix.symlink ~target:(Path.absolute symlink_target) ~link_name:(Path.absolute symlink_source);
  Hashtbl.set symlink_targets_to_sources ~key:(Path.absolute symlink_target) ~data:symlink_source;

  let open_message path =
    {
      DidOpenTextDocument.jsonrpc = "2.0";
      method_ = "textDocument/didOpen";
      parameters =
        Some
          {
            DidOpenTextDocumentParameters.textDocument =
              {
                TextDocumentItem.uri = "file://" ^ Path.absolute path;
                languageId = "python";
                version = 2;
                text = "file content goes here";
              };
          };
    }
    |> DidOpenTextDocument.to_yojson
  in
  let close_message =
    {
      DidCloseTextDocument.jsonrpc = "2.0";
      method_ = "textDocument/didClose";
      parameters =
        Some
          {
            DidCloseTextDocumentParameters.textDocument =
              { TextDocumentIdentifier.uri = "file://" ^ Path.absolute file_path; version = None };
          };
    }
    |> DidCloseTextDocument.to_yojson
  in
  let save_message =
    let { Configuration.Analysis.local_root; _ } = configuration in
    let relative = "filename.py" in
    DidSaveTextDocument.create ~root:local_root relative None
    |> Or_error.ok_exn
    |> DidSaveTextDocument.to_yojson
    |> Yojson.Safe.sort
  in
  let change_message =
    {
      DidChangeTextDocument.jsonrpc = "2.0";
      method_ = "textDocument/didChange";
      parameters =
        Some
          {
            DidChangeTextDocumentParameters.textDocument =
              {
                VersionedTextDocumentIdentifier.uri = "file://" ^ Path.absolute file_path;
                version = 1;
              };
            contentChanges = [{ text = "changed source"; range = None; rangeLength = None }];
          };
    }
    |> DidChangeTextDocument.to_yojson
  in
  let update_message =
    {
      UpdateFiles.jsonrpc = "2.0";
      method_ = "updateFiles";
      parameters =
        Some
          {
            UpdateFilesParameters.files =
              [Path.absolute file_path; Path.absolute symlink_source; Path.absolute stub_path];
            invalidated = [];
          };
    }
    |> UpdateFiles.to_yojson
  in
  let display_type_errors_message =
    {
      LanguageServer.Types.DisplayTypeErrors.jsonrpc = "2.0";
      method_ = "displayTypeErrors";
      parameters =
        Some
          {
            LanguageServer.Types.DisplayTypeErrorsParameters.files =
              [Path.absolute file_path; Path.absolute symlink_source; Path.absolute stub_path];
          };
    }
    |> LanguageServer.Types.DisplayTypeErrors.to_yojson
  in
  let assert_parsed_request_equals message request =
    assert_equal
      ~cmp:(Option.equal Protocol.Request.equal)
      ~printer:(function
        | Some request -> Protocol.Request.show request
        | _ -> "None")
      request
      (Request.parse_lsp ~configuration ~state ~request:message)
  in
  assert_parsed_request_equals
    (open_message file_path)
    (Some (Protocol.Request.OpenDocument file_path));
  assert_parsed_request_equals
    (open_message symlink_source)
    (Some (Protocol.Request.OpenDocument symlink_source));
  assert_parsed_request_equals
    (open_message symlink_target)
    (Some (Protocol.Request.OpenDocument symlink_source));
  assert_parsed_request_equals close_message (Some (Protocol.Request.CloseDocument file_path));
  assert_parsed_request_equals save_message (Some (Protocol.Request.SaveDocument file_path));
  assert_parsed_request_equals
    change_message
    (Some (Protocol.Request.DocumentChange (File.create ~content:"changed source" file_path)));
  assert_parsed_request_equals
    update_message
    (Some (Protocol.Request.TypeCheckRequest [file_path; symlink_source; stub_path]));
  assert_parsed_request_equals
    display_type_errors_message
    (Some (Protocol.Request.DisplayTypeErrors [file_path; symlink_source; stub_path]))


let test_publish_diagnostics _ =
  let assert_json_equal ~expected ~actual =
    assert_equal
      ~printer:Yojson.Safe.pretty_to_string
      (Yojson.Safe.from_string expected)
      (PublishDiagnostics.to_yojson actual)
  in
  assert_json_equal
    ~expected:
      {|
     {
       "jsonrpc": "2.0",
       "method": "textDocument/publishDiagnostics",
       "params": { "uri": "file:///data/users/user/root/b.py", "diagnostics": [] }
     }
    |}
    ~actual:(PublishDiagnostics.clear_diagnostics_for_uri ~uri:"file:///data/users/user/root/b.py");
  assert_equal
    "uri"
    (PublishDiagnostics.uri (PublishDiagnostics.clear_diagnostics_for_uri ~uri:"uri"))


let () =
  "language_server"
  >::: [ "language_server_protocol_message_format" >:: test_language_server_protocol_message_format;
         "language_server_protocol_read_message" >:: test_language_server_protocol_read_message;
         "language_server_protocol_read_message_eof"
         >:: test_language_server_protocol_read_message_eof;
         "language_server_definition_response" >:: test_language_server_definition_response;
         "language_server_hover_request" >:: test_language_server_hover_request;
         "language_server_hover_response" >:: test_language_server_hover_response;
         "language_server_protocol_deserialize_request"
         >:: test_language_server_protocol_deserialize_request;
         "initialize_request_parses" >:: test_initialize_request_parses;
         "initialize_response" >:: test_initialize_response;
         "show_message_notification" >:: test_show_message_notification;
         "did_save_notification" >:: test_did_save_notification;
         "request_parser" >:: test_request_parser;
         "publish_diagnostics" >:: test_publish_diagnostics ]
  |> Test.run
