(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis
open Pyre

module CompletionRequest = struct
  type t = {
    id: LanguageServer.Types.RequestId.t;
    path: Path.t;
    position: Location.position;
  }
  [@@deriving eq, show]
end

module DefinitionRequest = struct
  type t = {
    id: LanguageServer.Types.RequestId.t;
    path: Path.t;
    position: Location.position;
  }
  [@@deriving eq, show]
end

type client =
  | FileNotifier
  | Persistent
[@@deriving eq, show]

module TypeQuery = struct
  let json_socket_response response =
    `Assoc ["jsonrpc", `String "2.0"; "error", `Null; "result", Query.Response.to_yojson response]
end

module Request = struct
  type t =
    | ClientConnectionRequest of client
    | ClientExitRequest of client
    | ClientShutdownRequest of LanguageServer.Types.RequestId.t
    | CloseDocument of Path.t
    | CodeActionRequest of {
        id: LanguageServer.Types.RequestId.t;
        uri: LanguageServer.Types.DocumentUri.t;
        diagnostics: LanguageServer.Types.Diagnostic.t list;
        path: Path.t;
      }
    | CompletionRequest of CompletionRequest.t
    | DisplayTypeErrors of Path.t list
    | DocumentChange of File.t
    | ExecuteCommandRequest of {
        id: LanguageServer.Types.RequestId.t;
        arguments: LanguageServer.Types.CommandArguments.t list;
      }
    | GetDefinitionRequest of DefinitionRequest.t
    | HoverRequest of DefinitionRequest.t
    | InitializeRequest of LanguageServer.Types.RequestId.t
    | InitializedRequest
    | LanguageServerProtocolRequest of string
    | OpenDocument of Path.t
    | RageRequest of LanguageServer.Types.RequestId.t
    | SaveDocument of Path.t
    | ShowStatusRequest of LanguageServer.Types.ShowStatusParameters.t
    | StopRequest
    | TypeCheckRequest of Path.t list
    | TypeQueryRequest of Query.Request.t
    | UnparsableQuery of {
        query: string;
        reason: string;
      }
  [@@deriving eq, show]

  type origin =
    | FileNotifier
    | JSONSocket of Unix.File_descr.t
    | NewConnectionSocket of Unix.File_descr.t
    | PersistentSocket of Unix.File_descr.t

  let origin_name = function
    | FileNotifier -> "File notifier"
    | JSONSocket _ -> "JSONSocket"
    | NewConnectionSocket _ -> "New connection"
    | PersistentSocket _ -> "Persistent client"


  let name = function
    | ClientConnectionRequest _ -> "ClientConnection"
    | ClientExitRequest _ -> "ClientExit"
    | ClientShutdownRequest _ -> "ClientConnection"
    | CloseDocument _ -> "CloseDocument"
    | CodeActionRequest _ -> "CodeAction"
    | CompletionRequest _ -> "Completion"
    | DisplayTypeErrors _ -> "DisplayTypeErrors"
    | DocumentChange _ -> "DocumentChange"
    | ExecuteCommandRequest _ -> "ExecuteCommandRequest"
    | GetDefinitionRequest _ -> "GetDefinition"
    | HoverRequest _ -> "Hover"
    | InitializeRequest _ -> "Initialize"
    | InitializedRequest -> "Initialized"
    | LanguageServerProtocolRequest _ -> "LanguageServerProtocol"
    | OpenDocument _ -> "OpenDocument"
    | RageRequest _ -> "Rage"
    | SaveDocument _ -> "SaveDocument"
    | ShowStatusRequest _ -> "ShowStatusRequest"
    | StopRequest -> "Stop"
    | TypeCheckRequest [] -> "TypeCheck"
    | TypeCheckRequest _ -> "IncrementalCheck"
    | TypeQueryRequest _ -> "TypeQuery"
    | UnparsableQuery _ -> "UnparsableQuery"
end

type response =
  | ClientConnectionResponse of client
  | ClientExitResponse of client
  | GetDefinitionResponse of Location.WithPath.t option
  | HoverResponse of Location.t option
  | LanguageServerProtocolResponse of string
  | ServerUuidResponse of string
  | StopResponse
  | TypeCheckResponse of AnalysisError.Instantiated.t list
  | TypeQueryResponse of Query.Response.t
[@@deriving eq, show]
