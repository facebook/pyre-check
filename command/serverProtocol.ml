(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Analysis


module DefinitionRequest = struct
  type t = {
    id: int;
    path: string;
    position: Location.position;
  }
  [@@deriving eq, show]
end


type client =
  | FileNotifier
  | Persistent
[@@deriving eq, show]


type type_query_request =
  | Join of Expression.t * Expression.t
  | LessOrEqual of Expression.t * Expression.t
  | Meet of Expression.t * Expression.t
  | Methods of Expression.t
  | NormalizeType of Expression.t
  | Superclasses of Expression.t
  | TypeAtLocation of Location.Instantiated.t
[@@deriving eq, show]


module TypeCheckRequest = struct
  type t = {
    update_environment_with: File.t list;
    check: File.t list;
  }
  [@@deriving eq, show]


  let create ?(update_environment_with = []) ?(check = []) () =
    { update_environment_with; check }
end


module Request = struct
  type t =
    | LanguageServerProtocolRequest of string
    | ClientConnectionRequest of client
    | ClientExitRequest of client
    | RageRequest of int
    | DisplayTypeErrors of File.t list
    | FlushTypeErrorsRequest
    | TypeCheckRequest of TypeCheckRequest.t
    | TypeQueryRequest of type_query_request
    | StopRequest
    | ClientShutdownRequest of int
    | GetDefinitionRequest of DefinitionRequest.t
    | HoverRequest of DefinitionRequest.t
    | OpenDocument of File.t
    | CloseDocument of File.t
    | SaveDocument of File.t
  [@@deriving eq, show]


  type origin =
    | PersistentSocket of Unix.File_descr.t
    | NewConnectionSocket of Unix.File_descr.t
    | FileNotifier
    | Background


  let origin_name = function
    | PersistentSocket _ -> "Persistent client"
    | NewConnectionSocket _ -> "New connection"
    | FileNotifier -> "File notifier"
    | Background -> "Background"


  let flatten requests =
    let incremental_requests, unrelated =
      let is_incremental_request = function
        | TypeCheckRequest _ ->
            true
        | _ ->
            false
      in
      List.partition_tf ~f:is_incremental_request requests
    in
    if List.is_empty incremental_requests then
      unrelated
    else
      let add_files (current_update, current_check) request =
        match request with
        | TypeCheckRequest { TypeCheckRequest.update_environment_with; check } ->
            Set.union current_update (File.Set.of_list update_environment_with),
            Set.union current_check (File.Set.of_list check)
        | _ ->
            current_update, current_check
      in
      let (update, check) =
        List.fold ~init:(File.Set.empty, File.Set.empty) ~f:add_files requests
      in
      TypeCheckRequest {
        TypeCheckRequest.update_environment_with = Set.to_list update;
        check = Set.to_list check;
      } :: unrelated


  let name = function
    | LanguageServerProtocolRequest _ -> "LanguageServerProtocol"
    | ClientConnectionRequest _ -> "ClientConnection"
    | ClientExitRequest _ -> "ClientExit"
    | RageRequest _ -> "Rage"
    | DisplayTypeErrors _ -> "DisplayTypeErrors"
    | FlushTypeErrorsRequest -> "FlushTypeErrors"
    | TypeCheckRequest { TypeCheckRequest.check = []; update_environment_with = [] } -> "TypeCheck"
    | TypeCheckRequest _ -> "IncrementalCheck"
    | TypeQueryRequest _ -> "TypeQuery"
    | StopRequest -> "Stop"
    | ClientShutdownRequest _ -> "ClientConnection"
    | GetDefinitionRequest _ -> "GetDefinition"
    | HoverRequest _ -> "Hover"
    | OpenDocument _ -> "OpenDocument"
    | CloseDocument _ -> "CloseDocument"
    | SaveDocument _ -> "SaveDocument"
end


type response =
  | LanguageServerProtocolResponse of string
  | ClientConnectionResponse of client
  | ClientExitResponse of client
  | TypeCheckResponse of (File.Handle.t * (Error.t list)) list
  | TypeQueryResponse of string
  | StopResponse
  | GetDefinitionResponse of Location.Instantiated.t option
  | HoverResponse of Location.Instantiated.t option
[@@deriving eq, show]
