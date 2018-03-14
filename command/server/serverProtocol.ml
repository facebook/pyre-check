(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis


module DefinitionRequest = struct
  type t = {
    id: int;
    path: string;
    position: Ast.Location.position;
  }
  [@@deriving eq, show]
end


type client =
  | FileNotifier
  | Persistent
[@@deriving eq, show]


type type_query_request =
  | LessOrEqual of Type.t * Type.t
  | Join of Type.t * Type.t
  | Meet of Type.t * Type.t
  | Superclasses of Type.t
[@@deriving eq, show]


module TypeCheckRequest = struct
  type t = {
    update_environment_with: File.t list;
    check: File.t list;
  }
  [@@deriving eq, show]


  let has_no_files = function
    | { update_environment_with = []; check = [] } ->
        true
    | _ ->
        false


  let create ?(update_environment_with = []) ?(check = []) () =
    { update_environment_with; check }


  let empty =
    { update_environment_with = []; check = [] }
end


module Request = struct
  type t =
    | LanguageServerProtocolRequest of string
    | ClientConnectionRequest of client
    | ClientExitRequest of client
    | RageRequest of int
    | ReinitializeStateRequest
    | DisplayTypeErrors of File.t list
    | TypeCheckRequest of TypeCheckRequest.t
    | TypeQueryRequest of type_query_request
    | StopRequest
    | ClientShutdownRequest of int
    | GetDefinitionRequest of DefinitionRequest.t
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
        | TypeCheckRequest { TypeCheckRequest.check; _ }
          when not (List.is_empty check) ->
            true
        | _ ->
            false
      in
      List.partition_tf ~f:is_incremental_request requests
    in
    if List.is_empty incremental_requests then
      unrelated
    else
      let add_files file_set request =
        match request with
        | TypeCheckRequest { TypeCheckRequest.check; _ } ->
            Set.union file_set (File.Set.of_list check)
        | _ -> file_set
      in
      TypeCheckRequest {
        TypeCheckRequest.check =
          List.fold ~init:File.Set.empty ~f:add_files incremental_requests
          |> File.Set.to_list;
        update_environment_with = [];
      } :: unrelated


  let name = function
    | LanguageServerProtocolRequest _ -> "LanguageServerProtocol"
    | ClientConnectionRequest _ -> "ClientConnection"
    | ClientExitRequest _ -> "ClientExit"
    | RageRequest _ -> "Rage"
    | ReinitializeStateRequest -> "ReinitializeState"
    | DisplayTypeErrors _ -> "DisplayTypeErrors"
    | TypeCheckRequest { TypeCheckRequest.check = []; update_environment_with = [] } -> "TypeCheck"
    | TypeCheckRequest _ -> "IncrementalCheck"
    | TypeQueryRequest _ -> "TypeQuery"
    | StopRequest -> "Stop"
    | ClientShutdownRequest _ -> "ClientConnection"
    | GetDefinitionRequest _ -> "GetDefinition"
end


type response =
  | LanguageServerProtocolResponse of string
  | ClientConnectionResponse of client
  | ClientExitResponse of client
  | TypeCheckResponse of (File.Handle.t * (Error.t list)) list
  | TypeQueryResponse of string
  | StopResponse
  | GetDefinitionResponse of Ast.Location.t option
[@@deriving eq, show]
