(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Analysis
open Expression


module DefinitionRequest = struct
  type t = {
    id: int;
    file: File.t;
    position: Location.position;
  }
  [@@deriving eq, show]
end


type client =
  | FileNotifier
  | Persistent
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


module TypeQuery = struct
  type request =
    | Attributes of Access.t
    | Join of Access.t * Access.t
    | LessOrEqual of Access.t * Access.t
    | Meet of Access.t * Access.t
    | Methods of Access.t
    | NormalizeType of Access.t
    | SaveServerState of Pyre.Path.t
    | Signature of Access.t
    | Superclasses of Access.t
    | Type of Expression.t
    | TypeAtPosition of {
        file: File.t;
        position: Location.position;
      }
    | TypesInFile of File.t
  [@@deriving eq, show]


  type attribute = {
    name: string;
    annotation: Type.t;
  }
  [@@deriving eq, show, to_yojson]

  type method_representation = {
    name: string;
    parameters: Type.t list;
    return_annotation: Type.t;
  }
  [@@deriving eq, show, to_yojson]

  type found_parameter = {
    parameter_name: string;
    annotation: Type.t option;
  }
  [@@deriving eq, show, to_yojson]

  type found_signature = {
    return_type: Type.t option;
    parameters: found_parameter list;
  }
  [@@deriving eq, show, to_yojson]

  type type_at_location = {
    location: Location.Instantiated.t;
    annotation: Type.t;
  }
  [@@deriving eq, show, to_yojson]

  type base_response =
    | Boolean of bool
    | FoundAttributes of attribute list
    | FoundMethods of method_representation list
    | FoundSignature of found_signature list
    | Success of unit
    | Superclasses of Type.t list
    | Type of Type.t
    | TypeAtLocation of type_at_location
    | TypesAtLocations of type_at_location list
  [@@deriving eq, show]

  let base_response_to_yojson = function
    | Boolean boolean ->
        `Assoc ["boolean", `Bool boolean]
    | FoundAttributes attributes ->
        `Assoc ["attributes", `List (List.map attributes ~f:attribute_to_yojson)]
    | FoundMethods methods ->
        `Assoc ["methods", `List (List.map methods ~f:method_representation_to_yojson)]
    | FoundSignature signatures ->
        `Assoc ["signature", `List (List.map signatures ~f:found_signature_to_yojson)]
    | Success () ->
        `Assoc []
    | Superclasses classes ->
        `Assoc ["superclasses", `List (List.map classes ~f:Type.to_yojson)]
    | Type annotation ->
        `Assoc ["type", Type.to_yojson annotation]
    | TypeAtLocation annotation ->
        type_at_location_to_yojson annotation
    | TypesAtLocations annotations ->
        `Assoc ["types", `List (List.map annotations ~f:type_at_location_to_yojson)]

  type response =
    | Response of base_response
    | Error of string
  [@@deriving eq, show]

  let response_to_yojson = function
    | Response base_response ->
        `Assoc ["response", base_response_to_yojson base_response]
    | Error message ->
        `Assoc ["error", `String message]
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
    | TypeQueryRequest of TypeQuery.request
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
  | TypeQueryResponse of TypeQuery.response
  | StopResponse
  | GetDefinitionResponse of Location.Instantiated.t option
  | HoverResponse of Location.Instantiated.t option
[@@deriving eq, show]
