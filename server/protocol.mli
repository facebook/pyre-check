(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Analysis


module DefinitionRequest: sig
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


module TypeQuery: sig
  type request =
    | Attributes of Expression.t
    | Join of Expression.t * Expression.t
    | LessOrEqual of Expression.t * Expression.t
    | Meet of Expression.t * Expression.t
    | Methods of Expression.t
    | NormalizeType of Expression.t
    | Superclasses of Expression.t
    | TypeAtLocation of Location.Instantiated.t
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

  type base_response =
    | FoundAttributes of attribute list
    | FoundMethods of method_representation list
    | Type of Type.t
    | Superclasses of Type.t list
    | Boolean of bool
  [@@deriving eq, show, to_yojson]

  type response =
    | Response of base_response
    | Error of string
  [@@deriving eq, show, to_yojson]
end


module TypeCheckRequest: sig
  type t = {
    update_environment_with: File.t list;
    check: File.t list;
  }
  [@@deriving eq, show]

  val create: ?update_environment_with: File.t list -> ?check: File.t list -> unit -> t
end

module Request : sig
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

  val origin_name: origin -> string

  val flatten: t list -> t list

  val name: t -> string
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
