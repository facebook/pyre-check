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


type type_query_request =
  | Attributes of Expression.t
  | Join of Expression.t * Expression.t
  | LessOrEqual of Expression.t * Expression.t
  | Meet of Expression.t * Expression.t
  | Methods of Expression.t
  | NormalizeType of Expression.t
  | Superclasses of Expression.t
  | TypeAtLocation of Location.Instantiated.t
[@@deriving eq, show]


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

  val origin_name: origin -> string

  val flatten: t list -> t list

  val name: t -> string
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
