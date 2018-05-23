(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis

module DefinitionRequest: sig
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
  | GetDefinitionResponse of Ast.Location.t option
  | HoverResponse of Ast.Location.t option
[@@deriving eq, show]
