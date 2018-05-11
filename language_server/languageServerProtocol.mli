(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Analysis

open Pyre


module InitializeRequest : module type of LanguageServerProtocolTypes.InitializeRequest

module TextDocumentDefinitionRequest
  : module type of LanguageServerProtocolTypes.TextDocumentDefinitionRequest

(** PublishDiagnostics notification, method="textDocument/publishDiagnostics" *)
module PublishDiagnostics : sig
  type t
  [@@deriving to_yojson]

  (** Turn a type check error into a PublishDiagnostics notification *)
  val of_errors: ?root: Path.t -> File.Handle.t -> TypeCheck.Error.t list -> t Or_error.t
end

(** DidSaveTextDocument notification, method="textDocument/didSave" *)
module DidSaveTextDocument : sig
  type t
  [@@deriving to_yojson]


  (** create path content_option evaluates to a DidSaveTextDocument notification *)
  val create: ?root: Path.t -> string -> string option -> t Or_error.t
end

module ShowMessage : sig
  type t
  [@@deriving to_yojson]

  val create: LanguageServerProtocolTypes.ShowMessageParams.messageType -> string -> t
end

module InitializeResponse : sig
  type t
  [@@deriving to_yojson]

  (** default [id] [response] the server initialize response message *)
  val default: int -> t
end

module ShutdownResponse : sig
  type t
  [@@deriving to_yojson]

  (** default [id] [response] the shutdown response *)
  val default: int -> t
end

module TextDocumentDefinitionResponse : sig
  type t
  [@@deriving to_yojson]

  val create: ?root: Path.t -> id: int -> location: Ast.Location.t option -> t
end

module HoverResponse : sig
  type t
  [@@deriving to_yojson]

  val create: contents: string -> id: int -> location: Ast.Location.t option -> t
end

module RageResponse : sig
  type t
  [@@deriving to_yojson]

  val create
    :  items: LanguageServerProtocolTypes.RageResponse.RageResult.rageItem list
    -> id: int
    -> t
end

(** Convert json to string content and set the content length headers.
    cf. https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#base-protocol *)
val to_message: Yojson.Safe.json -> string

(** Write a language server protocol message to a channel *)
val write_message: Out_channel.t -> Yojson.Safe.json -> unit

(** Read a language server protocol message from a channel *)
val read_message: In_channel.t -> Yojson.Safe.json option
