(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Analysis

module InitializeRequest : module type of Types.InitializeRequest

module TextDocumentDefinitionRequest : module type of Types.TextDocumentDefinitionRequest

(** PublishDiagnostics notification, method="textDocument/publishDiagnostics" *)
module PublishDiagnostics : sig
  type t [@@deriving yojson]

  val of_errors
    :  configuration:Configuration.Analysis.t ->
    string ->
    TypeCheck.Error.t list ->
    t Or_error.t
  (** Turn a type check error into a PublishDiagnostics notification *)

  val clear_diagnostics_for_uri : uri:string -> t
  (** Clear diagnostics for the given URI. *)

  val uri : t -> string
end

(** DidSaveTextDocument notification, method="textDocument/didSave" *)
module DidSaveTextDocument : sig
  type t [@@deriving to_yojson]

  val create : ?root:Path.t -> string -> string option -> t Or_error.t
  (** create path content_option evaluates to a DidSaveTextDocument notification *)
end

module ApplyWorkspaceEdit : sig
  type t [@@deriving to_yojson]

  val create : id:Types.RequestId.t -> Types.WorkspaceEdit.t -> t
end

module ShowMessage : sig
  type t [@@deriving to_yojson]

  val create : Types.ShowMessageParameters.messageType -> string -> t
end

module ShowStatus : sig
  type t [@@deriving to_yojson]

  val create
    :  message_type:Types.ShowMessageParameters.messageType ->
    content:string ->
    short_message:string option ->
    progress:Types.ShowStatusParameters.progress option ->
    t
end

module InitializeResponse : sig
  type t [@@deriving to_yojson]

  val default : Types.RequestId.t -> t
  (** default [id] [response] the server initialize response message *)
end

module ShutdownResponse : sig
  type t [@@deriving to_yojson]

  val default : Types.RequestId.t -> t
  (** default [id] [response] the shutdown response *)
end

module CompletionResponse : sig
  type t [@@deriving to_yojson]

  val create : id:Types.RequestId.t -> items:Types.CompletionItems.t -> t
end

module TextDocumentDefinitionResponse : sig
  type t [@@deriving to_yojson]

  val create
    :  configuration:Configuration.Analysis.t ->
    id:Types.RequestId.t ->
    location:Ast.Location.Instantiated.t option ->
    t
end

module HoverResponse : sig
  type t [@@deriving to_yojson]

  type hover_result = {
    location: Ast.Location.Instantiated.t;
    contents: string;
  }

  val create : id:Types.RequestId.t -> result:hover_result option -> t
end

module CodeActionResponse : sig
  type t [@@deriving to_yojson]

  val create : id:Types.RequestId.t -> code_actions:Types.CodeAction.t list -> t
end

module TypeCoverageResponse : sig
  type t [@@deriving to_yojson]

  val create : id:Types.RequestId.t -> covered_percent:int -> t
end

module RageResponse : sig
  type t [@@deriving to_yojson]

  val create : items:Types.RageResponse.RageResult.rageItem list -> id:Types.RequestId.t -> t
end

val to_message : Yojson.Safe.json -> string
(** Convert json to string content and set the content length headers. cf.
    https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#base-protocol *)

val write_message : Out_channel.t -> Yojson.Safe.json -> unit
(** Write a language server protocol message to a channel *)

val read_message : In_channel.t -> Yojson.Safe.json option
(** Read a language server protocol message from a channel (can raise End_of_file) *)
