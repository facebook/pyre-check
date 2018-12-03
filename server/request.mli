(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


val parse_lsp
  :  configuration: Configuration.Analysis.t
  -> request: Yojson.Safe.json
  -> Protocol.Request.t option

type response = {
  state: State.t;
  response: Protocol.response option;
}

val process_client_shutdown_request: state: State.t -> id: int -> response
val process_type_query_request
  :  state: State.t
  -> configuration: Configuration.Analysis.t
  -> request: Protocol.TypeQuery.request
  -> response
val process_display_type_errors_request
  :  state: State.t
  -> configuration: Configuration.Analysis.t
  -> files: File.t list
  -> response
val process_type_check_request
  :  state: State.t
  -> configuration: Configuration.Analysis.t
  -> request: Protocol.TypeCheckRequest.t
  -> response
val process_get_definition_request
  :  state: State.t
  -> configuration: Configuration.Analysis.t
  -> request: Protocol.DefinitionRequest.t
  -> response

val process
  :  socket: Unix.File_descr.t
  -> state: State.t
  -> configuration: Configuration.Server.t
  -> request: Protocol.Request.t
  -> response
