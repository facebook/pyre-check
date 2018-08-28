(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre


exception InvalidRequest

val parse
  :  root: Path.t
  -> request: Yojson.Safe.json
  -> Protocol.Request.t option

val handle_client_shutdown_request: state: State.t -> id: int -> State.t * Protocol.response option
val handle_type_query_request
  :  state: State.t
  -> local_root: Path.t
  -> request: Protocol.TypeQuery.request
  -> Protocol.response
val handle_display_type_errors_request
  :  state: State.t
  -> local_root: Path.t
  -> files: File.t list
  -> State.t * (Protocol.response option)
val handle_type_check_request
  :  state: State.t
  -> configuration: Configuration.t
  -> request: Protocol.TypeCheckRequest.t
  -> State.t * (Protocol.response option)

val process_request
  :  new_socket: Unix.File_descr.t
  -> state: State.t
  -> configuration: ServerConfiguration.t
  -> request: Protocol.Request.t
  -> State.t * (Protocol.response option)
