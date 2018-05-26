(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

type version_mismatch = {
  server_version: string;
  expected_version: string;
}
[@@deriving show]

exception ConnectionFailure
exception VersionMismatch of version_mismatch

val connect: retries: int -> configuration: Configuration.t -> CommandSocket.t

val initialize
  :  ?old_state: ServerState.t
  -> Mutex.t
  -> ServerState.connections ref
  -> ServerConfiguration.t
  -> ServerState.t

val stop_server: reason: string -> ServerConfiguration.t -> Unix.File_descr.t -> unit

val remove_server_files: ServerConfiguration.t -> unit
