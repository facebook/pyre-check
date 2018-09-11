(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Network


type version_mismatch = {
  server_version: string;
  expected_version: string;
}
[@@deriving show]

exception ConnectionFailure
exception VersionMismatch of version_mismatch

val start
  :  ?old_state: State.t
  -> lock: Mutex.t
  -> connections: State.connections ref
  -> configuration: ServerConfiguration.t
  -> unit
  -> State.t
val stop
  :  reason: string
  -> configuration: ServerConfiguration.t
  -> socket: Unix.File_descr.t
  -> unit

val connect: retries: int -> configuration: Configuration.t -> Socket.t
