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
  -> Mutex.t
  -> State.connections ref
  -> ServerConfiguration.t
  -> State.t
val stop: reason: string -> ServerConfiguration.t -> Unix.File_descr.t -> unit

val connect: retries: int -> configuration: Configuration.t -> Socket.t
