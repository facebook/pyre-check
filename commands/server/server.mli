(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

module Socket = PyreSocket

type version_mismatch = {
  server_version: string;
  client_version: string;
}
[@@deriving show]

exception ConnectionFailure
exception VersionMismatch of version_mismatch
val connect: retries: int -> configuration: Configuration.t -> Socket.t

val start: ServerConfiguration.t -> int
val start_command: Command.t

val stop: string -> unit -> unit
val stop_command: Command.t
