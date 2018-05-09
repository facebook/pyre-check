(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

module Socket = CommandSocket

val start: ServerConfiguration.t -> int
val start_command: Command.t

val stop: ?graceful: bool -> string -> unit -> unit
val stop_command: Command.t
