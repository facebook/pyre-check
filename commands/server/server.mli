(** Copyright 2016-present Facebook. All rights reserved. **)

open Core

module Socket = PyreSocket

exception ConnectionFailure
val connect: retries: int -> configuration: Configuration.t -> Socket.t

val start: ServerConfiguration.t -> int
val start_command: Command.t

val stop: string -> unit -> unit
val stop_command: Command.t
