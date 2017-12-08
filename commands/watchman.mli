(** Copyright 2016-present Facebook. All rights reserved. **)

open Core

open Pyre

module Socket = PyreSocket

val process_response
  :  root: Path.t
  -> watchman_directory:Path.t
  -> symlinks: Path.t Path.Map.t
  -> string
  -> (Path.t Path.Map.t * Protocol.Request.t) Option.t

val run_command
  :  bool
  -> bool
  -> string list
  -> string
  -> unit
  -> unit

val command : Command.t
