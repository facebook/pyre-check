(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre

module Socket = CommandSocket

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
  -> string option
  -> string
  -> unit
  -> unit

val command : Command.t
