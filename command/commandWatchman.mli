(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre

module Socket = CommandSocket

(* Exposed for testing. *)
val build_symlink_map: PyrePath.t list -> PyrePath.t Path.Map.t
val process_response
  :  root: Path.t
  -> watchman_directory:Path.t
  -> symlinks: Path.t Path.Map.t
  -> string
  -> (Path.t Path.Map.t * ServerProtocol.Request.t) Option.t

val run_command
  :  daemonize: bool
  -> verbose: bool
  -> sections: string list
  -> local_root: string
  -> project_root: string option
  -> Pid.t

val command : Command.t
