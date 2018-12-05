(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre


type state = {
  configuration: Configuration.Analysis.t;
  watchman_directory: Path.t;
  symlinks: Path.t Path.Map.t;
}

(* Exposed for testing. *)
val build_symlink_map: PyrePath.t list -> PyrePath.t Path.Map.t
val process_response: state -> string -> (state * Server.Protocol.Request.t) Option.t

val run_command
  :  daemonize: bool
  -> verbose: bool
  -> sections: string list
  -> local_root: string
  -> search_path: Path.SearchPath.t list
  -> project_root: string option
  -> Pid.t

val command : Command.t
