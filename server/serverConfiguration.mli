(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Pyre

exception ServerNotRunning


type load = {
  shared_memory_path: Path.t;
  changed_files_path: Path.t;
}

type saved_state =
  | Save of string
  | Load of load

type t = {
  (* Server-specific configuration options *)
  socket_path: Path.t;
  socket_link: Path.t;
  lock_path: Path.t;
  pid_path: Path.t;
  log_path: Path.t;
  daemonize: bool;
  use_watchman: bool;
  watchman_creation_timeout: float;
  saved_state: saved_state option;
  (* Analysis configuration *)
  configuration: Configuration.t
}

(** Returns the path of the socket the server listens on *)
val socket_path: ?create: bool -> Configuration.t -> Path.t

val create
  :  ?daemonize: bool
  -> ?log_path: Path.t
  -> ?use_watchman: bool
  -> ?saved_state: saved_state
  -> Configuration.t
  -> t
