(** Copyright 2016-present Facebook. All rights reserved. **)

open Pyre

exception ServerNotRunning

type t = {
  (* Server-specific configuration options *)
  socket_path: Path.t;
  socket_link: Path.t;
  lock_path: Path.t;
  pid_path: Path.t;
  log_path: Path.t;
  daemonize: bool;
  (* Analysis configuration *)
  configuration: Configuration.t
}

(** Returns the path of the socket the server listens on *)
val socket_path: ?create: bool -> Configuration.t -> Path.t

val create
  :  ?daemonize:bool
  -> ?log_path: Path.t
  -> Configuration.t
  -> t
