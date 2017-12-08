(** Copyright 2016-present Facebook. All rights reserved. **)

open Core

val initialize
  :  ?old_state: State.t
  -> Mutex.t
  -> State.connections ref
  -> ServerConfiguration.t
  -> State.t

val stop_server: ServerConfiguration.t -> Unix.File_descr.t -> unit

val remove_server_files: ServerConfiguration.t -> unit
