(** Copyright 2016-present Facebook. All rights reserved. **)

open Pyre

val watchman_root: Configuration.t -> Path.t

val lock_path: Configuration.t -> Path.t

val pid_path: Configuration.t -> Path.t

val log_path: Configuration.t -> Path.t
