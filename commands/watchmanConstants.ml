(** Copyright 2016-present Facebook. All rights reserved. **)

open Pyre
open Path.AppendOperator

let watchman_root configuration =
  Configuration.pyre_root configuration ^| "watchman"


let lock_path configuration =
  (watchman_root configuration) ^| "watchman.lock"


let pid_path configuration =
  (watchman_root configuration) ^| "watchman.pid"


let log_path configuration =
  (watchman_root configuration) ^| "watchman.stdout"
