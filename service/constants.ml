(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre
open Path.AppendOperator


module Persistent = struct
  let log_path configuration =
    (Configuration.Analysis.pyre_root configuration
     ^| "persistent")
    ^| "client.log"
end


module Server = struct
  let root configuration =
    let server_root = Configuration.Analysis.pyre_root configuration ^| "server" in
    Unix.mkdir_p (Path.absolute server_root);
    server_root

  let log_path configuration =
    let root = root configuration in
    Path.create_relative ~root ~relative:"server.stdout"

  let saved_state_path configuration =
    let root = root configuration in
    Path.create_relative ~root ~relative:"server.state"
end


module Watchman = struct
  let watchman_root configuration =
    Configuration.Analysis.pyre_root configuration ^| "watchman"


  let lock_path configuration =
    (watchman_root configuration) ^| "watchman.lock"


  let pid_path configuration =
    (watchman_root configuration) ^| "watchman.pid"


  let log_path configuration =
    (watchman_root configuration) ^| "watchman.stdout"
end
