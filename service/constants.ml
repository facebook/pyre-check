(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Path.AppendOperator

module Adapter = struct
  let log_path configuration =
    (Configuration.Analysis.log_directory configuration ^| "server") ^| "adapter.log"
end

module Persistent = struct
  let log_path configuration =
    (Configuration.Analysis.log_directory configuration ^| "persistent") ^| "client.log"
end

module Server = struct
  let root configuration =
    let server_root = Configuration.Analysis.log_directory configuration ^| "server" in
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
  let file_monitor_log_path configuration =
    Configuration.Analysis.log_directory configuration ^| "file_monitor/file_monitor.log"


  let configuration_monitor_log_path configuration =
    Configuration.Analysis.log_directory configuration
    ^| "configuration_monitor/configuration_monitor.log"
end
