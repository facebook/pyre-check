(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open LanguageServer.Types

let get_logs configuration =
  let get_log (name, path) =
    let read_path path =
      if Path.file_exists path then
        Some (Sys_utils.cat (Path.absolute path))
      else
        None
    in
    read_path path >>| fun content -> { RageResponse.RageResult.title = Some name; data = content }
  in
  List.filter_map
    ~f:get_log
    [
      "server", Constants.Server.log_path configuration;
      "client", Constants.Client.log_path configuration;
      "file monitor", Constants.Watchman.file_monitor_log_path configuration;
      "configuration monitor", Constants.Watchman.configuration_monitor_log_path configuration;
      "persistent", Constants.Persistent.log_path configuration;
      "adapter", Constants.Adapter.log_path configuration;
    ]
