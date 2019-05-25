(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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
    [ "server", Constants.Server.log_path configuration;
      "watchman", Constants.Watchman.log_path configuration;
      "persistent", Constants.Persistent.log_path configuration ]
