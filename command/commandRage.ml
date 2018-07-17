(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre

open LanguageServer.Types

module WatchmanConstants = CommandWatchmanConstants
module PersistentConstants = CommandPersistentConstants


let get_logs configuration =
  let get_log (name, path) =
    let read_path path =
      if Path.file_exists path then
        Some (Sys_utils.cat (Path.absolute path))
      else
        None
    in
    read_path path
    >>| fun content ->
    { RageResponse.RageResult.title = Some name; data = content }
  in
  List.filter_map
    ~f:get_log
    [
      "server",
      (ServerConfiguration.create configuration
       |> fun { ServerConfiguration.log_path; _ } -> log_path);
      "watchman",
      (WatchmanConstants.log_path configuration);
      "persistent",
      (PersistentConstants.log_path configuration);
    ]


let get_watchman_watched_directories () =
  let channel = Unix.open_process_in "watchman watch-list" in
  let data = In_channel.input_all channel in
  In_channel.close channel;
  { RageResponse.RageResult.title = Some "Watchman watched directories"; data }


let display_log { RageResponse.RageResult.title; data } =
  let name = Option.value_exn title in
  Out_channel.printf "\nDisplaying logs for %s:\n%s" name data


let run_rage source_root () =
  let configuration = Configuration.create ~source_root:(Path.create_absolute source_root) () in
  let logs = get_watchman_watched_directories () :: get_logs configuration in
  List.iter ~f:display_log logs


let command =
  let open Command.Spec in
  Command.basic_spec
    ~summary:"Reports debugging diagnostics for Pyre to the standard output."
    (empty
     +> anon (maybe_with_default "." ("source-root" %: string)))
    run_rage
