(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre

let display_logs ~name path =
  let read_path path =
    if Path.file_exists path then
      Some (Sys_utils.cat (Path.absolute path))
    else
      None
  in
  read_path path
  >>| Out_channel.printf "\nDisplaying logs for %s:\n%s" name
  |> ignore


let run_rage project_root () =
  let configuration = Configuration.create ~project_root:(Path.create_absolute project_root) () in
  display_logs
    ~name:"server"
    (ServerConfiguration.create configuration
     |> fun { ServerConfiguration.log_path; _ } -> log_path);
  display_logs
    ~name:"watchman"
    (WatchmanConstants.log_path configuration);
  display_logs
    ~name:"persistent client"
    (Persistent.log_path configuration)

let command =
  let open Command.Spec in
  Command.basic
    ~summary:"Reports debugging diagnostics for Pyre to the standard output."
    (empty
     +> anon (maybe_with_default "." ("project-root" %: string)))
    run_rage
