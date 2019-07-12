(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open LanguageServer.Types

let get_watchman_watched_directories () =
  let channel = Unix.open_process_in "watchman watch-list" in
  let data = In_channel.input_all channel in
  In_channel.close channel;
  { RageResponse.RageResult.title = Some "Watchman watched directories"; data }


let display_log { RageResponse.RageResult.title; data } =
  let name = Option.value_exn title in
  Out_channel.printf "\nDisplaying logs for %s:\n%s" name data


let get_pyre_locks () =
  try
    let channel = Unix.open_process_in "lslocks" in
    let data = In_channel.input_all channel in
    In_channel.close channel;
    [{ RageResponse.RageResult.title = Some "Pyre lock information"; data }]
  with
  | Unix.Unix_error _ -> []


let run_rage local_root () =
  Out_channel.printf
    "Actual binary version: %s\nBinary build info: %s\n"
    (Version.version ())
    (Version.build_info ());
  let configuration =
    Configuration.Analysis.create ~local_root:(Path.create_absolute local_root) ()
  in
  let logs =
    (get_watchman_watched_directories () :: get_pyre_locks ())
    @ Service.Rage.get_logs configuration
  in
  List.iter ~f:display_log logs


let command =
  let open Command.Spec in
  Command.basic_spec
    ~summary:"Reports debugging diagnostics for Pyre to the standard output."
    (empty +> anon (maybe_with_default "." ("source-root" %: string)))
    run_rage
