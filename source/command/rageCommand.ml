(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open LanguageServer.Types

let get_watchman_watched_directories () =
  let channel = Unix.open_process_in "watchman watch-list" in
  let data = In_channel.input_all channel in
  In_channel.close channel;
  { RageResponse.RageResult.title = Some "Watchman watched directories"; data }


let get_mercurial_base () =
  let channel = Unix.open_process_in "hg id" in
  let data = In_channel.input_all channel in
  In_channel.close channel;
  { RageResponse.RageResult.title = Some "Mercurial commit hash"; data }


let get_mercurial_status () =
  let channel = Unix.open_process_in "hg status" in
  let data = In_channel.input_all channel in
  In_channel.close channel;
  { RageResponse.RageResult.title = Some "Mercurial status"; data }


let get_mercurial_diff () =
  let channel = Unix.open_process_in "hg diff" in
  let data = In_channel.input_all channel in
  In_channel.close channel;
  { RageResponse.RageResult.title = Some "Mercurial diff"; data }


let get_mercurial_reflog () =
  let channel = Unix.open_process_in "hg reflog --limit 100" in
  let data = In_channel.input_all channel in
  In_channel.close channel;
  { RageResponse.RageResult.title = Some "Mercurial Reflog"; data }


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


let run_rage log_directory _unused_source_directory () =
  try
    Out_channel.printf
      "Actual binary version: %s\nBinary build info: %s\n"
      (Version.version ())
      (Version.build_info ());
    let configuration = Configuration.Analysis.create ?log_directory ~source_path:[] () in
    let logs =
      get_mercurial_base ()
      :: get_mercurial_status ()
      :: get_mercurial_diff ()
      :: get_mercurial_reflog ()
      :: get_watchman_watched_directories ()
      :: get_pyre_locks ()
      @ Service.Rage.get_logs configuration
    in
    List.iter ~f:display_log logs
  with
  | error ->
      Log.log_exception error;
      raise error


let command =
  let open Command.Spec in
  Command.basic_spec
    ~summary:"Reports debugging diagnostics for Pyre to the standard output."
    ( empty
    +> flag "-log-directory" (optional string) ~doc:"Location to write logs and other data"
    +> anon (maybe_with_default "." ("source-root" %: string)) )
    run_rage
