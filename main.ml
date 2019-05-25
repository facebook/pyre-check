(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Commands

let commands =
  [ "analyze", Analyze.command;
    "check", Check.check_command;
    "query", Query.command;
    "rage", Rage.command;
    "incremental", Incremental.command;
    "persistent", Persistent.command;
    "start", Start.command;
    "stop", Stop.command ]


let () =
  try
    Random.self_init ();
    Scheduler.Daemon.check_entry_point ();
    Command.group ~summary:"Analyze Python files" commands
    |> Command.run ~build_info:(Version.build_info ()) ~version:(Version.version ())
  with
  | error ->
      Log.error "%s" (Exn.to_string error);
      raise error
