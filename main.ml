(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Commands


let commands = [
  "analyze", Analyze.command;
  "check", Check.check_command;
  "codex", CodexGenerator.command;
  "query", Query.command;
  "rage", Rage.command;
  "incremental", Incremental.command;
  "persistent", Persistent.command;
  "start", Start.command;
  "stop", Stop.command;
  "watchman", Watchman.command;
]


let () =
  Random.self_init ();
  Scheduler.Daemon.check_entry_point ();
  Command.group
    ~summary:"Analyze Python files"
    commands
  |> Command.run
    ~build_info:(Version.build_info ())
    ~version:(Version.version ())
