(** Copyright 2016-present Facebook. All rights reserved. **)

open Core

module Parallel = Hack_parallel.Std

let commands = [
  "check", Check.check_command;
  "codex", CodexGenerator.command;
  "query", Query.command;
  "rage", Rage.command;
  "incremental", Check.incremental_command;
  "persistent", Persistent.command;
  "start", Server.start_command;
  "stop", Server.stop_command;
  "watchman", Watchman.command;
]


let () =
  Parallel.Daemon.check_entry_point ();
  Command.group
    ~summary:"Analyze Python files"
    commands
  |> Command.run
