(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

module Parallel = Hack_parallel.Std
open PyreCommand


let commands = [
  "check", Check.check_command;
  "codex", CodexGenerator.command;
  "query", Query.command;
  "rage", Rage.command;
  "incremental", Incremental.command;
  "persistent", Persistent.command;
  "start", Server.start_command;
  "stop", Server.stop_command;
  "watchman", Watchman.command;
]


let () =
  Random.self_init ();
  Parallel.Daemon.check_entry_point ();
  Command.group
    ~summary:"Analyze Python files"
    commands
  |> Command.run
