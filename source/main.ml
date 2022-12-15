(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Commands

let commands =
  [
    "analyze", Analyze.command;
    "check", Check.command;
    "code-navigation", CodeNavigation.command;
    "infer", Infer.command;
    "server", Server.command;
    (* TODO(T126811354) remove these once the client is updated *)
    "newanalyze", Analyze.command;
    "newcheck", Check.command;
    "newinfer", Infer.command;
    "newserver", Server.command;
    "no-daemon-query", NoDaemonQuery.command;
  ]


let () =
  try
    Random.self_init ();
    Scheduler.Daemon.check_entry_point ();
    Command.group ~summary:"Analyze Python files" commands
    |> Command_unix.run ~build_info:(Version.build_info ()) ~version:(Version.version ())
  with
  | error ->
      Log.error "%s" (Exn.to_string error);
      raise error
