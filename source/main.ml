(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Core CLI commands and main pyre entry point *)

open Core
open Commands

let commands () =
  [
    Analyze.command ();
    Check.command ();
    CodeNavigation.command ();
    Infer.command ();
    Server.command ();
    (* TODO(T126811354) remove these once the client is updated *)
    Analyze.command ~name:"newanalyze" ();
    Check.command ~name:"newcheck" ();
    Infer.command ~name:"newinfer" ();
    Server.command ~name:"newserver" ();
    NoDaemonQuery.command ();
  ]


let run () =
  let open Cmdliner in
  let version = Version.version () in
  let info = Cmd.info "pyre" ~doc:"Analyze Python files" ~version in
  Cmd.group info (commands ()) |> Cmd.eval |> exit


let () =
  try
    Exception.record_backtrace true;
    Random.self_init ();
    run ()
  with
  | error ->
      let exn = Exception.wrap error in
      Log.error "%s" (Exception.to_string exn);
      Exception.reraise exn
