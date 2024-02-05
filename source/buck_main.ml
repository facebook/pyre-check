(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Main pyre entry point for Buck *)

let commands () = [Buck_commands.Check.command ()]

let run () =
  let open Cmdliner in
  let version = Version.version () in
  let info = Cmd.info "pyre_for_buck" ~doc:"Pyre entry point to be invoked from Buck" ~version in
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
