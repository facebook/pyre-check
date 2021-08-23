(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

(* Analyze command uses the same exit code scheme as check command. *)
module ExitStatus = NewCheckCommand.ExitStatus

let run_analyze _configuration_file =
  Log.warning "Coming soon...";
  let exit_status = ExitStatus.Ok in
  Statistics.flush ();
  exit (ExitStatus.exit_code exit_status)


let command =
  let filename_argument = Command.Param.(anon ("filename" %: Filename.arg_type)) in
  Command.basic
    ~summary:"Runs taint analysis"
    (Command.Param.map filename_argument ~f:(fun filename () -> run_analyze filename))
