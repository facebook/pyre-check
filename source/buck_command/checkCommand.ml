(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let doc = "Runs a full check"

let run_check_command input_argument_file = Log.info "Input argument file = %s" input_argument_file

let command () =
  let open Cmdliner in
  let filename = Arg.(required & pos 0 (some file) None & info [] ~docv:"filename") in
  let term = Term.(const run_check_command $ filename) in
  let info = Cmd.info "check" ~doc in
  Cmd.v info term
