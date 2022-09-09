(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module ExitStatus = struct
  type t = Ok [@@deriving sexp, compare, hash]

  let exit_code = function
    | Ok -> 0
end

let run_query _ =
  let () = Printf.printf "In run query" in
  exit (ExitStatus.exit_code Ok)


let command =
  Printexc.record_backtrace true;
  let filename_argument = Command.Param.(anon ("filename" %: Filename.arg_type)) in
  Command.basic
    ~summary:"Runs a full check without a server"
    (Command.Param.map filename_argument ~f:(fun filename () -> run_query filename))
