(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

let setsid =
  (* Not implemented on Windows. Let's just return the pid *)
  if Sys.win32 then Unix.getpid else Unix.setsid

external nproc: unit -> int = "nproc"

let nbr_procs = nproc ()

external set_priorities : cpu_priority:int -> io_priority:int -> unit =
  "hh_set_priorities"

let terminate_process pid = Unix.kill pid Sys.sigkill
