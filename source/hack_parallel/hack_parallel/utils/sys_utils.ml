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

external get_total_ram : unit -> int = "hh_sysinfo_totalram"
external uptime : unit -> int = "hh_sysinfo_uptime"
external nproc: unit -> int = "nproc"

let total_ram = get_total_ram ()
let nbr_procs = nproc ()

external set_priorities : cpu_priority:int -> io_priority:int -> unit =
  "hh_set_priorities"

external pid_of_handle: int -> int = "pid_of_handle"
external handle_of_pid_for_termination: int -> int =
  "handle_of_pid_for_termination"

let terminate_process pid = Unix.kill pid Sys.sigkill
