(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let register_alarm callback = Sys.signal Sys.sigalrm (Sys.Signal_handle callback)

let start_alarm time = ignore (Unix.alarm time)

let clear_alarm previous =
  let _ = Unix.alarm 0 in
  let _ = Sys.signal Sys.sigalrm previous in
  ()


let callable_max_time_in_seconds = 60

let with_alarm name f () =
  (* Print a warning every `callable_max_time_in_seconds` * 2^n seconds. *)
  let alarm_counter = ref 0 in
  let next_warning = ref 1 in
  let callback _ =
    start_alarm callable_max_time_in_seconds;
    incr alarm_counter;
    if !alarm_counter = !next_warning then (
      next_warning := 2 * !next_warning;
      let current_time_in_seconds = !alarm_counter * callable_max_time_in_seconds in
      Statistics.event
        ~flush:true
        ~name:"long taint analysis of callable"
        ~section:`Performance
        ~integers:["cutoff time", current_time_in_seconds]
        ~normals:["callable", Interprocedural.Target.show_pretty name]
        ();
      let pid = Unix.getpid () in
      Log.info
        "The analysis of %a is taking more than %d seconds (pid = %d)"
        Interprocedural.Target.pp_pretty
        name
        current_time_in_seconds
        pid)
  in
  let id = register_alarm callback in
  let () = start_alarm callable_max_time_in_seconds in
  try
    let result = f () in
    clear_alarm id;
    result
  with
  | e ->
      clear_alarm id;
      raise e
