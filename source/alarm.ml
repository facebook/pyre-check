(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Alarm: implements an alarm that triggers every given period of time, to
 * warn about the slow analysis of a given callable. *)

let register_alarm callback = Sys.signal Sys.sigalrm (Sys.Signal_handle callback)

let start_alarm time = ignore (Unix.alarm time)

let clear_alarm previous =
  let _ = Unix.alarm 0 in
  let _ = Sys.signal Sys.sigalrm previous in
  ()


let with_alarm ~max_time_in_seconds ~event_name ~callable f () =
  let alarm_counter = ref 0 in
  let next_warning = ref 1 in
  let callback _ =
    start_alarm max_time_in_seconds;
    incr alarm_counter;
    if !alarm_counter = !next_warning then (
      next_warning := 2 * !next_warning;
      let current_time_in_seconds = !alarm_counter * max_time_in_seconds in
      Statistics.event
        ~flush:true
        ~name:(Format.sprintf "long %s of callable" event_name)
        ~section:`Performance
        ~integers:["cutoff time", current_time_in_seconds]
        ~normals:["callable", callable]
        ();
      let pid = Unix.getpid () in
      Log.info
        "The %s of %s is taking more than %d seconds (pid = %d)"
        event_name
        callable
        current_time_in_seconds
        pid)
  in
  let id = register_alarm callback in
  let () = start_alarm max_time_in_seconds in
  try
    let result = f () in
    clear_alarm id;
    result
  with
  | e ->
      clear_alarm id;
      raise e
