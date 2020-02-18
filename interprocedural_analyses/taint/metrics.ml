(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

let register_alarm time callback =
  let previous = Sys.signal Sys.sigalrm (Sys.Signal_handle callback) in
  let _ = Unix.alarm time in
  previous


let clear_alarm previous =
  let _ = Unix.alarm 0 in
  let _ = Sys.signal Sys.sigalrm previous in
  ()


(* seconds *)
let callable_max_time = 120

let with_alarm name f () =
  let callback _ =
    let pid = Unix.getpid () in
    Log.info
      "The analysis of %a is taking more than %d seconds (pid = %d)"
      Reference.pp
      name
      callable_max_time
      pid
  in
  let id = register_alarm callable_max_time callback in
  try
    let result = f () in
    clear_alarm id;
    result
  with
  | e ->
      clear_alarm id;
      raise e
