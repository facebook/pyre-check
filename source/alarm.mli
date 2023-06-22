(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Print a warning every `max_time_in_seconds` * 2^n seconds. *)
val with_alarm
  :  max_time_in_seconds:int ->
  event_name:string ->
  callable:string ->
  (unit -> 'a) ->
  unit ->
  'a
