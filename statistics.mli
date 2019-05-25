(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

val disable : unit -> unit

val sample
  :  ?integers:(string * int) list ->
  ?normals:(string * string) list ->
  ?metadata:bool ->
  unit ->
  string

val flush : unit -> unit

val performance
  :  ?flush:bool ->
  ?randomly_log_every:int ->
  ?always_log_time_threshold:float ->
  ?section:Log.section ->
  ?category:string ->
  name:string ->
  timer:Timer.t ->
  ?integers:(string * int) list ->
  ?normals:(string * string) list ->
  unit ->
  unit

val coverage
  :  ?flush:bool ->
  ?randomly_log_every:int ->
  path:string ->
  coverage:(string * int) list ->
  unit ->
  unit

val event
  :  ?flush:bool ->
  ?randomly_log_every:int ->
  ?section:Log.section ->
  name:string ->
  ?integers:(string * int) list ->
  ?normals:(string * string) list ->
  unit ->
  unit

val log_exception : exn -> fatal:bool -> origin:string -> unit
