(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


val disable: unit -> unit

val sample
  :  ?system_time: float
  -> ?integers: (string * int) list
  -> ?normals: (string * string) list
  -> unit
  -> string

val flush: unit -> unit

val performance
  :  ?flush: bool
  -> ?randomly_log_every: int
  -> name: string
  -> timer: Timer.t
  -> root: string
  -> ?normals: (string * string) list
  -> unit
  -> unit

val coverage
  :  ?flush: bool
  -> coverage: (string * int) list
  -> root: string
  -> ?normals: (string * string) list
  -> unit
  -> unit

val event
  :  ?flush: bool
  -> name: string
  -> root: string
  -> ?integers: (string * int) list
  -> ?normals: (string * string) list
  -> unit
  -> unit
