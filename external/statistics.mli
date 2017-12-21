(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


val sample
  :  ?system_time: float
  -> ?integers: (string * int) list
  -> ?normals: (string * string) list
  -> unit
  -> string

val flush: unit -> unit

val performance
  :  ?flush: bool
  -> root: string
  -> time: int
  -> normals: (string * string) list
  -> unit

val coverage
  :  ?flush: bool
  -> root: string
  -> coverage: (string * int) list
  -> normals: (string * string) list
  -> unit

val event
  :  ?flush: bool
  -> root: string
  -> name: string
  -> integers: (string * int) list
  -> normals: (string * string) list
  -> unit
