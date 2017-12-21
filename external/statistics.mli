(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


val performance
  :  ?flush: bool
  -> name: string
  -> time: int
  -> normals: (string * string) list
  -> unit

val coverage
  :  ?flush: bool
  -> coverage: (string * int) list
  -> normals: (string * string) list
  -> unit

val event
  :  ?flush: bool
  -> name: string
  -> integers: (string * int) list
  -> normals: (string * string) list
  -> unit
