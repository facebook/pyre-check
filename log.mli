(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


type section = [
    `Check
  | `Debug
  | `Dump
  | `Environment
  | `Error
  | `Info
  | `Performance
  | `Server
  | `Warning
]


(* Setup the logging environment where `sections` is a list of sections that are
   enabled. *)
val initialize: verbose:bool -> sections:(string list) -> unit
val initialize_for_tests: unit -> unit

val log: section:section -> ('a, Format.formatter, unit, unit, unit, unit) Core.format6 -> 'a

val debug: ('a, Format.formatter, unit, unit, unit, unit) Core.format6 -> 'a
val dump: ('a, Format.formatter, unit, unit, unit, unit) Core.format6 -> 'a
val info: ('a, Format.formatter, unit, unit, unit, unit) Core.format6 -> 'a
val error: ('a, Format.formatter, unit, unit, unit, unit) Core.format6 -> 'a
val warning: ('a, Format.formatter, unit, unit, unit, unit) Core.format6 -> 'a

(* Logs directly to the standard output. *)
val print: ('a, Format.formatter, unit, unit, unit, unit) Core.format6 -> 'a

val performance
  :  ?flush: bool
  -> name: string
  -> timer: Timer.t
  -> root: string
  -> normals: (string * string) list
  -> unit

val coverage
  :  ?flush: bool
  -> coverage: (string * int) list
  -> root: string
  -> normals: (string * string) list
  -> unit

val event
  :  ?flush: bool
  -> name: string
  -> root: string
  -> integers: (string * int) list
  -> normals: (string * string) list
  -> unit

module Color : sig
  val yellow: string -> string
end
