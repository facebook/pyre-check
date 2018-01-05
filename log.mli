(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


type section = [
    `Check
  | `Debug
  | `Dependencies
  | `Dump
  | `Environment
  | `Error
  | `Event
  | `Info
  | `Performance
  | `Protocols
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

module Color : sig
  val yellow: string -> string
end
