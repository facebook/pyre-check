(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

type section =
  [ `Check
  | `Coverage
  | `Debug
  | `Dependencies
  | `DependencyGraph
  | `Dotty
  | `Dump
  | `Environment
  | `Error
  | `Event
  | `Fixpoint
  | `Info
  | `Interprocedural
  | `Memory
  | `Parser
  | `Performance
  | `Progress
  | `Protocols
  | `Server
  | `Taint
  | `Warning
  ]

val is_enabled : section -> bool

(* Setup the logging environment where `sections` is a list of sections that are enabled. *)
val initialize : verbose:bool -> sections:string list -> unit

val initialize_for_tests : unit -> unit

val log : section:section -> ('a, Format.formatter, unit, unit, unit, unit) Core.format6 -> 'a

val debug : ('a, Format.formatter, unit, unit, unit, unit) Core.format6 -> 'a

val dump : ('a, Format.formatter, unit, unit, unit, unit) Core.format6 -> 'a

val info : ('a, Format.formatter, unit, unit, unit, unit) Core.format6 -> 'a

val error : ('a, Format.formatter, unit, unit, unit, unit) Core.format6 -> 'a

val warning : ('a, Format.formatter, unit, unit, unit, unit) Core.format6 -> 'a

(* Logs directly to the standard output. *)
val print : ('a, Stdio.Out_channel.t, Base.unit) Base.format -> 'a

val log_unix_error : ?section:section -> Unix.error * string * string -> unit

module Color : sig
  val cyan : string -> string

  val red : string -> string

  val yellow : string -> string
end

val rotate : ?number_to_keep:int -> string -> string
