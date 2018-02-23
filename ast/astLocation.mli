(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

type position = {
  line: int;
  column: int;
}
[@@deriving compare, eq, sexp, show, hash]

(* Yes, I hate abbreviations that much *)
type t = {
  path: string;
  start: position;
  stop: position;
}
[@@deriving compare, eq, sexp, show, hash]

val to_string: t -> string

val pp_start: Format.formatter -> t -> unit

module Map : Map.S with type Key.t = t

module Set : Set.S with type Elt.t = t

include Hashable with type t := t

val create: start:Lexing.position -> stop:Lexing.position -> t

val any: t

val start_line: t -> int -> t

val line: t -> int
val column: t -> int
val path: t -> string
