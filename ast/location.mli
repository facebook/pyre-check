(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

type position = {
  line: int;
  column: int;
}
[@@deriving compare, eq, sexp, show]

(* Yes, I hate abbreviations that much *)
type t = {
  path: string;
  start: position;
  stop: position;
}
[@@deriving compare, eq, sexp, show]

val pp_start: Format.formatter -> t -> unit

module Map : Map.S with type Key.t = t

include Hashable with type t := t

val create: start:Lexing.position -> stop:Lexing.position -> t

val any: t

val line: t -> int
val column: t -> int
val path: t -> string
