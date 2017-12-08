(** Copyright 2016-present Facebook. All rights reserved. **)

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

module Map : Map.S with type Key.t = t

include Hashable with type t := t

val create: start:Lexing.position -> stop:Lexing.position -> t

val any: t

val line: t -> int
val column: t -> int
val path: t -> string
