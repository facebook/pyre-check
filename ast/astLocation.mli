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
type 'path t = {
  path: 'path;
  start: position;
  stop: position;
}
[@@deriving compare, eq, sexp, show, hash]


type reference = int t
[@@deriving compare, eq, sexp, show, hash]


type instantiated = string t
[@@deriving compare, eq, sexp, show, hash]


val to_string_reference: reference -> string
val to_string_instantiated: instantiated -> string

val pp_start_instantiated: Format.formatter -> instantiated -> unit

module ReferenceMap : Map.S with type Key.t = reference

module ReferenceSet : Set.S with type Elt.t = reference

include Hashable with type t := reference

val create: start:Lexing.position -> stop:Lexing.position -> reference
val instantiate: lookup: (int -> string option) -> reference -> instantiated
val to_reference: instantiated -> reference

val any: reference
val any_instantiated: instantiated

val line: 'path t -> int
val column: 'path t -> int
val path: 'path t -> 'path
