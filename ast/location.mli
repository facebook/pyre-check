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
type 'path location = {
  path: 'path;
  start: position;
  stop: position;
}
[@@deriving compare, eq, sexp, show, hash]

module Reference : sig
  type t = int location
  [@@deriving compare, eq, sexp, show, hash]

  module Map : Map.S with type Key.t = t
  module Set : Set.S with type Elt.t = t
  include Hashable with type t := t

  val create: start:Lexing.position -> stop:Lexing.position -> t
  val any: t

  val to_string: t -> string
end

module Instantiated : sig
  type  t = string location
  [@@deriving compare, eq, sexp, show, hash]

  val create: start:Lexing.position -> stop:Lexing.position -> t
  val any: t

  val to_string: t -> string
  val pp_start: Format.formatter -> t -> unit
end

val instantiate: Reference.t -> lookup: (int -> string option) -> Instantiated.t
val reference: Instantiated.t -> Reference.t

val line: 'path location -> int
val column: 'path location -> int
val path: 'path location -> 'path

(* Shortcuts to make this more palatable. *)
type t = Reference.t
[@@deriving compare, eq, sexp, show, hash]

val create: start:Lexing.position -> stop:Lexing.position -> t
