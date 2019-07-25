(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
module AstReference = Reference

type position = {
  line: int;
  column: int;
}
[@@deriving compare, eq, sexp, show, hash, to_yojson]

val any_position : position

(* Yes, I hate abbreviations that much *)
type 'path location = {
  path: 'path;
  start: position;
  stop: position;
}
[@@deriving compare, eq, sexp, show, hash, to_yojson]

module Reference : sig
  type t = AstReference.t location [@@deriving compare, eq, sexp, show, hash]

  module Map : Map.S with type Key.t = t

  module Set : Set.S with type Elt.t = t

  include Hashable with type t := t

  val create : start:Lexing.position -> stop:Lexing.position -> t

  val start : t -> position

  val stop : t -> position

  val any : t

  val synthetic : t

  val pp_line_and_column : Format.formatter -> t -> unit
end

module Instantiated : sig
  type t = string location [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val create : start:Lexing.position -> stop:Lexing.position -> t

  val any : t

  val synthetic : t

  val pp_start : Format.formatter -> t -> unit

  val pp_line : Format.formatter -> t -> unit
end

val instantiate : Reference.t -> lookup:(AstReference.t -> string option) -> Instantiated.t

val reference : Instantiated.t -> Reference.t

val line : 'path location -> int

val column : 'path location -> int

val stop_column : 'path location -> int

val path : 'path location -> 'path

(* Shortcuts to make this more palatable. *)
type t = Reference.t [@@deriving compare, eq, sexp, show, hash, to_yojson]

val create : start:Lexing.position -> stop:Lexing.position -> t
