(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

type t
[@@deriving compare, eq, sexp, show, hash]

module Map : Map.S with type Key.t = t
module Set: Set.S with type Elt.t = t

val create: string -> t

val sanitize: t -> t
val pp_sanitized: Format.formatter -> t -> unit
val show_sanitized: t -> string

val remove_leading_underscores: t -> t

val map: t -> f: (string -> string) -> t
