(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

type t
[@@deriving compare, eq, sexp, show, hash]

module Map : Map.S with type Key.t = t
module Set: Set.S with type Elt.t = t

val create: string -> t
val show_sanitized: t -> string

val length: t -> int
val append: separator:string -> t -> t -> t

val add_prefix: prefix: string -> t -> t
val remove_prefix: prefix: string -> t -> t
