(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

type t = string [@@deriving compare, eq, sexp, hash, to_yojson]

module Map : Map.S with type Key.t = t

module SerializableMap : SerializableMap.S with type key = t

module Set : Set.S with type Elt.t = t

include Hashable with type t := t

val sanitized : t -> t

val equal_sanitized : t -> t -> bool

val pp : Format.formatter -> t -> unit

val pp_sanitized : Format.formatter -> t -> unit

val remove_leading_underscores : t -> t

val split_star : t -> string * t
