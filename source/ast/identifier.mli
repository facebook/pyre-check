(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type t = string [@@deriving compare, sexp, hash, to_yojson]

module Map : Map.S with type Key.t = t

module SerializableMap : Data_structures.SerializableMap.S with type key = t

module Set : Set.S with type Elt.t = t

include Hashable with type t := t

val sanitized : t -> t

val is_sanitized : t -> bool

val equal_sanitized : t -> t -> bool

val equal : t -> t -> bool

val pp : Format.formatter -> t -> unit

val pp_sanitized : Format.formatter -> t -> unit

val remove_leading_underscores : t -> t

val split_star : t -> string * t
