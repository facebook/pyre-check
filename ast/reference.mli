(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

type t [@@deriving compare, eq, sexp, show, hash]

val create : ?prefix:t -> string -> t

module Map : Map.S with type Key.t = t

module SerializableMap : SerializableMap.S with type key = t

module Set : Set.S with type Elt.t = t

include Hashable with type t := t

module Key : Memory.KeyType with type t = t and type out = t

module ListValue : Value.Type with type t = t list

val empty : t

val create_from_list : Identifier.t list -> t

val as_list : t -> Identifier.t list

val combine : t -> t -> t

val from_name : Expression.t Expression.Name.t -> t option

val from_name_exn : Expression.t Expression.Name.t -> t

val name : ?location:Location.t -> t -> Expression.t Expression.Name.t

val expression : ?location:Location.t -> t -> Expression.t

val delocalize : t -> t

val sanitized : t -> t

val sanitize_qualified : t -> t

val equal_sanitized : t -> t -> bool

val pp_sanitized : Format.formatter -> t -> unit

val show_sanitized : t -> string

val single : t -> Identifier.t option

val length : t -> int

val reverse : t -> t

val is_empty : t -> bool

val is_prefix : prefix:t -> t -> bool

val is_suffix : suffix:t -> t -> bool

val is_strict_prefix : prefix:t -> t -> bool

val drop_prefix : prefix:t -> t -> t

val prefix : t -> t option

val last : t -> Identifier.t
