(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

type t
[@@deriving compare, eq, sexp, show, hash]

module Map : Map.S with type Key.t = t
module SerializableMap: SerializableMap.S with type key = t
module Set: Set.S with type Elt.t = t
include Hashable with type t := t

val empty: t
val create: ?prefix: t -> string -> t
val create_from_list: string list -> t
val combine: t -> t -> t

val from_access: Expression.Access.t -> t
val access: t -> Expression.Access.t
val name: t -> Expression.t Expression.Name.t
val expression: ?location: Location.t -> t -> Expression.t
val new_expression: ?location: Location.t -> t -> Expression.t

val sanitized: t -> t
val sanitize_qualified: t -> t
val equal_sanitized: t -> t -> bool
val pp_sanitized: Format.formatter -> t -> unit
val show_sanitized: t -> string

val single: t -> Identifier.t option
val is_prefix: prefix: t -> t -> bool
val is_suffix: suffix: t -> t -> bool
val is_strict_prefix: prefix: t -> t -> bool
val drop_prefix: prefix: t -> t -> t
val prefix: t -> t option
val last: t -> Identifier.t
