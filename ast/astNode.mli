(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

module Location = AstLocation

type 'node_type t = {
  location: Location.t;
  value: 'node_type;
}
[@@deriving compare, eq, sexp, show, hash]

val create: ?location: Location.t -> 'node_value -> 'node_value t

val value: 'node_type t -> 'node_type

val start: 'node_type t -> Location.position
val stop: 'node_type t -> Location.position
