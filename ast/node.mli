(** Copyright 2016-present Facebook. All rights reserved. **)

type 'node_type t = {
  location: Location.t;
  value: 'node_type;
}
[@@deriving compare, eq, sexp, show]

val create: ?location: Location.t -> 'node_value -> 'node_value t

val value: 'node_type t -> 'node_type

val start: 'node_type t -> Location.position
val stop: 'node_type t -> Location.position
