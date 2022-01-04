(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'node_type t = {
  location: Location.t;
  value: 'node_type;
}
[@@deriving compare, sexp, show, hash, to_yojson]

val create : location:Location.t -> 'node_value -> 'node_value t

val create_with_default_location : 'node_value -> 'node_value t

val value : 'node_type t -> 'node_type

val location : 'node_type t -> Location.t

val start : 'node_type t -> Location.position

val stop : 'node_type t -> Location.position

val number_of_lines : 'node_type t -> int

val map : 'node_type t -> f:('node_type -> 'new_node_type) -> 'new_node_type t

val location_insensitive_compare
  :  ('node_type -> 'node_type -> int) ->
  'node_type t ->
  'node_type t ->
  int

val location_insensitive_equal
  :  ('node_type -> 'node_type -> bool) ->
  'node_type t ->
  'node_type t ->
  bool
