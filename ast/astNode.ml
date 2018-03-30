(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

module Location = AstLocation

type 'node_type t = {
  location: Location.t;
  value: 'node_type;
}
[@@deriving sexp, show, hash]


let create ~location value =
  { location; value }

let create_with_default_location value =
  { location = Location.any; value }

let pp print_node format { value; _ } =
  print_node format value


let compare compare_value left right =
  compare_value left.value right.value


let equal equal_value left right =
  equal_value left.value right.value


let start {location; _ } =
  location.Location.start


let stop {location; _ } =
  location.Location.stop


let value { value; _ } =
  value


let location { location; _ } =
  location
