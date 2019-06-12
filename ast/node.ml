(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

type 'node_type t = {
  location: Location.t; [@hash.ignore]
  value: 'node_type
}
[@@deriving sexp, show, hash]

let in_testing = ref false

let create ~location value = { location; value }

let create_with_default_location value = { location = Location.Reference.any; value }

let pp print_node format { value; _ } = print_node format value

let compare compare_value left right = compare_value left.value right.value

let equal equal_value left right =
  let locations_equal =
    if not !in_testing then
      true
    else
      Location.equal Location.Reference.any left.location
      || Location.equal Location.Reference.any right.location
      || Location.equal left.location right.location
  in
  equal_value left.value right.value && locations_equal


let start { location; _ } = location.Location.start

let stop { location; _ } = location.Location.stop

let value { value; _ } = value

let location { location; _ } = location

let map { location; value } ~f = { location; value = f value }
