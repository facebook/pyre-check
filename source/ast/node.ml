(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type 'node_type t = {
  location: Location.t;
  value: 'node_type;
}
[@@deriving sexp, show, hash, to_yojson, compare]

let create ~location value = { location; value }

let create_with_default_location value = { location = Location.any; value }

let pp print_node format { value; _ } = print_node format value

let location_insensitive_compare compare_value left right = compare_value left.value right.value

let location_insensitive_equal equal_value left right = equal_value left.value right.value

let start { location; _ } = location.Location.start

let stop { location; _ } = location.Location.stop

let number_of_lines
    {
      location =
        {
          Location.start = { Location.line = start_line; _ };
          stop = { Location.line = stop_line; _ };
          _;
        };
      _;
    }
  =
  stop_line - start_line + 1


let value { value; _ } = value

let location { location; _ } = location

let map { location; value } ~f = { location; value = f value }
