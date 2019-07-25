(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

type kind =
  | TypeIgnore
  | PyreFixme
  | PyreIgnore
[@@deriving compare, eq, show, sexp, hash]

type t = {
  ignored_line: int;
  codes: int list;
  location: Location.t;
  kind: kind;
}
[@@deriving compare, eq, show, sexp, hash]

let create ~ignored_line ~codes ~location ~kind = { ignored_line; codes; location; kind }

let ignored_line { ignored_line; _ } = ignored_line

let codes { codes; _ } = codes

let location { location; _ } = location

let kind { kind; _ } = kind

let increment ({ ignored_line; _ } as ignore) = { ignore with ignored_line = ignored_line + 1 }

let key { location; ignored_line; _ } =
  let start = { Location.line = ignored_line; column = -1 } in
  { location with Location.start; stop = start }
