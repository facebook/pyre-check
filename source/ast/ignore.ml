(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core

type kind =
  | TypeIgnore
  | PyreFixme
  | PyreIgnore
[@@deriving compare, show, sexp, hash]

type t = {
  ignored_line: int;
  codes: int list;
  location: Location.t;
  kind: kind;
}
[@@deriving show, sexp]

let compare left right =
  let { Location.start = left_start; _ } = left.location in
  let { Location.start = right_start; _ } = right.location in
  [%compare: int * int list * Location.position * kind]
    (left.ignored_line, left.codes, left_start, left.kind)
    (right.ignored_line, right.codes, right_start, right.kind)


let hash_fold_t state { ignored_line; codes; location; kind; _ } =
  let { Location.start; _ } = location in
  [%hash_fold: int * int list * Location.position * kind] state (ignored_line, codes, start, kind)


let hash = Hash.run hash_fold_t

let create ~ignored_line ~codes ~location ~kind = { ignored_line; codes; location; kind }

let codes { codes; _ } = codes

let location { location; _ } = location

let kind { kind; _ } = kind

let increment ({ ignored_line; _ } as ignore) = { ignore with ignored_line = ignored_line + 1 }

let start_of_ignored_line_or_range { ignored_line; _ } = ignored_line

let with_start_line ~start_line ignore = { ignore with ignored_line = start_line }
