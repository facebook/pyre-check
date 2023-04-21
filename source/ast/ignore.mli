(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
[@@deriving compare, show, sexp, hash]

val create : ignored_line:int -> codes:int list -> location:Location.t -> kind:kind -> t

val codes : t -> int list

val location : t -> Location.t

val kind : t -> kind

val increment : t -> t

val start_of_ignored_line_or_range : t -> int

val with_start_line : start_line:int -> t -> t
