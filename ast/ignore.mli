(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

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

val create
  :  ignored_line: int
  -> codes: int list
  -> location: Location.t
  -> kind: kind
  -> t

val ignored_line: t -> int
val codes: t -> int list
val location: t -> Location.t
val kind: t -> kind
val key: t -> Location.t
