(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Sexplib.Std


type 'expression t = {
  name: Identifier.t option;
  value: 'expression;
}
[@@deriving compare, eq, sexp, show]


let is_positional { name; _ } =
  name = None
