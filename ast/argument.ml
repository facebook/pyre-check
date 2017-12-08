(** Copyright 2016-present Facebook. All rights reserved. **)

open Core
open Sexplib.Std


type 'expression t = {
  name: Identifier.t option;
  value: 'expression;
}
[@@deriving compare, eq, sexp, show]


let is_positional { name; _ } =
  name = None
