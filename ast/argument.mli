(** Copyright 2016-present Facebook. All rights reserved. **)

type 'expression t = {
  name: Identifier.t option;
  value: 'expression;
}
[@@deriving compare, eq, sexp, show]

val is_positional: 'expression t -> bool
