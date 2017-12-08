(** Copyright 2016-present Facebook. All rights reserved. **)

type 'expression parameter = {
  name: Identifier.t;
  value: 'expression option;
  annotation: 'expression option;
}
[@@deriving compare, eq, sexp, show]

type 'expression t = 'expression parameter Node.t
[@@deriving compare, eq, sexp, show]

val create
  :  ?location: Location.t
  -> ?value: 'expression
  -> ?annotation: 'expression
  -> name: Identifier.t
  -> unit
  -> 'expression t

val name: 'expression t -> Identifier.t
