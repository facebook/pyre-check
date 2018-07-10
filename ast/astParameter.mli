(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

module Node = AstNode
module Identifier = AstIdentifier
module Location = AstLocation

type 'expression parameter = {
  name: Identifier.t;
  value: 'expression option;
  annotation: 'expression option;
}
[@@deriving compare, eq, sexp, show, hash]

type 'expression t = 'expression parameter Node.t
[@@deriving compare, eq, sexp, show, hash]

val create
  :  ?location: Location.reference
  -> ?value: 'expression
  -> ?annotation: 'expression
  -> name: Identifier.t
  -> unit
  -> 'expression t

val name: 'expression t -> Identifier.t
