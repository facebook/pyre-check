(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

module Identifier = AstIdentifier


type 'expression t = {
  name: Identifier.t option;
  value: 'expression;
}
[@@deriving compare, eq, sexp, show, hash]

val is_positional: 'expression t -> bool
