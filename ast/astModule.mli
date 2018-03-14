(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

module Statement = AstStatement
module Access = AstExpression.Access


type t
[@@deriving compare, eq, sexp, show]

val create: Statement.t list -> t

val export: t -> Access.t -> Access.t option
