(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

module Statement = AstStatement
module Expression = AstExpression
module Access = Expression.Access


type t
[@@deriving compare, eq, sexp, show]

val create: Statement.t list -> t

val export: t -> Access.t -> Access.t option

val resolve_export
  :  t
  -> head: Expression.t Access.access
  -> (Access.t * AstExpression.t Access.access) option
