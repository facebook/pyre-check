(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

module Expression = AstExpression
module Statement = AstStatement
module Source = AstSource
module Location = AstLocation
module Node = AstNode


module type Visitor = sig
  type t
  val expression: t -> Expression.t -> t
  val statement: t -> Statement.t -> t
end

module type StatementVisitor = sig
  type t
  val statement: Source.t -> t -> Statement.t -> t
end

module Make (Visitor: Visitor) : sig
  val visit: Visitor.t -> Source.t -> Visitor.t
end

module MakeStatementVisitor (Visitor: StatementVisitor) : sig
  val visit: Visitor.t -> Source.t -> Visitor.t
end

module type ExpressionPredicate = sig
  type t
  val predicate: Expression.t -> t option
end

module type StatementPredicate = sig
  type t
  val predicate: Statement.t -> t option
end

module Collector
    (ExpressionPredicate: ExpressionPredicate)
    (StatementPredicate: StatementPredicate) : sig
  val collect
    :  Source.t
    -> ExpressionPredicate.t list * StatementPredicate.t list
end

module ExpressionCollector (Predicate: ExpressionPredicate) : sig
  val collect: Source.t -> Predicate.t list
end

module StatementCollector (Predicate: StatementPredicate) : sig
  val collect: Source.t -> Predicate.t list
end

val collect_accesses_in_position
  :  Statement.t
  -> Location.position
  -> (Expression.Access.t Node.t) list

val collect_accesses: Statement.t -> Expression.Access.t list
