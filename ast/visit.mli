(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

module type Visitor = sig
  type t

  val expression : t -> Expression.t -> t

  val statement : t -> Statement.t -> t
end

module type StatementVisitor = sig
  type t

  val visit_children : Statement.t -> bool

  val statement : Source.t -> t -> Statement.t -> t
end

module Make (Visitor : Visitor) : sig
  val visit_expression
    :  state:Visitor.t ref ->
    visitor:(Visitor.t -> Expression.t -> Visitor.t) ->
    Expression.t ->
    unit

  val visit_statement
    :  state:Visitor.t ref ->
    visitor:(Visitor.t -> Statement.t -> Visitor.t) ->
    Statement.t ->
    unit

  val visit : Visitor.t -> Source.t -> Visitor.t
end

module MakeStatementVisitor (Visitor : StatementVisitor) : sig
  val visit : Visitor.t -> Source.t -> Visitor.t
end

module type ExpressionPredicate = sig
  type t

  val predicate : Expression.t -> t option
end

module type StatementPredicate = sig
  type t

  val visit_children : Statement.t -> bool

  val predicate : Statement.t -> t option
end

module Collector
    (ExpressionPredicate : ExpressionPredicate)
    (StatementPredicate : StatementPredicate) : sig
  val collect : Source.t -> ExpressionPredicate.t list * StatementPredicate.t list
end

module ExpressionCollector (Predicate : ExpressionPredicate) : sig
  val collect : Source.t -> Predicate.t list
end

module StatementCollector (Predicate : StatementPredicate) : sig
  val collect : Source.t -> Predicate.t list
end

val collect_accesses : Statement.t -> Expression.Access.general_access Node.t list

val collect_calls : Statement.t -> Expression.t Expression.Call.t Node.t list

val collect_names : ?only_simple:bool -> Statement.t -> Expression.t Expression.Name.t Node.t list

val collect_base_identifiers : Statement.t -> Identifier.t Node.t list
