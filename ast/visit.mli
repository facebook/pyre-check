(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Expression

type node =
  | Expression of Expression.t
  | Statement of Statement.t
  | Identifier of Identifier.t Node.t
  | Parameter of Expression.t Parameter.t
  | Substring of StringLiteral.Substring.t Node.t

module type NodeVisitor = sig
  type t

  val node : t -> node -> t
end

module MakeNodeVisitor (Visitor : NodeVisitor) : sig
  val visit_expression
    :  state:Visitor.t ref ->
    ?visitor_override:(Visitor.t -> node -> Visitor.t) ->
    Expression.t ->
    unit

  val visit_statement : state:Visitor.t ref -> Statement.t -> unit

  val visit : Visitor.t -> Source.t -> Visitor.t
end

module type Visitor = sig
  type t

  val expression : t -> Expression.t -> t

  val statement : t -> Statement.t -> t
end

module Make (Visitor : Visitor) : sig
  val visit : Visitor.t -> Source.t -> Visitor.t
end

module type StatementVisitor = sig
  type t

  val visit_children : Statement.t -> bool

  val statement : Source.t -> t -> Statement.t -> t
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

module type NodePredicate = sig
  type t

  val predicate : node -> t option
end

module Collector
    (ExpressionPredicate : ExpressionPredicate)
    (StatementPredicate : StatementPredicate)
    (NodePredicate : NodePredicate) : sig
  type collection = {
    expressions: ExpressionPredicate.t list;
    statements: StatementPredicate.t list;
    nodes: NodePredicate.t list;
  }

  val collect : Source.t -> collection
end

module ExpressionCollector (Predicate : ExpressionPredicate) : sig
  val collect : Source.t -> Predicate.t list
end

module StatementCollector (Predicate : StatementPredicate) : sig
  val collect : Source.t -> Predicate.t list
end

module NodeCollector (Predicate : NodePredicate) : sig
  val collect : Source.t -> Predicate.t list
end

val collect_locations : Source.t -> Location.t list

val collect_calls : Statement.t -> Expression.Call.t Node.t list

val collect_names : ?only_simple:bool -> Statement.t -> Expression.t Expression.Name.t Node.t list

val collect_calls_and_names : Statement.t -> Expression.t list

val collect_base_identifiers : Statement.t -> Identifier.t Node.t list
