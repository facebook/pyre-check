(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Expression

type node =
  | Expression of Expression.t
  | Statement of Statement.t
  | Argument of Identifier.t Node.t
  | Parameter of Parameter.t
  | Reference of Reference.t Node.t
  | Substring of Substring.t
  | Generator of Comprehension.Generator.t

module type NodeVisitor = sig
  type t

  val node : t -> node -> t

  val visit_statement_children : t -> Statement.t -> bool

  val visit_expression_children : t -> Expression.t -> bool

  val visit_format_string_children : t -> Expression.t -> bool
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

  val visit_children : Expression.t -> bool

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

val collect_calls : Statement.t -> Call.t Node.t list

val collect_names : ?only_simple:bool -> Statement.t -> Name.t Node.t list

val collect_calls_and_names : Statement.t -> Expression.t list

val collect_base_identifiers : Statement.t -> Identifier.t Node.t list

val collect_non_generic_type_names : Expression.t -> Identifier.t list

val collect_format_strings_with_ignores
  :  ignore_line_map:Ignore.t list Int.Map.t ->
  Source.t ->
  (Expression.t * Ignore.t list) list
