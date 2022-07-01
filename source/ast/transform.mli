(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type Transformer = sig
  type t

  val transform_expression_children : t -> Expression.t -> bool

  val expression : t -> Expression.t -> Expression.t

  val transform_children : t -> Statement.t -> t * bool

  val statement : t -> Statement.t -> t * Statement.t list
end

module type StatementTransformer = sig
  type t

  val statement : t -> Statement.t -> t * Statement.t list
end

module Identity : sig
  val transform_expression_children : 't -> Expression.t -> bool

  val expression : 't -> Expression.t -> Expression.t

  val transform_children : 't -> Statement.t -> 't * bool

  val statement : 't -> Statement.t -> 't * Statement.t list
end

module Make (Transformer : Transformer) : sig
  type result = {
    state: Transformer.t;
    source: Source.t;
  }

  val source : result -> Source.t

  val transform : Transformer.t -> Source.t -> result
end

module MakeStatementTransformer (Transformer : StatementTransformer) : sig
  type result = {
    state: Transformer.t;
    source: Source.t;
  }

  val source : result -> Source.t

  val transform : Transformer.t -> Source.t -> result
end

val transform_in_statement
  :  transform:(Expression.expression -> Expression.expression) ->
  Statement.statement ->
  Statement.statement

val transform_in_expression
  :  transform:(Expression.expression -> Expression.expression) ->
  Expression.t ->
  Expression.t

val sanitize_expression : Expression.t -> Expression.t

val sanitize_statement : Statement.statement -> Statement.statement
