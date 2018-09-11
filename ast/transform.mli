(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


module type Transformer = sig
  type t

  val expression: t -> Expression.t -> Expression.t
  val transform_children: t -> Statement.t -> bool
  val statement: t -> Statement.t -> t * Statement.t list
end

module type StatementTransformer = sig
  type t
  val statement: t -> Statement.t -> t * Statement.t list
end

module Identity : sig
  val expression: 't -> Expression.t -> Expression.t
  val transform_children: 't -> Statement.t -> bool
  val statement: 't -> Statement.t -> 't * Statement.t list
end

module Make (Transformer: Transformer) : sig
  val transform: Transformer.t -> Source.t -> Transformer.t * Source.t
end

module MakeStatementTransformer(Transformer: StatementTransformer) : sig
  val transform: Transformer.t -> Source.t -> Transformer.t * Source.t
end
