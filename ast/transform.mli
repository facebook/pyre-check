(** Copyright 2016-present Facebook. All rights reserved. **)


module type Transformer = sig
  type t
  val expression: t -> Expression.t -> t * Expression.t
  val statement: t -> Statement.t -> t * Statement.t list
end

module Make (Transformer: Transformer) : sig
  val transform: ?shallow:bool -> Transformer.t -> Source.t -> Transformer.t * Source.t
end
