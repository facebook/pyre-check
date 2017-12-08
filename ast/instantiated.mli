(** Copyright 2016-present Facebook. All rights reserved. **)

open Core

open Expression
open Statement


module Access : sig
  type t = Expression.t Access.t
  [@@deriving compare, eq, sexp, show]

  module Set: Set.S with type Elt.t = t
  module Map : Map.S with type Key.t = t
  include Hashable with type t := t

  val create: string -> t
  val create_from_identifiers: Identifier.t list -> t

  val access: Expression.t -> t
end

type access = Access.t
[@@deriving compare, eq, sexp, show]

module Define : sig
  type t = Statement.t Define.t
  [@@deriving compare, eq, sexp, show]

  val is_method: t -> bool
  val is_abstract_method: t -> bool
  val is_overloaded_method: t -> bool
  val is_static_method: t -> bool
  val is_class_method: t -> bool
  val is_constructor: t -> bool
  val is_untyped: t -> bool

  val dump: t -> bool
  val dump_cfg: t -> bool
end

type define = Define.t
[@@deriving compare, eq, sexp, show]
