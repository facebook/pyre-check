(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

type t
[@@deriving show]

val empty: t

type solution = Type.t Type.Map.t

val exists: t -> predicate: (Type.t -> bool) -> bool

module type OrderedConstraintsType = sig
  type order
  val add_lower_bound: t -> order: order -> variable: Type.variable -> bound: Type.t -> t option
  val add_upper_bound: t -> order: order -> variable: Type.variable -> bound: Type.t -> t option
  val solve: t -> order: order -> solution option
  val extract_partial_solution
    :  t
    -> order: order
    -> variables: Type.variable list
    -> (t * solution) option
end

module type OrderType = sig
  type t
  val less_or_equal: t -> left: Type.t -> right: Type.t -> bool
  val meet: t -> Type.t -> Type.t -> Type.t
  val join: t -> Type.t -> Type.t -> Type.t
end

module OrderedConstraints(Order: OrderType) : OrderedConstraintsType with type order = Order.t
