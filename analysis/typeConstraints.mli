(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

type t
[@@deriving show]

val empty: t

type solution = Type.t Type.Map.t

(* Checks a predicate against all of the bounds accumulated in a set of constraints *)
val exists: t -> predicate: (Type.t -> bool) -> bool

module type OrderedConstraintsType = sig
  (* This module defines a system to construct and solve a set of constraints on type variables.
     These constraints are built up in the form of intervals on the type lattice defined by the
     given order module.
     The solve system can handle chained constraints of the form X =<= F(Y) && Y =<= T, but declines
     to solve cyclic ones, e.g. X =<= Y, Y =<= X. *)
  type order
  val add_lower_bound: t -> order: order -> variable: Type.Variable.t -> bound: Type.t -> t option
  val add_upper_bound: t -> order: order -> variable: Type.Variable.t -> bound: Type.t -> t option
  val solve: t -> order: order -> solution option
  (* This solves the constraints for the given variables, and then substitutes those solution in
     for those variables in the constraints for the remaining constraints. *)
  val extract_partial_solution
    :  t
    -> order: order
    -> variables: Type.Variable.t list
    -> (t * solution) option
end

module type OrderType = sig
  type t
  val less_or_equal: t -> left: Type.t -> right: Type.t -> bool
  val meet: t -> Type.t -> Type.t -> Type.t
  val join: t -> Type.t -> Type.t -> Type.t
end

module OrderedConstraints(Order: OrderType) : OrderedConstraintsType with type order = Order.t
