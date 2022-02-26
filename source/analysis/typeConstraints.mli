(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t [@@deriving compare, show]

val empty : t

(* Checks a predicate against all of the bounds accumulated in a set of constraints *)
val exists_in_bounds : t -> variables:Type.Variable.t list -> bool

module Solution : sig
  type t [@@deriving show, eq]

  val empty : t

  val instantiate : t -> Type.t -> Type.t

  val instantiate_single_variable : t -> Type.Variable.Unary.t -> Type.t option

  val instantiate_single_parameter_variadic
    :  t ->
    Type.Variable.Variadic.Parameters.t ->
    Type.Callable.parameters option

  val instantiate_ordered_types : t -> Type.OrderedTypes.t -> Type.OrderedTypes.t

  val instantiate_callable_parameters : t -> Type.Callable.parameters -> Type.Callable.parameters

  (* For testing *)
  val create : Type.Variable.pair list -> t
end

module type OrderedConstraintsType = sig
  (* This module defines a system to construct and solve a set of constraints on type variables.
     These constraints are built up in the form of intervals on the type lattice defined by the
     given order module. The solve system can handle chained constraints of the form X =<= F(Y) && Y
     =<= T, but declines to solve cyclic ones, e.g. X =<= Y, Y =<= X. *)
  type order

  val add_lower_bound : t -> order:order -> pair:Type.Variable.pair -> t option

  val add_upper_bound : t -> order:order -> pair:Type.Variable.pair -> t option

  val add_fallback_to_any : t -> Type.Variable.t -> t

  val solve : t -> order:order -> Solution.t option

  (* This solves the constraints for the given variables, and then substitutes those solution in for
     those variables in the constraints for the remaining constraints. *)
  val extract_partial_solution
    :  t ->
    order:order ->
    variables:Type.Variable.t list ->
    (t * Solution.t) option
end

module type OrderType = sig
  type t

  val always_less_or_equal : t -> left:Type.t -> right:Type.t -> bool

  val meet : t -> Type.t -> Type.t -> Type.t

  val join : t -> Type.t -> Type.t -> Type.t
end

module OrderedConstraints (Order : OrderType) : OrderedConstraintsType with type order = Order.t
