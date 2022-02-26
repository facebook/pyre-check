(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type class_hierarchy = ConstraintsSet.class_hierarchy

type order = ConstraintsSet.order

val always_less_or_equal : order -> left:Type.t -> right:Type.t -> bool

val is_compatible_with : order -> left:Type.t -> right:Type.t -> bool

val join : order -> Type.t -> Type.t -> Type.t

val meet : order -> Type.t -> Type.t -> Type.t

val widen
  :  order ->
  widening_threshold:int ->
  previous:Type.t ->
  next:Type.t ->
  iteration:int ->
  Type.t

module OrderedConstraints : TypeConstraints.OrderedConstraintsType with type order = order

module OrderedConstraintsSet : ConstraintsSet.OrderedConstraintsSetType

val instantiate_protocol_parameters
  :  order ->
  candidate:Type.t ->
  protocol:Ast.Identifier.t ->
  Type.Parameter.t list option
