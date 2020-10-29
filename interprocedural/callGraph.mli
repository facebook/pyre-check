(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Analysis
open Ast
open Expression

type regular_targets = {
  implicit_self: bool;
  collapse_tito: bool;
  targets: Callable.t list;
}
[@@deriving eq, show]

type raw_callees =
  | ConstructorTargets of {
      new_targets: Callable.t list;
      init_targets: Callable.t list;
    }
  | RegularTargets of regular_targets
  | HigherOrderTargets of {
      higher_order_function: regular_targets;
      callable_argument: int * regular_targets;
    }
[@@deriving eq, show]

type callees =
  | Callees of raw_callees
  | SyntheticCallees of raw_callees String.Map.Tree.t
[@@deriving eq, show]

val call_graph_of_define
  :  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  define:Ast.Statement.Define.t ->
  callees Ast.Location.Map.t

val defining_attribute
  :  resolution:Resolution.t ->
  Type.t ->
  string ->
  Annotated.Attribute.instantiated option

val compute_indirect_targets
  :  resolution:Resolution.t ->
  receiver_type:Type.t ->
  Ast.Reference.t ->
  Callable.t list

val call_name : Call.t -> string

val resolve_ignoring_optional : resolution:Resolution.t -> Ast.Expression.t -> Type.t

val transform_special_calls : resolution:Resolution.t -> Call.t -> Call.t option

val redirect_special_calls : resolution:Resolution.t -> Call.t -> Call.t

module SharedMemory : sig
  val add : callable:Callable.real_target -> callees:callees Location.Map.t -> unit

  (* Attempts to read the call graph for the given callable from shared memory. If it doesn't exist,
     computes the call graph and writes to shard memory. *)
  val get_or_compute
    :  callable:Callable.real_target ->
    environment:Analysis.TypeEnvironment.ReadOnly.t ->
    define:Ast.Statement.Define.t ->
    callees Ast.Location.Map.t

  val remove : Callable.real_target list -> unit
end
