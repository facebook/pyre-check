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

module CallTarget : sig
  type t = {
    target: Target.t;
    (* True if the call has an implicit receiver.
     * For instance, `x.foo()` should be treated as `C.foo(x)`. *)
    implicit_self: bool;
    (* True if we should collapse the taint from arguments, cf. the taint analysis. *)
    collapse_tito: bool;
  }
  [@@deriving eq, show]
end

(* Information about an argument being a callable. *)
module HigherOrderParameter : sig
  type t = {
    index: int;
    call_targets: CallTarget.t list;
    return_type: Type.t;
  }
  [@@deriving eq, show]
end

(* An aggregate of all possible callees at a call site. *)
module RawCallees : sig
  type t = {
    (* Normal call targets. *)
    call_targets: CallTarget.t list;
    (* Call targets for calls to the `__new__` class method. *)
    new_targets: Target.t list;
    (* Call targets for calls to the `__init__` instance method. *)
    init_targets: Target.t list;
    (* The return type of the call. *)
    return_type: Type.t;
    (* Information about an argument being a callable, and possibly called. *)
    higher_order_parameter: HigherOrderParameter.t option;
    (* True if at least one callee could not be resolved.
     * Usually indicates missing type information at the call site. *)
    unresolved: bool;
  }
  [@@deriving eq, show]

  val create
    :  ?call_targets:CallTarget.t list ->
    ?new_targets:Target.t list ->
    ?init_targets:Target.t list ->
    ?higher_order_parameter:HigherOrderParameter.t ->
    ?unresolved:bool ->
    return_type:Type.t ->
    unit ->
    t

  val create_unresolved : Type.t -> t

  val is_partially_resolved : t -> bool

  val pp_option : Format.formatter -> t option -> unit
end

module Callees : sig
  type t =
    | Callees of RawCallees.t
    | SyntheticCallees of RawCallees.t String.Map.Tree.t
  [@@deriving eq, show]
end

val call_graph_of_define
  :  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  define:Ast.Statement.Define.t ->
  Callees.t Ast.Location.Map.t

val call_name : Call.t -> string

val resolve_ignoring_optional : resolution:Resolution.t -> Ast.Expression.t -> Type.t

val redirect_special_calls : resolution:Resolution.t -> Call.t -> Call.t

module SharedMemory : sig
  val add : callable:Target.callable_t -> callees:Callees.t Location.Map.t -> unit

  (* Attempts to read the call graph for the given callable from shared memory. If it doesn't exist,
     computes the call graph and writes to shard memory. *)
  val get_or_compute
    :  callable:Target.callable_t ->
    environment:Analysis.TypeEnvironment.ReadOnly.t ->
    define:Ast.Statement.Define.t ->
    Callees.t Ast.Location.Map.t

  val remove : Target.callable_t list -> unit
end

val create_callgraph
  :  ?use_shared_memory:bool ->
  environment:TypeEnvironment.ReadOnly.t ->
  source:Source.t ->
  DependencyGraph.callgraph
