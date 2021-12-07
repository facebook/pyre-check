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
    (* True if this is an implicit call to the `__call__` method. *)
    implicit_dunder_call: bool;
    (* True if we should collapse the taint from arguments, cf. the taint analysis. *)
    collapse_tito: bool;
  }
  [@@deriving eq, show]

  val create
    :  ?implicit_self:bool ->
    ?implicit_dunder_call:bool ->
    ?collapse_tito:bool ->
    Target.t ->
    t
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
module CallCallees : sig
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

(* An aggregrate of all possible callees for a given attribute access. *)
module AttributeAccessCallees : sig
  type t = {
    property_targets: Target.t list;
    global_targets: Target.t list;
    return_type: Type.t;
    (* True if the attribute access should also be considered a regular attribute.
     * For instance, if the object has type `Union[A, B]` where only `A` defines a property. *)
    is_attribute: bool;
  }
  [@@deriving eq, show]
end

(* An aggregate of all possible callees for a given identifier expression. *)
module IdentifierCallees : sig
  type t = { global_targets: Target.t list } [@@deriving eq, show]
end

(* An aggregate of all possible callees for an arbitrary expression. *)
module ExpressionCallees : sig
  type t = {
    call: CallCallees.t option;
    attribute_access: AttributeAccessCallees.t option;
    identifier: IdentifierCallees.t option;
  }
  [@@deriving eq, show]

  val from_call : CallCallees.t -> t

  val from_attribute_access : AttributeAccessCallees.t -> t

  val from_identifier : IdentifierCallees.t -> t
end

(* An aggregate of all possible callees for an arbitrary location.
 * Note that multiple expressions might have the same location. *)
module LocationCallees : sig
  type t =
    | Singleton of ExpressionCallees.t
    | Compound of ExpressionCallees.t String.Map.Tree.t
  [@@deriving eq, show]
end

module DefineCallGraph : sig
  type t [@@deriving eq, show]

  val empty : t

  val add : t -> location:Ast.Location.t -> callees:LocationCallees.t -> t

  val resolve_call
    :  t ->
    location:Ast.Location.t ->
    call:Ast.Expression.Call.t ->
    CallCallees.t option

  val resolve_attribute_access
    :  t ->
    location:Ast.Location.t ->
    attribute:string ->
    AttributeAccessCallees.t option

  val resolve_identifier
    :  t ->
    location:Ast.Location.t ->
    identifier:string ->
    IdentifierCallees.t option
end

val call_graph_of_define
  :  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  define:Ast.Statement.Define.t ->
  DefineCallGraph.t

val resolve_ignoring_optional : resolution:Resolution.t -> Ast.Expression.t -> Type.t

val redirect_special_calls : resolution:Resolution.t -> Call.t -> Call.t

module SharedMemory : sig
  val add : callable:Target.callable_t -> call_graph:DefineCallGraph.t -> unit

  (* Attempts to read the call graph for the given callable from shared memory. If it doesn't exist,
     computes the call graph and writes to shard memory. *)
  val get_or_compute
    :  callable:Target.callable_t ->
    environment:Analysis.TypeEnvironment.ReadOnly.t ->
    define:Ast.Statement.Define.t ->
    DefineCallGraph.t

  val remove : Target.callable_t list -> unit
end

val create_callgraph
  :  ?use_shared_memory:bool ->
  environment:TypeEnvironment.ReadOnly.t ->
  source:Source.t ->
  DependencyGraph.callgraph
