(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Analysis
open Ast
open Expression

module ReturnType : sig
  type t = {
    is_boolean: bool;
    is_integer: bool;
    is_float: bool;
    is_enumeration: bool;
  }
  [@@deriving eq, show]

  val any : t

  val none : t

  val bool : t

  val integer : t

  val from_annotation : resolution:GlobalResolution.t -> Type.t -> t
end

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
    (* The textual order index of the call in the function. *)
    index: int;
    (* The return type of the call expression, or `None` for object targets. *)
    return_type: ReturnType.t option;
    (* The type of the receiver object at this call site, if any. *)
    receiver_type: Type.t option;
  }
  [@@deriving eq, show]

  val target : t -> Target.t

  val create
    :  ?implicit_self:bool ->
    ?implicit_dunder_call:bool ->
    ?collapse_tito:bool ->
    ?index:int ->
    ?return_type:ReturnType.t option ->
    ?receiver_type:Type.t ->
    Target.t ->
    t
end

(* Information about an argument being a callable. *)
module HigherOrderParameter : sig
  type t = {
    index: int;
    call_targets: CallTarget.t list;
  }
  [@@deriving eq, show]
end

(* An aggregate of all possible callees at a call site. *)
module CallCallees : sig
  type t = {
    (* Normal call targets. *)
    call_targets: CallTarget.t list;
    (* Call targets for calls to the `__new__` class method. *)
    new_targets: CallTarget.t list;
    (* Call targets for calls to the `__init__` instance method. *)
    init_targets: CallTarget.t list;
    (* Information about an argument being a callable, and possibly called. *)
    higher_order_parameter: HigherOrderParameter.t option;
    (* True if at least one callee could not be resolved.
     * Usually indicates missing type information at the call site. *)
    unresolved: bool;
  }
  [@@deriving eq, show]

  val create
    :  ?call_targets:CallTarget.t list ->
    ?new_targets:CallTarget.t list ->
    ?init_targets:CallTarget.t list ->
    ?higher_order_parameter:HigherOrderParameter.t ->
    ?unresolved:bool ->
    unit ->
    t

  val unresolved : t

  val is_partially_resolved : t -> bool

  val pp_option : Format.formatter -> t option -> unit
end

(* An aggregrate of all possible callees for a given attribute access. *)
module AttributeAccessCallees : sig
  type t = {
    property_targets: CallTarget.t list;
    global_targets: CallTarget.t list;
    (* True if the attribute access should also be considered a regular attribute.
     * For instance, if the object has type `Union[A, B]` where only `A` defines a property. *)
    is_attribute: bool;
  }
  [@@deriving eq, show]

  val empty : t
end

(* An aggregate of all possible callees for a given identifier expression. *)
module IdentifierCallees : sig
  type t = { global_targets: CallTarget.t list } [@@deriving eq, show]
end

(* An aggregate of all implicit callees for any expression used in a f string *)
module FormatStringCallees : sig
  type t = { call_targets: CallTarget.t list } [@@deriving eq, show]
end

(* An aggregate of all possible callees for an arbitrary expression. *)
module ExpressionCallees : sig
  type t = {
    call: CallCallees.t option;
    attribute_access: AttributeAccessCallees.t option;
    identifier: IdentifierCallees.t option;
    format_string: FormatStringCallees.t option;
  }
  [@@deriving eq, show]

  val from_call : CallCallees.t -> t

  val from_call_with_empty_attribute : CallCallees.t -> t

  val from_attribute_access : AttributeAccessCallees.t -> t

  val from_identifier : IdentifierCallees.t -> t

  val from_format_string : FormatStringCallees.t -> t
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

  val resolve_format_string : t -> location:Ast.Location.t -> FormatStringCallees.t option

  (* For testing purpose only. *)
  val equal_ignoring_types : t -> t -> bool
end

val call_graph_of_define
  :  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  qualifier:Reference.t ->
  define:Ast.Statement.Define.t ->
  DefineCallGraph.t

val redirect_special_calls : resolution:Resolution.t -> Call.t -> Call.t

module SharedMemory : sig
  val add : callable:Target.t -> call_graph:DefineCallGraph.t -> unit

  val get : callable:Target.t -> DefineCallGraph.t option

  val remove : Target.t list -> unit
end

val create_callgraph
  :  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  store_shared_memory:bool ->
  environment:TypeEnvironment.ReadOnly.t ->
  source:Source.t ->
  DependencyGraph.callgraph
