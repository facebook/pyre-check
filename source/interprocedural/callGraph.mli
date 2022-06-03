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

(** Represents type information about the return type of a call. *)
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

(** A specific target of a given call, with extra information. *)
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

(** Information about an argument being a callable. *)
module HigherOrderParameter : sig
  type t = {
    index: int;
    call_targets: CallTarget.t list;
  }
  [@@deriving eq, show]
end

(** An aggregate of all possible callees at a call site. *)
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

(** An aggregrate of all possible callees for a given attribute access. *)
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

(** An aggregate of all possible callees for a given identifier expression, i.e `foo`. *)
module IdentifierCallees : sig
  type t = { global_targets: CallTarget.t list } [@@deriving eq, show]
end

(** An aggregate of all implicit callees for any expression used in a f-string. *)
module FormatStringCallees : sig
  type t = { call_targets: CallTarget.t list } [@@deriving eq, show]
end

(** An aggregate of all possible callees for an arbitrary expression. *)
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

(** An aggregate of all possible callees for an arbitrary location.

    Note that multiple expressions might have the same location. *)
module LocationCallees : sig
  type t =
    | Singleton of ExpressionCallees.t
    | Compound of ExpressionCallees.t String.Map.Tree.t
  [@@deriving eq, show]
end

(** The call graph of a function or method definition. *)
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

  val all_targets : t -> Target.t list
  (** Return all callees of the call graph, as a sorted list. *)
end

val call_graph_of_define
  :  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  override_graph:OverrideGraph.SharedMemory.t ->
  attribute_targets:Target.HashSet.t ->
  qualifier:Reference.t ->
  define:Ast.Statement.Define.t ->
  DefineCallGraph.t

val redirect_special_calls : resolution:Resolution.t -> Call.t -> Call.t

val call_graph_of_callable
  :  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  override_graph:OverrideGraph.SharedMemory.t ->
  attribute_targets:Target.HashSet.t ->
  callable:Target.t ->
  DefineCallGraph.t

(** Call graphs of callables, stored in the shared memory. This is a mapping from a callable to its
    `DefineCallGraph.t`. *)
module DefineCallGraphSharedMemory : sig
  type t

  val get : t -> callable:Target.t -> DefineCallGraph.t option
end

(** Whole-program call graph, stored in the ocaml heap. This is a mapping from a callable to all its
    callees. *)
module WholeProgramCallGraph : sig
  type t

  val empty : t

  val is_empty : t -> bool

  val of_alist_exn : (Target.t * Target.t list) list -> t

  val add_or_exn : callable:Target.t -> callees:Target.t list -> t -> t

  val fold : t -> init:'a -> f:(target:Target.t -> callees:Target.t list -> 'a -> 'a) -> 'a

  val to_target_graph : t -> TargetGraph.t
end

type call_graphs = {
  whole_program_call_graph: WholeProgramCallGraph.t;
  define_call_graphs: DefineCallGraphSharedMemory.t;
}

val build_whole_program_call_graph
  :  scheduler:Scheduler.t ->
  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  environment:TypeEnvironment.ReadOnly.t ->
  override_graph:OverrideGraph.SharedMemory.t ->
  store_shared_memory:bool ->
  attribute_targets:Target.HashSet.t ->
  callables:Target.t list ->
  call_graphs
(** Build the whole call graph of the program.

    The overrides must be computed first because we depend on a global shared memory graph to
    include overrides in the call graph. Without it, we'll underanalyze and have an inconsistent
    fixpoint. *)
