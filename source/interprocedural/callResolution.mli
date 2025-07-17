(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
module PyrePysaLogic = Analysis.PyrePysaLogic

(* Evaluates to whether the provided expression is a superclass of define. *)
val is_super
  :  pyre_in_context:PyrePysaApi.InContext.t ->
  define:Statement.Define.t Node.t ->
  Expression.t ->
  bool

(* Evaluate to whether a variable is nonlocal to a given define *)
val is_nonlocal
  :  pyre_in_context:PyrePysaApi.InContext.t ->
  define:Reference.t ->
  Reference.t ->
  bool

(* Check whether `successor` extends `predecessor`.
 * Returns false on untracked types.
 * Returns `reflexive` if `predecessor` and `successor` are equal. *)
val has_transitive_successor_ignoring_untracked
  :  pyre_api:PyrePysaApi.ReadOnly.t ->
  reflexive:bool ->
  predecessor:string ->
  successor:string ->
  bool

(* Resolve an expression into a type. Untracked types are resolved into `Any`. *)
val resolve_ignoring_untracked
  :  pyre_in_context:PyrePysaApi.InContext.t ->
  Ast.Expression.t ->
  Type.t

(* Resolve an attribute access into a type. Untracked types are resolved into `Any`. *)
val resolve_attribute_access_ignoring_untracked
  :  pyre_in_context:PyrePysaApi.InContext.t ->
  base_type:Type.t ->
  attribute:string ->
  Type.t

val defining_attribute
  :  pyre_in_context:PyrePysaApi.InContext.t ->
  Type.t ->
  string ->
  PyrePysaLogic.AnnotatedAttribute.instantiated option

(* Resolve an expression into a type, ignoring
 * errors related to accessing `None`, `ReadOnly`, and bound `TypeVar`s. *)
val resolve_ignoring_errors
  :  pyre_in_context:PyrePysaApi.InContext.t ->
  callables_to_definitions_map:Target.CallablesSharedMemory.ReadOnly.t ->
  Ast.Expression.t ->
  Type.t

val strip_optional : Type.t -> Type.t

val unbind_type_variable : Type.t -> Type.t

val strip_readonly : Type.t -> Type.t

val extract_coroutine_value : Type.t -> Type.t
