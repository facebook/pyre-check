(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Analysis

(* Evaluates to the representation of literal strings, integers and enums. *)
val extract_constant_name : Expression.t -> string option

(* Evaluates to whether the provided expression is a superclass of define. *)
val is_super : resolution:Resolution.t -> define:Statement.Define.t Node.t -> Expression.t -> bool

(* Evaluate to whether a variable is nonlocal to a given define *)
val is_nonlocal : resolution:Resolution.t -> define:Reference.t -> Reference.t -> bool

(* Check whether `successor` extends `predecessor`.
 * Returns false on untracked types.
 * Returns `reflexive` if `predecessor` and `successor` are equal. *)
val is_transitive_successor_ignoring_untracked
  :  GlobalResolution.t ->
  reflexive:bool ->
  predecessor:string ->
  successor:string ->
  bool

(* Resolve an expression into a type. Untracked types are resolved into `Any`. *)
val resolve_ignoring_untracked : resolution:Resolution.t -> Ast.Expression.t -> Type.t

(* Resolve an attribute access into a type. Untracked types are resolved into `Any`. *)
val resolve_attribute_access_ignoring_untracked
  :  resolution:Resolution.t ->
  base_type:Type.t ->
  attribute:string ->
  Type.t

val defining_attribute
  :  resolution:Resolution.t ->
  Type.t ->
  string ->
  AnnotatedAttribute.instantiated_annotation AnnotatedAttribute.t option

(* Resolve an expression into a type, ignoring
 * errors related to accessing `None`, `ReadOnly`, and bound `TypeVar`s. *)
val resolve_ignoring_errors : resolution:Resolution.t -> Ast.Expression.t -> Type.t

val strip_optional : Type.t -> Type.t

val unbind_type_variable : Type.t -> Type.t

val strip_readonly : Type.t -> Type.t

val extract_coroutine_value : Type.t -> Type.t
