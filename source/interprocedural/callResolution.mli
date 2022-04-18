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

(* Check whether `successor` extends `predecessor`. Returns false on untracked types. *)
val is_transitive_successor_ignoring_untracked
  :  GlobalResolution.t ->
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
  Annotated.Attribute.instantiated_annotation Annotated.Attribute.t option

(* Resolve an expression into a type, ignoring errors related to accessing `None`. *)
val resolve_ignoring_optional : resolution:Resolution.t -> Ast.Expression.t -> Type.t
