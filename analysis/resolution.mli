(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

type t [@@deriving show]

val create
  :  global_resolution:GlobalResolution.t ->
  imports:Reference.Set.t ->
  annotations:Annotation.t Reference.Map.t ->
  resolve:(resolution:t -> Expression.t -> Annotation.t) ->
  ?parent:Reference.t ->
  unit ->
  t

val add_import : t -> reference:Reference.t -> t

val is_imported : t -> reference:Reference.t -> bool

val set_local : t -> reference:Reference.t -> annotation:Annotation.t -> t

val get_local : ?global_fallback:bool -> reference:Reference.t -> t -> Annotation.t option

val unset_local : t -> reference:Reference.t -> t

val is_global : t -> reference:Reference.t -> bool

val add_type_variable : t -> variable:Type.Variable.t -> t

val type_variable_exists : t -> variable:Type.Variable.t -> bool

val all_type_variables_in_scope : t -> Type.Variable.t list

val annotations : t -> Annotation.t Reference.Map.t

val with_annotations : t -> annotations:Annotation.t Reference.Map.t -> t

val parent : t -> Reference.t option

val with_parent : t -> parent:Reference.t option -> t

val resolve : t -> Expression.t -> Type.t

val resolve_to_annotation : t -> Expression.t -> Annotation.t

val resolve_reference : t -> Reference.t -> Type.t

val resolve_mutable_literals
  :  t ->
  expression:Ast.Expression.t option ->
  resolved:Type.t ->
  expected:Type.t ->
  Type.t

val is_consistent_with : t -> Type.t -> Type.t -> expression:Ast.Expression.t option -> bool

val global_resolution : t -> GlobalResolution.t
