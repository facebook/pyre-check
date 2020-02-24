(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

type t [@@deriving show]

val create
  :  global_resolution:GlobalResolution.t ->
  annotation_store:RefinementUnit.t Reference.Map.t ->
  resolve_expression:(resolution:t -> Expression.t -> Annotation.t) ->
  resolve_statement:(resolution:t -> Statement.t -> t * AnalysisError.t list) ->
  ?parent:Reference.t ->
  unit ->
  t

val resolve_expression : t -> Expression.t -> Type.t

val resolve_expression_to_annotation : t -> Expression.t -> Annotation.t

val resolve_reference : t -> Reference.t -> Type.t

val resolve_statement : t -> Statement.t -> t * AnalysisError.t list

val resolve_assignment : t -> Statement.Assign.t -> t

val resolve_assertion : t -> asserted_expression:Expression.t -> t

val resolve_attribute_access : t -> base_type:Type.t -> attribute:string -> Type.t

val partition_name : t -> name:Expression.Name.t -> Reference.t * Reference.t * Annotation.t option

val set_local : t -> reference:Reference.t -> annotation:Annotation.t -> t

val set_local_with_attributes : t -> name:Expression.Name.t -> annotation:Annotation.t -> t

val get_local : ?global_fallback:bool -> reference:Reference.t -> t -> Annotation.t option

val get_local_with_attributes
  :  ?global_fallback:bool ->
  name:Expression.Name.t ->
  t ->
  Annotation.t option

val unset_local : t -> reference:Reference.t -> t

val is_global : t -> reference:Reference.t -> bool

val add_type_variable : t -> variable:Type.Variable.t -> t

val type_variable_exists : t -> variable:Type.Variable.t -> bool

val all_type_variables_in_scope : t -> Type.Variable.t list

val annotation_store : t -> RefinementUnit.t Reference.Map.t

val with_annotation_store : t -> annotation_store:RefinementUnit.t Reference.Map.t -> t

val parent : t -> Reference.t option

val with_parent : t -> parent:Reference.t option -> t

val is_consistent_with : t -> Type.t -> Type.t -> expression:Ast.Expression.t option -> bool

val global_resolution : t -> GlobalResolution.t
