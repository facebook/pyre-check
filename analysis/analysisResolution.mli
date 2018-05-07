(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression
open Statement

module Annotation = AnalysisAnnotation
module Type = AnalysisType
module TypeOrder = AnalysisTypeOrder


type global = Annotation.t Node.t
[@@deriving eq, show]

type t

val create
  :  annotations: Annotation.t Access.Map.t
  -> order: (module TypeOrder.Handler)
  -> resolve: (resolution: t -> Expression.t -> Type.t)
  -> resolve_literal: (resolution: t -> Expression.t -> Type.t)
  -> parse_annotation: (Expression.t -> Type.t)
  -> global: (Access.t -> global option)
  -> module_definition: (Access.t -> Module.t option)
  -> class_definition:(Type.t -> (Class.t Node.t) option)
  -> t

val set_local: t -> access: Access.t -> annotation: Annotation.t -> t
val get_local: t -> access: Access.t -> Annotation.t option
val get_local_callable: t -> access: Access.t -> Type.Callable.t option

val annotations: t -> Annotation.t Access.Map.t
val with_annotations: t -> annotations: Annotation.t Access.Map.t -> t

val with_define: t -> Statement.Define.t -> t
val define: t -> Statement.Define.t option
val order: t -> (module TypeOrder.Handler)

val resolve: t -> Expression.t -> Type.t
val resolve_literal: t -> Expression.t -> Type.t
val parse_annotation: t -> Expression.t -> Type.t

val global: t -> Access.t -> global option

val module_definition: t -> Access.t -> Module.t option
val class_definition: t -> Type.t -> (Class.t Node.t) option

val less_or_equal: t -> left:Type.t -> right:Type.t -> bool
val join: t -> Type.t -> Type.t -> Type.t
val meet: t -> Type.t -> Type.t -> Type.t
val widen
  :  t
  -> widening_threshold: int
  -> previous: Type.t
  -> next: Type.t
  -> iteration: int
  -> Type.t
