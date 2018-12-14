(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Statement


type global = Annotation.t Node.t
[@@deriving eq, show]

type class_representation = {
  class_definition: Class.t Node.t;
  successors: Type.t list;
  explicit_attributes: Attribute.t Access.SerializableMap.t;
  implicit_attributes: Attribute.t Access.SerializableMap.t;
  is_test: bool;
  methods: Type.t list;
}

type t
[@@deriving show]

val create
  :  annotations: Annotation.t Access.Map.t
  -> order: (module TypeOrder.Handler)
  -> resolve: (resolution: t -> Expression.t -> Type.t)
  -> parse_annotation: (Expression.t -> Type.t)
  -> global: (Access.t -> global option)
  -> module_definition: (Access.t -> Module.t option)
  -> class_definition: (Type.t -> (Class.t Node.t) option)
  -> class_representation: (Type.t -> class_representation option)
  -> ?parent: Access.t
  -> unit
  -> t

val set_local: t -> access: Access.t -> annotation: Annotation.t -> t
val get_local: ?global_fallback: bool -> access: Access.t -> t -> Annotation.t option

val annotations: t -> Annotation.t Access.Map.t
val with_annotations: t -> annotations: Annotation.t Access.Map.t -> t

val parent: t -> Access.t option
val with_parent: t -> parent: Access.t option -> t

val order: t -> (module TypeOrder.Handler)

val resolve: t -> Expression.t -> Type.t
val resolve_literal: t -> Expression.t -> Type.t
val parse_annotation: t -> Expression.t -> Type.t
val parse_meta_annotation: t -> Expression.t -> Type.t option

val global: t -> Access.t -> global option

val module_definition: t -> Access.t -> Module.t option
val class_definition: t -> Type.t -> (Class.t Node.t) option
val class_representation: t -> Type.t -> class_representation option

val less_or_equal: t -> left: Type.t -> right: Type.t -> bool
val join: t -> Type.t -> Type.t -> Type.t
val meet: t -> Type.t -> Type.t -> Type.t
val widen
  :  t
  -> widening_threshold: int
  -> previous: Type.t
  -> next: Type.t
  -> iteration: int
  -> Type.t
val is_instantiated: t -> Type.t -> bool
val is_tracked: t -> Type.t -> bool
val is_invariance_mismatch: t -> left: Type.t -> right: Type.t -> bool
