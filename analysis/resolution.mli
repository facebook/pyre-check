(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression
open Statement


type global = {
  annotation: Annotation.t;
  location: Location.t;
}
[@@deriving show]


type t

val create
  :  annotations: Annotation.t Access.Map.t
  -> order: (module TypeOrder.Handler)
  -> resolve: (resolution: t -> Expression.t -> Type.t)
  -> parse_annotation: (Expression.t -> Type.t)
  -> global: (Access.t -> global option)
  -> class_definition:(Type.t -> (Class.t Node.t) option)
  -> function_signature:
       (Access.t
        -> Call.t
        -> Signature.argument list
        -> Signature.t list)
  -> method_signature:
       (resolution: t
        -> Type.t
        -> Call.t
        -> Signature.argument list
        -> Signature.t list)
  -> t
val with_annotations: t -> Annotation.t Access.Map.t -> t
val with_define: t -> Statement.Define.t -> t

val annotations: t -> Annotation.t Access.Map.t
val define: t -> Statement.Define.t option
val order: t -> (module TypeOrder.Handler)

val resolve: t -> Expression.t -> Type.t
val parse_annotation: t -> Expression.t -> Type.t

val global: t -> Access.t -> global option
val class_definition: t -> Type.t -> (Class.t Node.t) option

val function_signature
  :  t
  -> Access.t
  -> Call.t
  -> Signature.argument list
  -> Signature.t list
val method_signature
  :  t
  -> Type.t
  -> Call.t
  -> Signature.argument list
  -> Signature.t list

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
