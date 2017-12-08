(** Copyright 2016-present Facebook. All rights reserved. **)

open Ast
open Expression
open Statement


type t

val create
  :  annotations: Annotation.t Instantiated.Access.Map.t
  -> order: (module TypeOrder.Reader)
  -> resolve: (resolution: t -> Expression.t -> Type.t)
  -> parse_annotation: (Expression.t -> Type.t)
  -> global: (access -> Annotation.t option)
  -> class_definition:(Type.t -> (Statement.t Class.t) option)
  -> function_signature:
       (access
        -> Expression.t Call.t
        -> Signature.argument list
        -> Signature.t list)
  -> method_signature:
       (resolution: t
        -> Type.t
        -> Expression.t Call.t
        -> Signature.argument list
        -> Signature.t list)
  -> t
val with_annotations: t -> Annotation.t Instantiated.Access.Map.t -> t

val annotations: t -> Annotation.t Instantiated.Access.Map.t
val order: t -> (module TypeOrder.Reader)

val resolve: t -> Expression.t -> Type.t
val parse_annotation: t -> Expression.t -> Type.t

val global: t -> access -> Annotation.t option
val class_definition: t -> Type.t -> (Statement.t Class.t) option

val function_signature
  :  t
  -> access
  -> Expression.t Call.t
  -> Signature.argument list
  -> Signature.t list
val method_signature
  :  t
  -> Type.t
  -> Expression.t Call.t
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
