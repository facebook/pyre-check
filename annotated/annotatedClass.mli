(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Statement

module Annotation = AnalysisAnnotation
module Resolution = AnalysisResolution
module Type = AnalysisType


type t
[@@deriving compare, eq, sexp, show, hash]

val name_equal: t -> t -> bool

type class_t = t
[@@deriving compare, eq, sexp, show, hash]

val create: Class.t Node.t -> t

val name: t -> Access.t
val bases: t -> Argument.t list
val body: t -> Statement.t list

val annotation: t -> resolution: Resolution.t -> Type.t

module Method : sig
  type t
  [@@deriving compare, eq, sexp, show, hash]

  val create: define: Define.t -> parent: class_t -> t

  val name: t -> Access.t

  val define: t -> Define.t
  val parent: t -> class_t

  val parameter_annotations
    :  t
    -> resolution: Resolution.t
    -> Type.t Identifier.Map.t
  val parameter_annotations_positional
    :  t
    -> resolution: Resolution.t
    -> Type.t Int.Map.t
  val return_annotation: t -> resolution: Resolution.t -> Type.t

  val overloads: t -> resolution: Resolution.t -> t option

  val implements: t -> protocol_method: t -> bool
end

val generics: t -> resolution: Resolution.t -> Type.t list
(* Find free variables in the parametric type. E.g. for generic class
    `class A(typing.Generic[_T], typing.Generic[_S]): ...`
    and instantiated type `A[int, Bottom]` we consider `_S` to be free. *)

val inferred_generic_base
  :  t
  -> aliases: (Type.t -> Type.t option)
  -> Argument.t list

val constraints
  :  ?target: t
  -> ?parameters: Type.t list
  -> t
  -> instantiated: Type.t
  -> resolution: Resolution.t
  -> Type.t Type.Map.t

val superclasses
  :  t
  -> resolution: Resolution.t
  -> t list
val immediate_superclasses
  :  t
  -> resolution: Resolution.t
  -> t option

val constructors: t -> resolution: Resolution.t -> Define.t list
val methods: t -> Method.t list

val is_protocol: t -> bool
val implements: t -> protocol: t -> bool

module Attribute : sig
  type attribute = {
    name: Expression.expression;
    parent: class_t;
    annotation: Annotation.t;
    value: Expression.t option;
    defined: bool;
    class_attribute: bool;
    async: bool;
  }
  [@@deriving eq, show]

  type t = attribute Node.t
  [@@deriving eq, show]

  val create
    :  resolution: Resolution.t
    -> parent: class_t
    -> ?defined: bool
    -> ?default_class_attribute: bool
    -> Statement.Attribute.t
    -> t

  val name: t -> Expression.expression
  val access: t -> Access.t
  val async: t -> bool

  val annotation: t -> Annotation.t
  val parent: t -> class_t
  val value: t -> Expression.t option
  val location: t -> Location.t
  val defined: t -> bool
  val class_attribute: t -> bool

  val instantiate: t -> constraints:Type.t Type.Map.t -> t
end

module AttributesCache : sig
  val clear: unit -> unit
end

val attributes
  :  ?transitive: bool
  -> ?class_attributes: bool
  -> ?include_generated_attributes: bool
  -> t
  -> resolution:Resolution.t
  -> Attribute.t list
val attribute_fold
  :  ?transitive: bool
  -> ?class_attributes: bool
  -> ?include_generated_attributes: bool
  -> t
  -> initial: 'accumulator
  -> f: ('accumulator -> Attribute.t -> 'accumulator)
  -> resolution: Resolution.t
  -> 'accumulator
val attribute
  :  ?transitive: bool
  -> ?class_attributes: bool
  -> t
  -> resolution: Resolution.t
  -> name: Access.t
  -> instantiated: Type.t
  -> Attribute.t

(* Attribute defined by `__getattr__`. *)
val fallback_attribute: resolution: Resolution.t -> access: Access.t -> t -> Attribute.t option
