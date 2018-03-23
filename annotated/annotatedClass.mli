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

type parent_class = t
[@@deriving compare, eq, sexp, show, hash]

val create: Class.t Node.t -> t

val name: t -> Access.t
val bases: t -> (Expression.t Argument.t) list
val body: t -> Statement.t list

val annotation: t -> resolution: Resolution.t -> Type.t

module Method : sig
  type t
  [@@deriving compare, eq, sexp, show, hash]

  val create: define: Define.t -> parent: parent_class -> t

  val name: t -> Access.t

  val define: t -> Define.t
  val parent: t -> parent_class

  val parameter_annotations
    :  t
    -> resolution: Resolution.t
    -> Type.t Identifier.Map.t
  val parameter_annotations_positional
    :  t
    -> resolution: Resolution.t
    -> Type.t Int.Map.t
  val return_annotation: t -> resolution: Resolution.t -> Type.t

  val overrides: t -> resolution: Resolution.t -> t option

  val implements: t -> protocol_method: t -> bool
end

val generics: t -> resolution: Resolution.t -> Type.t list
(* Find free variables in the parametric type. E.g. for generic class
    `class A(typing.Generic[_T], typing.Generic[_S]): ...`
    and instantiated type `A[int, Bottom]` we consider `_S` to be free. *)
val free_variables
  :  t
  -> resolution: Resolution.t
  -> parameters: Type.t list
  -> (Type.t option) list

val inferred_generic_base
  :  t
  -> aliases: (Type.t -> Type.t option)
  -> (Expression.t Argument.t) list

val constraints
  :  ?target: t
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
  type t = {
    name: Expression.expression;
    parent: parent_class;
    annotation: Annotation.t;
    value: Expression.t option;
    location: Location.t;
    defined: bool;
    class_attribute: bool;
    async: bool;
  }
  [@@deriving eq, show]

  val create
    :  resolution: Resolution.t
    -> parent: parent_class
    -> ?defined: bool
    -> ?default_class_attribute: bool
    -> Statement.Attribute.t
    -> t

  val name: t -> Expression.expression
  val access: t -> Access.t
  val async: t -> bool

  val annotation: t -> Annotation.t
  val parent: t -> parent_class
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
