(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Statement


type t
[@@deriving compare, eq, sexp, show, hash]

type decorator = {
  access: string;
  arguments: (Expression.t Expression.Call.Argument.t list) option
}
[@@deriving compare, eq, sexp, show, hash]

val name_equal: t -> t -> bool

type class_t = t
[@@deriving compare, eq, sexp, show, hash]

val create: Class.t Node.t -> t

val name: t -> Reference.t
val bases: t -> Expression.t Expression.Call.Argument.t list
val get_decorator: t -> decorator: string -> decorator list

val annotation: t -> Type.t
val successors: t -> resolution: Resolution.t -> Type.primitive list
val metaclass: t -> resolution: Resolution.t -> Type.t

module Method : sig
  type t
  [@@deriving compare, eq, sexp, show, hash]

  val create: define: Define.t -> parent: Type.t -> t

  val name: t -> Identifier.t

  val define: t -> Define.t
  val parent: t -> Type.t

  val parameter_annotations
    :  t
    -> resolution: Resolution.t
    -> Type.t Identifier.Map.t
  val parameter_annotations_positional
    :  t
    -> resolution: Resolution.t
    -> Type.t Int.Map.t
  val return_annotation: t -> resolution: Resolution.t -> Type.t

end

val generics: t -> resolution: Resolution.t -> Type.t list
(* Find free variables in the parametric type. E.g. for generic class
    `class A(typing.Generic[_T], typing.Generic[_S]): ...`
    and instantiated type `A[int, Bottom]` we consider `_S` to be free. *)

val inferred_generic_base
  :  t
  -> resolution: Resolution.t
  -> Expression.t Expression.Call.Argument.t list

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


val methods: t -> Method.t list

val is_protocol: t -> bool

val callable_implements
  :  resolution: Resolution.t
  -> Type.Callable.t
  -> protocol: t
  -> TypeOrder.implements_result
val implements: resolution: Resolution.t -> t -> protocol: t -> TypeOrder.implements_result

module Attribute : sig
  type attribute = {
    name: Identifier.t;
    parent: Type.t;
    annotation: Annotation.t;
    value: Expression.t;
    defined: bool;
    class_attribute: bool;
    async: bool;
    initialized: bool;
    property: bool;
  }
  [@@deriving eq, show]

  type t = attribute Node.t
  [@@deriving eq, show]

  val create
    :  resolution: Resolution.t
    -> parent: class_t
    -> ?instantiated: Type.t
    -> ?defined: bool
    -> ?inherited: bool
    -> ?default_class_attribute: bool
    -> Statement.Attribute.t
    -> t

  val name: t -> Identifier.t
  val async: t -> bool

  val annotation: t -> Annotation.t
  val parent: t -> Type.t
  val value: t -> Expression.t
  val initialized: t -> bool
  val location: t -> Location.t
  val defined: t -> bool
  val class_attribute: t -> bool

  val instantiate: t -> constraints: Type.t Type.Map.t -> t

  module Cache: sig
    val clear: unit -> unit
  end
end

val attributes
  :  ?transitive: bool
  -> ?class_attributes: bool
  -> ?include_generated_attributes: bool
  -> ?instantiated: Type.t option
  -> t
  -> resolution: Resolution.t
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
  -> name: Identifier.t
  -> instantiated: Type.t
  -> Attribute.t

(* Attribute defined by `__getattr__`. *)
val fallback_attribute: resolution: Resolution.t -> name: Identifier.t -> t -> Attribute.t option

val constructor: t -> instantiated:Type.t -> resolution: Resolution.t -> Type.t

val overrides: t -> resolution: Resolution.t -> name: Identifier.t -> Attribute.t option

val has_method: ?transitive: bool -> t -> resolution: Resolution.t -> name: Identifier.t -> bool

val inferred_callable_type: t -> resolution: Resolution.t -> Type.t option
