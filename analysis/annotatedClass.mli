(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Statement
module Attribute = AnnotatedAttribute


type t
[@@deriving compare, eq, sexp, show, hash]

type decorator = {
  name: string;
  arguments: (Expression.t Expression.Call.Argument.t list) option
}
[@@deriving compare, eq, sexp, show, hash]

type class_data = {
  instantiated: Type.t;
  class_attributes: bool;
  class_definition: t;
}

val name_equal: t -> t -> bool

val create: Class.t Node.t -> t

val name: t -> Reference.t
val bases: t -> Expression.t Expression.Call.Argument.t list
val get_decorator: t -> decorator: string -> decorator list

val annotation: t -> Type.t
val successors: t -> resolution: Resolution.t -> Type.primitive list
val successors_fold:
  'a Class.record Node.t ->
  resolution:Resolution.t ->
  f:('b -> string -> 'b) ->
  initial:'b ->
  'b

val metaclass: t -> resolution: Resolution.t -> Type.t

val resolve_class: resolution: Resolution.t -> Type.t -> class_data list option

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
    -> (Identifier.t * Type.t) list

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

val methods: t -> Method.t list
val is_protocol: t -> bool



val create_attribute
  :  resolution: Resolution.t
  -> parent: t
  -> ?instantiated: Type.t
  -> ?defined: bool
  -> ?inherited: bool
  -> ?default_class_attribute: bool
  -> Statement.Attribute.t
  -> Attribute.t

val attributes
  :  ?transitive: bool
  -> ?class_attributes: bool
  -> ?include_generated_attributes: bool
  -> ?instantiated: Type.t
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
  -> AnnotatedAttribute.t

(* Attribute defined by `__getattr__`. *)
val fallback_attribute: resolution: Resolution.t -> name: Identifier.t -> t -> Attribute.t option

val constructor: t -> instantiated:Type.t -> resolution: Resolution.t -> Type.t

val overrides: t -> resolution: Resolution.t -> name: Identifier.t -> Attribute.t option

val has_method: ?transitive: bool -> t -> resolution: Resolution.t -> name: Identifier.t -> bool

val inferred_callable_type: t -> resolution: Resolution.t -> Type.t option

module AttributeCache: sig
  val clear: unit -> unit
end
