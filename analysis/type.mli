(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression


module Record : sig
  module Callable : sig
    module RecordParameter : sig
      type 'annotation named = {
        name: Access.t;
        annotation: 'annotation;
        default: bool;
      }

      and 'annotation t =
        | Named of 'annotation named
        | Variable of 'annotation named
        | Keywords of 'annotation named
      [@@deriving compare, eq, sexp, show, hash]
    end

    type kind =
      | Anonymous
      | Named of Access.t

    and 'annotation implicit_record = {
      implicit_annotation: 'annotation;
      name: Access.t;
    }

    and 'annotation parameters =
      | Defined of ('annotation RecordParameter.t) list
      | Undefined

    and 'annotation overload = {
      annotation: 'annotation;
      parameters: 'annotation parameters;
    }

    and 'annotation record = {
      kind: kind;
      implementation: 'annotation overload;
      overloads: ('annotation overload) list;
      implicit: ('annotation implicit_record) option;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end
end

type tuple =
  | Bounded of t list
  | Unbounded of t

and constraints =
  | Bound of t
  | Explicit of t list
  | Unconstrained

and variance =
  | Covariant
  | Contravariant
  | Invariant

and typed_dictionary_field = {
  name: string;
  annotation: t;
}

and t =
  | Bottom
  | Callable of t Record.Callable.record
  | Deleted
  | Object
  | Optional of t
  | Parametric of { name: Identifier.t; parameters: t list }
  | Primitive of Identifier.t
  | Top
  | Tuple of tuple
  | TypedDictionary of { name: Identifier.t; fields: typed_dictionary_field list }
  | Union of t list
  | Variable of { variable: Identifier.t; constraints: constraints; variance: variance }
[@@deriving compare, eq, sexp, show]

type type_t = t
[@@deriving compare, eq, sexp, show]

module Map : Map.S with type Key.t = t
module Set: Set.S with type Elt.t = t
include Hashable with type t := t

module Cache: sig
  val enable: unit -> unit
  val disable: unit -> unit
end

module Callable : sig
  module Parameter: sig
    include module type of struct include Record.Callable.RecordParameter end

    type parameter = type_t t
    [@@deriving compare, eq, sexp, show, hash]

    module Map : Core.Map.S with type Key.t = parameter

    val name: parameter -> Identifier.t
    val annotation: parameter -> type_t
    val default: parameter -> bool
    val is_anonymous: parameter -> bool

    val names_compatible: parameter -> parameter -> bool
  end

  include module type of struct include Record.Callable end

  type implicit = type_t Record.Callable.implicit_record
  [@@deriving compare, eq, sexp, show, hash]

  type t = type_t Record.Callable.record
  [@@deriving compare, eq, sexp, show, hash]

  module Overload: sig
    val parameters: type_t overload -> Parameter.parameter list option

    val return_annotation: type_t overload -> type_t
    val is_undefined: type_t overload -> bool
  end

  val from_overloads: t list -> t option

  val map: t -> f:(type_t -> type_t) -> t option

  val with_return_annotation: t -> annotation: type_t -> t
end

module TypedDictionary : sig
  val anonymous: typed_dictionary_field list -> t

  val fields_have_colliding_keys
    : typed_dictionary_field list
    -> typed_dictionary_field list
    -> bool

  val constructor: name: Identifier.t -> fields: typed_dictionary_field list -> Callable.t
  val setter: callable: Callable.t -> annotation: t -> Callable.t
end


val serialize: t -> string

val primitive: string -> t
val parametric: string -> t list -> t
val variable: ?constraints: constraints -> ?variance: variance -> string -> t

val awaitable: t -> t
val bool: t
val bytes: t
val callable
  :  ?name: Access.t
  -> ?overloads: (t Callable.overload) list
  -> ?parameters: t Callable.parameters
  -> ?implicit: Callable.implicit
  -> annotation: t
  -> unit
  -> t
val complex: t
val dictionary: key: t -> value: t -> t
val ellipses: t
val float: t
val generator: ?async: bool -> t -> t
val generic: t
val integer: t
val iterable: t -> t
val iterator: t -> t
val lambda: parameters: (Access.t * t) list -> return_annotation: t -> t
val list: t -> t
val meta: t -> t
val named_tuple: t
val none: t
val optional: t -> t
val sequence: t -> t
val set: t -> t
val string: t
val tuple: t list -> t
val undeclared: t
val union: t list -> t
val yield: t -> t

val create
  :  aliases:(t -> t option)
  -> Expression.t
  -> t

val expression: t -> Expression.t
val access: t -> Access.t

module Transform : sig
  module type Transformer = sig
    type state
    val visit: state -> t -> state * t
    val visit_children: state -> t -> bool
  end

  module Make (Transformer : Transformer) : sig
    val visit: Transformer.state -> t -> Transformer.state * t
  end
end

val exists: t -> predicate: (t -> bool) -> bool

val contains_callable: t -> bool

val is_callable: t -> bool
val is_deleted: t -> bool
val is_ellipses: t -> bool
val is_generator: t -> bool
val is_generic: t -> bool
val is_iterable: t -> bool
val is_iterator: t -> bool
val is_meta: t -> bool
val is_none: t -> bool
val is_noreturn: t -> bool
val is_optional: t -> bool
val is_optional_primitive: t -> bool
val is_primitive: t -> bool
val is_protocol: t -> bool
val is_tuple: t -> bool
val is_typed_dictionary: t -> bool
val is_unbound: t -> bool
val is_unknown: t -> bool
val is_type_alias: t -> bool

(* Contains `Bottom` or variables. *)
val is_not_instantiated: t -> bool

val variables: t -> t list
val primitives: t -> t list
(* Does not contain `Variable`. *)
val is_resolved: t -> bool

val is_partially_typed: t -> bool
val is_untyped: t -> bool

val mismatch_with_any: t -> t -> bool

val optional_value: t -> t
val async_generator_value: t -> t
val awaitable_value: t -> t

val parameters: t -> t list
val single_parameter: t -> t
val split: t -> t * (t list)
val class_name: t -> Access.t

val class_variable: t -> t
val class_variable_value: t -> t option

val assume_any: t -> t
val instantiate: ?widen: bool -> t -> constraints:(t -> t option) -> t
val instantiate_variables: replacement:t -> t -> t

(* Takes a map generated from Preprocessing.dequalify_map and a type and dequalifies the type *)
val dequalify: Access.t Access.Map.t -> t -> t

val to_yojson: t -> Yojson.Safe.json
