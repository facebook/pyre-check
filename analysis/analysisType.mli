(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression


module Record : sig
  module Callable : sig
    module Parameter : sig
      type 'annotation named = {
        name: Access.t;
        annotation: 'annotation;
        default: bool;
      }

      and 'annotation t =
        | Anonymous of 'annotation
        | Named of 'annotation named
        | Variable of 'annotation named
        | Keywords of 'annotation named
      [@@deriving compare, eq, sexp, show, hash]
    end

    type kind =
      | Anonymous
      | Named of Access.t

    and 'annotation parameters =
      | Defined of ('annotation Parameter.t) list
      | Undefined

    and 'annotation overload = {
      annotation: 'annotation;
      parameters: 'annotation parameters;
    }

    and implicit =
      | Class
      | Instance
      | Function

    and 'annotation record = {
      kind: kind;
      overloads: ('annotation overload) list;
      implicit: implicit;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end
end

type parametric = {
  name: Identifier.t;
  parameters: t list;
}

and tuple =
  | Bounded of t list
  | Unbounded of t

and variable = {
  variable: Identifier.t;
  constraints: t list;
}

and t =
  | Bottom
  | Callable of t Record.Callable.record
  | Object
  | Optional of t
  | Parametric of parametric
  | Primitive of Identifier.t
  | Top
  | Tuple of tuple
  | Union of t list
  | Variable of variable
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

val serialize: t -> string

val primitive: string -> t
val parametric: string -> t list -> t
val variable: ?constraints: t list -> string -> t

val awaitable: t -> t
val bool: t
val bytes: t
val callable
  :  ?name: Access.t
  -> ?overloads: (t Record.Callable.overload) list
  -> ?parameters: t Record.Callable.parameters
  -> annotation: t
  -> unit
  -> t
val complex: t
val dictionary: key:t -> value:t -> t
val float: t
val generator: ?async:bool -> t -> t
val generic: t
val integer: t
val iterable: t -> t
val iterator: t -> t
val lambda: parameters: t list -> return_annotation: t -> t
val list: t -> t
val optional: t -> t
val meta: t -> t
val sequence: t -> t
val set: t -> t
val string: t
val tuple: t list -> t
val unbound: t
val union: t list -> t
val none: t
val yield: t -> t

val create
  :  aliases:(t -> t option)
  -> Expression.t
  -> t

val expression: t -> Expression.t
val access: t -> Access.t

val exists: t -> predicate: (t -> bool) -> bool

val contains_callable: t -> bool

val is_callable:t -> bool
val is_generator: t -> bool
val is_awaitable: t -> bool
val is_generic: t -> bool
val is_meta: t -> bool
val is_none: t -> bool
val is_optional: t -> bool
val is_optional_primitive: t -> bool
val is_primitive: t -> bool
val is_protocol: t -> bool
val is_tuple: t -> bool
val is_unknown: t -> bool

(* Contains `Bottom` or variables. *)
val is_not_instantiated: t -> bool

val variables: t -> t list
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

(* Takes a map generated from Preprocessing.dequalify_map and a type and dequalifies the type *)
val dequalify: Access.t Access.Map.t -> t -> t

module Callable : sig
  include module type of struct include Record.Callable end

  type t = type_t Record.Callable.record
  [@@deriving compare, eq, sexp, show, hash]

  val from_overloads: t list -> t option

  val map: t -> f:(type_t -> type_t) -> t option
end
