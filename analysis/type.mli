(** Copyright 2016-present Facebook. All rights reserved. **)

open Core

open Ast
open Expression


type parametric =
  {
    name: Identifier.t;
    parameters: t list;
  }

and tuple =
  | Bounded of t list
  | Unbounded of t

and variable =
  {
    variable: Identifier.t;
    constraints: t list;
  }

and t =
  | Bottom
  | Object
  | Optional of t
  | Parametric of parametric
  | Primitive of Identifier.t
  | Top
  | Tuple of tuple
  | Union of t list
  | Variable of variable
[@@deriving compare, eq, sexp, show]

module Map : Map.S with type Key.t = t
module Set: Set.S with type Elt.t = t
include Hashable with type t := t

val awaitable: t -> t
val bool: t
val bytes: t
val complex: t
val dictionary: key:t -> value:t -> t
val float: t
val generator: t -> t
val generic: t
val integer: t
val iterable: t -> t
val iterator: t -> t
val lambda: t -> t
val list: t -> t
val optional: t -> t
val parametric: string -> t list -> t
val set: t -> t
val string: t
val tuple: t list -> t
val union: t list -> t
val void: t
val yield: t -> t

val create
  :  aliases:(t -> t option)
  -> Expression.t
  -> t

val expression: t -> Expression.t


val is_awaitable: t -> bool
val is_bottom: t -> bool
val is_generic: t -> bool
val is_meta: t -> bool
val is_optional: t -> bool
val is_primitive: t -> bool
val is_tuple: t -> bool
val is_unknown: t -> bool

val mismatch_with_any: t -> t -> bool

val optional_value: t -> t
val awaitable_value: t -> t

val parameters: t -> t list
val split: t -> t * (t list)
val class_variable: t -> t option

val assume_any: t -> t
val instantiate: t -> constraints:(t -> t option) -> t

(* Takes a map generated from Preprocessing.dequalify_map and a type and dequalifies the type *)
val dequalify: access Instantiated.Access.Map.t -> t -> t

module Build : sig
  val create
    :  aliases:(t -> t option)
    -> Expression.t
    -> t
end
