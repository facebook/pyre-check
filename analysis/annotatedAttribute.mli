(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast

type read_only =
  | Refinable of { overridable: bool }
  | Unrefinable
[@@deriving eq, show, compare, sexp]

type visibility =
  | ReadOnly of read_only
  | ReadWrite
[@@deriving eq, show, compare, sexp]

type attribute = {
  abstract: bool;
  annotation: Type.t;
  original_annotation: Type.t;
  async: bool;
  class_attribute: bool;
  defined: bool;
  initialized: bool;
  name: Identifier.t;
  parent: Type.t;
  visibility: visibility;
  property: bool;
  static: bool;
  value: Expression.t;
}
[@@deriving eq, show, compare, sexp]

type t = attribute Node.t [@@deriving eq, show]

val name : t -> Identifier.t

val abstract : t -> bool

val async : t -> bool

val annotation : t -> Annotation.t

val parent : t -> Type.t

val value : t -> Expression.t

val initialized : t -> bool

val location : t -> Location.t

val defined : t -> bool

val class_attribute : t -> bool

val static : t -> bool

val property : t -> bool

val instantiate : t -> constraints:(Type.t -> Type.t option) -> t

module Table : sig
  type element = t

  type t [@@deriving compare]

  val create : unit -> t

  val add : t -> element -> unit

  val lookup_name : t -> string -> element option

  val to_list : t -> element list

  val clear : t -> unit

  val filter_map : f:(element -> element option) -> t -> unit
end
