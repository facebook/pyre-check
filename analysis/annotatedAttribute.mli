(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Core


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

val name: t -> Identifier.t
val async: t -> bool

val annotation: t -> Annotation.t
val parent: t -> Type.t
val value: t -> Expression.t
val initialized: t -> bool
val location: t -> Location.t
val defined: t -> bool
val class_attribute: t -> bool

val instantiate: t -> constraints: (Type.t -> Type.t option) -> t

module Cache: sig
  type t = {
    transitive: bool;
    class_attributes: bool;
    include_generated_attributes: bool;
    name: Reference.t;
    instantiated: Type.t;
  }
  [@@deriving compare, sexp, hash]
  include Hashable with type t := t
  val cache: attribute Node.t list Table.t
  val clear: unit -> unit
end
