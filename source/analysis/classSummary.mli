(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Statement

module Attribute : sig
  type getter_property = {
    self: Expression.t option;
    return: Expression.t option;
  }
  [@@deriving compare, sexp, show, hash]

  type setter_property = {
    self: Expression.t option;
    value: Expression.t option;
  }
  [@@deriving compare, sexp, show, hash]

  type property_kind =
    | ReadOnly of { getter: getter_property }
    | ReadWrite of {
        getter: getter_property;
        setter: setter_property;
      }
  [@@deriving compare, sexp, show, hash]

  type origin =
    | Explicit
    | Implicit
  [@@deriving compare, sexp, show, hash]

  type value_and_origin = {
    value: Expression.t;
    origin: origin;
  }
  [@@deriving compare, sexp, show, hash]

  type simple = {
    annotation: Expression.t option;
    values: value_and_origin list;
    primitive: bool;
    frozen: bool;
    toplevel: bool;
    implicit: bool;
    nested_class: bool;
  }
  [@@deriving compare, sexp, show, hash]

  type method_ = {
    signatures: Define.Signature.t list;
    static: bool;
    final: bool;
  }
  [@@deriving compare, sexp, show, hash]

  type property = {
    async: bool;
    class_property: bool;
    kind: property_kind;
  }
  [@@deriving compare, sexp, show, hash]

  type kind =
    | Simple of simple
    | Method of method_
    | Property of property
  [@@deriving compare, sexp, show, hash]

  type attribute = {
    kind: kind;
    name: Identifier.t;
  }
  [@@deriving compare, sexp, show, hash]

  type t = attribute Node.t [@@deriving compare, sexp, show, hash]

  val location_insensitive_compare : t -> t -> int

  val location_insensitive_compare_kind : kind -> kind -> int
end

module ClassAttributes : sig
  type t [@@deriving compare, sexp, show, hash]

  val create : Class.t -> t

  val empty : unit -> t

  (* Exposed for testing only *)
  module Private : sig
    val assigned_by_define
      :  Define.t ->
      definition:Class.t ->
      Attribute.t Identifier.SerializableMap.t
  end
end

type bases = {
  base_classes: Expression.t list;
  metaclass: Expression.t option;
  init_subclass_arguments: Expression.Call.Argument.t list;
}
[@@deriving compare, sexp, show, hash, to_yojson]

type t = {
  name: Reference.t;
  qualifier: Reference.t;
  bases: bases;
  decorators: Expression.t list;
  class_attributes: ClassAttributes.t;
}
[@@deriving compare, sexp, show, hash]

val create : qualifier:Reference.t -> Class.t -> t

val is_protocol : t -> bool

val has_decorator : t -> string -> bool

val is_final : t -> bool

val is_abstract : t -> bool

val fields_tuple_value : t -> string list option

val name : t -> Reference.t

val bases : t -> bases

val base_classes : t -> Expression.t list

val constructor_attributes : t -> Attribute.t Identifier.SerializableMap.t

val attributes
  :  ?include_generated_attributes:bool ->
  ?in_test:bool ->
  t ->
  Attribute.t Identifier.SerializableMap.t
