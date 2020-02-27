(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Statement
module StatementAttribute = Attribute
module Attribute = AnnotatedAttribute

type t = ClassSummary.t Node.t [@@deriving compare, eq, sexp, show, hash]

val create : ClassSummary.t Node.t -> t

val name : t -> Reference.t

val bases : t -> Expression.Call.Argument.t list

val annotation : t -> Type.t

val has_abstract_base : t -> bool

val get_abstract_attributes
  :  resolution:GlobalResolution.t ->
  Type.Primitive.t ->
  AttributeResolution.uninstantiated_attribute list

(* Attribute defined by `__getattr__`. *)
val fallback_attribute
  :  resolution:Resolution.t ->
  name:Identifier.t ->
  Type.Primitive.t ->
  Attribute.instantiated option

val overrides
  :  Type.Primitive.t ->
  resolution:GlobalResolution.t ->
  name:Identifier.t ->
  Attribute.instantiated option
