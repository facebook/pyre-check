(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Statement
module StatementAttribute = Attribute
module Attribute = AnnotatedAttribute

type t = ClassSummary.t Node.t [@@deriving compare, eq, sexp, show, hash]

type global_resolution = GlobalResolution.t

val name_equal : t -> t -> bool

val create : ClassSummary.t Node.t -> t

val name : t -> Reference.t

val bases : t -> Expression.Call.Argument.t list

val annotation : t -> Type.t

val is_unit_test : t -> bool

val has_abstract_base : t -> bool

val get_abstract_attributes : resolution:GlobalResolution.t -> t -> AnnotatedAttribute.t list

val implicit_attributes : t -> StatementAttribute.t Identifier.SerializableMap.t

(* Attribute defined by `__getattr__`. *)
val fallback_attribute : resolution:Resolution.t -> name:Identifier.t -> t -> Attribute.t option

val has_explicit_constructor : t -> resolution:GlobalResolution.t -> bool

val overrides : t -> resolution:GlobalResolution.t -> name:Identifier.t -> Attribute.t option
