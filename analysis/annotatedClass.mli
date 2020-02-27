(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Statement
module StatementAttribute = Attribute
module Attribute = AnnotatedAttribute

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
