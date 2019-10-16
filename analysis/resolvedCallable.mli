(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Statement

val create_callable
  :  resolution:GlobalResolution.t ->
  parent:Type.t option ->
  name:Identifier.t ->
  (bool * Type.t Type.Callable.overload) list ->
  Type.Callable.t

val apply_decorators
  :  resolution:GlobalResolution.t ->
  Define.Signature.t Node.t ->
  Type.t Type.Callable.overload
