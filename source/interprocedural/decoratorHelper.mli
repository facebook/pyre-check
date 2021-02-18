(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Analysis
open Statement

val all_decorators : TypeEnvironment.ReadOnly.t -> Reference.t list

val all_decorator_bodies : TypeEnvironment.ReadOnly.t -> Define.t Reference.Map.t

val inline_decorators
  :  environment:TypeEnvironment.ReadOnly.t ->
  decorator_bodies:Define.t Reference.Map.t ->
  Source.t ->
  Source.t

val sanitize_defines : strip_decorators:bool -> Source.t -> Source.t
