(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Analysis
open Interprocedural

val at_callsite
  :  resolution:Resolution.t ->
  call_target:[< Target.t ] ->
  arguments:Expression.Call.Argument.t list ->
  Model.t
