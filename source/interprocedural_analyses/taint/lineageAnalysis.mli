(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Domains
open Ast
open Expression
open Analysis

val forward_analyze_call
  :  analyze_expression:
       (resolution:Resolution.t ->
       state:ForwardState.t ->
       expression:Expression.t ->
       ForwardState.Tree.t * ForwardState.t) ->
  resolution:Resolution.t ->
  callee:expression Node.t ->
  arguments:Call.Argument.t list ->
  taint:ForwardState.Tree.t ->
  state:ForwardState.t ->
  ForwardState.Tree.t * ForwardState.t
