(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Domains
open Ast
open Expression

val forward_analyze_call
  :  expression Node.t ->
  Call.Argument.t list ->
  ForwardState.Tree.t ->
  ForwardState.t ->
  ForwardState.Tree.t * ForwardState.t
