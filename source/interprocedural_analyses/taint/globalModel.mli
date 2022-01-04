(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Analysis
open Interprocedural

type t

val from_expression
  :  resolution:Resolution.t ->
  call_graph:CallGraph.DefineCallGraph.t ->
  qualifier:Reference.t ->
  expression:Expression.t ->
  t

val get_source : t -> Domains.ForwardState.Tree.t

val get_sink : t -> Domains.BackwardState.Tree.t

val get_tito : t -> Domains.BackwardState.Tree.t

val get_sanitize : t -> Domains.Sanitize.t

val get_modes : t -> Model.ModeSet.t

val is_sanitized : t -> bool
