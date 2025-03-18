(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Interprocedural
module PyrePysaEnvironment = Analysis.PyrePysaEnvironment

type t

val from_expression
  :  pyre_in_context:PyrePysaEnvironment.InContext.t ->
  call_graph:CallGraph.DefineCallGraph.t ->
  get_callee_model:(Target.t -> Model.t option) ->
  expression:Expression.t ->
  interval:ClassIntervalSet.t ->
  t

val get_source : t -> Domains.ForwardState.Tree.t

val get_sinks : t -> Domains.SinkTreeWithHandle.t list

val get_tito : t -> Domains.BackwardState.Tree.t

val get_sanitize : t -> Sanitize.t

val get_modes : t -> Model.ModeSet.t

val is_sanitized : t -> bool

val global_root : AccessPath.Root.t
