(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Interprocedural

type t

val from_expression
  :  pyre_in_context:Interprocedural.PyrePysaApi.InContext.t ->
  type_of_expression_shared_memory:Interprocedural.TypeOfExpressionSharedMemory.t ->
  call_graph:CallGraph.DefineCallGraph.t ->
  get_callee_model:(Target.t -> Model.t option) ->
  expression:Expression.t ->
  interval:ClassIntervalSet.t ->
  t

val get_source
  :  type_of_expression_shared_memory:Interprocedural.TypeOfExpressionSharedMemory.t ->
  t ->
  Domains.ForwardState.Tree.t

val get_sinks
  :  type_of_expression_shared_memory:Interprocedural.TypeOfExpressionSharedMemory.t ->
  t ->
  Domains.SinkTreeWithHandle.t list

val get_tito : t -> Domains.BackwardState.Tree.t

val get_sanitize : t -> Sanitize.t

val get_modes : t -> Model.ModeSet.t

val is_sanitized : t -> bool

val global_root : Analysis.TaintAccessPath.Root.t
