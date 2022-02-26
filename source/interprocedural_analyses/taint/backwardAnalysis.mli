(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Analysis
open Statement

val run
  :  ?profiler:TaintProfiler.t ->
  environment:TypeEnvironment.ReadOnly.t ->
  qualifier:Reference.t ->
  define:Define.t Node.t ->
  call_graph_of_define:Interprocedural.CallGraph.DefineCallGraph.t ->
  existing_model:Model.t ->
  triggered_sinks:ForwardAnalysis.triggered_sinks ->
  Model.Backward.t
