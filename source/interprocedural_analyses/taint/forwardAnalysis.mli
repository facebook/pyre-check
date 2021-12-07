(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Analysis
open Statement

type triggered_sinks = (AccessPath.Root.t * Sinks.t) list Location.Table.t

val run
  :  ?profiler:TaintProfiler.t ->
  environment:TypeEnvironment.ReadOnly.t ->
  qualifier:Reference.t ->
  define:Define.t Node.t ->
  call_graph_of_define:Interprocedural.CallGraph.DefineCallGraph.t ->
  existing_model:Model.t ->
  Model.Forward.t * Flow.issue list * triggered_sinks
