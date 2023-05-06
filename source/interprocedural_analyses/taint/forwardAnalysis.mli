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
  taint_configuration:TaintConfiguration.Heap.t ->
  string_combine_partial_sink_tree:Domains.BackwardState.Tree.t ->
  environment:TypeEnvironment.ReadOnly.t ->
  class_interval_graph:Interprocedural.ClassIntervalSetGraph.SharedMemory.t ->
  global_constants:Interprocedural.GlobalConstants.SharedMemory.t ->
  qualifier:Reference.t ->
  callable:Interprocedural.Target.t ->
  define:Define.t Node.t ->
  cfg:Cfg.t ->
  call_graph_of_define:Interprocedural.CallGraph.DefineCallGraph.t ->
  get_callee_model:(Interprocedural.Target.t -> Model.t option) ->
  existing_model:Model.t ->
  unit ->
  Model.Forward.t * Issue.t IssueHandle.SerializableMap.t * Issue.TriggeredSinkLocationMap.t
