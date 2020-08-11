(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Analysis
open Ast
open Statement
open Interprocedural

val record_and_merge_call_graph
  :  environment:TypeEnvironment.ReadOnly.t ->
  call_graph:DependencyGraph.callgraph ->
  source:Source.t ->
  DependencyGraph.callgraph

val record_overrides : ?maximum_overrides_to_analyze:int -> DependencyGraph.overrides -> unit

val regular_and_filtered_callables
  :  resolution:GlobalResolution.t ->
  source:Source.t ->
  (Callable.real_target * Define.t Node.t) list * Callable.real_target list

val analyze
  :  scheduler:Scheduler.t ->
  analysis_kind:Interprocedural.AnalysisKind.abstract ->
  configuration:Configuration.StaticAnalysis.t ->
  filename_lookup:(Reference.t -> string option) ->
  environment:TypeEnvironment.ReadOnly.t ->
  qualifiers:Reference.t list ->
  unit ->
  Interprocedural.Error.t list
