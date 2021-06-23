(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Analysis
open Ast
open Statement
open Interprocedural

module Cache : sig
  val load_environment : configuration:Configuration.Analysis.t -> TypeEnvironment.t option

  val save_environment
    :  configuration:Configuration.Analysis.t ->
    environment:TypeEnvironment.t ->
    unit
end

val type_check
  :  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  use_cache:bool ->
  TypeEnvironment.t

val record_and_merge_call_graph
  :  environment:TypeEnvironment.ReadOnly.t ->
  call_graph:DependencyGraph.callgraph ->
  source:Source.t ->
  DependencyGraph.callgraph

type found_callable = {
  callable: Callable.real_target;
  define: Define.t Node.t;
  is_internal: bool;
}

val regular_and_filtered_callables
  :  configuration:Configuration.Analysis.t ->
  resolution:GlobalResolution.t ->
  source:Source.t ->
  found_callable list * Callable.real_target list

val analyze
  :  scheduler:Scheduler.t ->
  analysis:Interprocedural.AnalysisKind.abstract ->
  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  filename_lookup:(Reference.t -> string option) ->
  environment:TypeEnvironment.ReadOnly.t ->
  qualifiers:Reference.t list ->
  ?initialized_models:Interprocedural.Result.model_t Interprocedural.Result.InitializedModels.t ->
  unit ->
  unit
