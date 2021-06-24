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

val type_check
  :  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  use_cache:bool ->
  TypeEnvironment.t

(* Exposed for testing purposes. *)
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

(* Exposed for testing purposes. *)
val regular_and_filtered_callables
  :  configuration:Configuration.Analysis.t ->
  resolution:GlobalResolution.t ->
  source:Source.t ->
  found_callable list * Callable.real_target list

(* The boolean indicated whether the callable is internal or not. *)
type callable_with_dependency_information = Callable.real_target * bool

type initial_callables = {
  callables_with_dependency_information: callable_with_dependency_information list;
  stubs: Callable.real_target list;
  filtered_callables: Callable.Set.t;
}

val fetch_initial_callables
  :  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  environment:TypeEnvironment.ReadOnly.t ->
  qualifiers:Reference.t list ->
  use_cache:bool ->
  initial_callables

val analyze
  :  scheduler:Scheduler.t ->
  analysis:Interprocedural.AnalysisKind.abstract ->
  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  filename_lookup:(Reference.t -> string option) ->
  environment:TypeEnvironment.ReadOnly.t ->
  qualifiers:Reference.t list ->
  initial_callables:initial_callables ->
  initial_models:Interprocedural.Result.model_t Callable.Map.t ->
  skip_overrides:Ast.Reference.Set.t ->
  unit ->
  unit
