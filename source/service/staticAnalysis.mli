(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Analysis
open Ast
open Statement
open Interprocedural

module Cache : sig
  type t

  val load : scheduler:Scheduler.t -> configuration:Configuration.Analysis.t -> enabled:bool -> t
end

val type_check
  :  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  cache:Cache.t ->
  TypeEnvironment.t

val parse_and_save_decorators_to_skip : inline_decorators:bool -> Configuration.Analysis.t -> unit

(* Exposed for testing purposes. *)
val record_and_merge_call_graph
  :  environment:TypeEnvironment.ReadOnly.t ->
  call_graph:DependencyGraph.callgraph ->
  source:Source.t ->
  DependencyGraph.callgraph

type found_callable = {
  callable: Target.callable_t;
  define: Define.t Node.t;
  is_internal: bool;
}

(* Exposed for testing purposes. *)
val regular_and_filtered_callables
  :  configuration:Configuration.Analysis.t ->
  resolution:GlobalResolution.t ->
  source:Source.t ->
  found_callable list * Target.callable_t list

(* The boolean indicated whether the callable is internal or not. *)
type callable_with_dependency_information = Target.callable_t * bool

type initial_callables = {
  callables_with_dependency_information: callable_with_dependency_information list;
  stubs: Target.callable_t list;
  filtered_callables: Target.Set.t;
}

val fetch_initial_callables
  :  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  cache:Cache.t ->
  environment:TypeEnvironment.ReadOnly.t ->
  qualifiers:Reference.t list ->
  initial_callables

val analyze
  :  scheduler:Scheduler.t ->
  analysis:AnalysisKind.abstract ->
  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  cache:Cache.t ->
  filename_lookup:(Reference.t -> string option) ->
  environment:TypeEnvironment.ReadOnly.t ->
  qualifiers:Reference.t list ->
  initial_callables:initial_callables ->
  initial_models:AnalysisResult.model_t Target.Map.t ->
  skip_overrides:Ast.Reference.Set.t ->
  unit ->
  unit
