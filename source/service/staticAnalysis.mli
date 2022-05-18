(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Analysis
open Ast
open Interprocedural

module Cache : sig
  type t

  val load : scheduler:Scheduler.t -> configuration:Configuration.Analysis.t -> enabled:bool -> t

  val initial_callables
    :  t ->
    (unit -> FetchCallables.initial_callables) ->
    FetchCallables.initial_callables
end

val type_check
  :  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  cache:Cache.t ->
  TypeEnvironment.t

val parse_and_save_decorators_to_skip : inline_decorators:bool -> Configuration.Analysis.t -> unit

(* Exposed for testing purposes. *)
val record_and_merge_call_graph
  :  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  environment:TypeEnvironment.ReadOnly.t ->
  attribute_targets:Target.HashSet.t ->
  call_graph:DependencyGraph.callgraph ->
  source:Source.t ->
  DependencyGraph.callgraph

val build_class_hierarchy_graph
  :  scheduler:Scheduler.t ->
  cache:Cache.t ->
  environment:TypeEnvironment.ReadOnly.t ->
  qualifiers:Reference.t list ->
  ClassHierarchyGraph.t

val build_class_intervals : ClassHierarchyGraph.t -> unit

val record_overrides_for_qualifiers
  :  scheduler:Scheduler.t ->
  cache:Cache.t ->
  environment:TypeEnvironment.ReadOnly.t ->
  skip_overrides:Reference.Set.t ->
  qualifiers:Reference.t list ->
  DependencyGraphSharedMemory.cap_overrides_result

val build_call_graph
  :  scheduler:Scheduler.t ->
  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  environment:TypeEnvironment.ReadOnly.t ->
  attribute_targets:Target.HashSet.t ->
  qualifiers:Reference.t list ->
  Target.t list Target.Map.t

val build_dependency_graph
  :  callables_with_dependency_information:(Target.t * bool) list ->
  callgraph:DependencyGraph.callgraph ->
  override_dependencies:Target.t list Target.Map.t ->
  DependencyGraph.t * Target.t list * Target.t list

val purge_shared_memory : environment:TypeEnvironment.t -> qualifiers:Reference.t list -> unit
