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
  :  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  environment:TypeEnvironment.ReadOnly.t ->
  attribute_targets:Target.HashSet.t ->
  call_graph:DependencyGraph.callgraph ->
  source:Source.t ->
  DependencyGraph.callgraph

type found_callable = {
  callable: Target.t;
  define: Define.t Node.t;
  is_internal: bool;
}

(* Exposed for testing purposes. *)
val regular_and_filtered_callables
  :  configuration:Configuration.Analysis.t ->
  resolution:GlobalResolution.t ->
  source:Source.t ->
  found_callable list * Target.t list

(* The boolean indicated whether the callable is internal or not. *)
type callable_with_dependency_information = Target.t * bool

type initial_callables = {
  callables_with_dependency_information: callable_with_dependency_information list;
  stubs: Target.t list;
  filtered_callables: Target.Set.t;
}

val fetch_initial_callables
  :  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  cache:Cache.t ->
  environment:TypeEnvironment.ReadOnly.t ->
  qualifiers:Reference.t list ->
  initial_callables

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

val object_targets_from_models : 'a Target.Map.t -> Target.HashSet.t

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
