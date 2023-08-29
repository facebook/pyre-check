(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Analysis
open Interprocedural

type t

val try_load
  :  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  decorator_configuration:Analysis.DecoratorPreprocessing.Configuration.t ->
  enabled:bool ->
  t

val save
  :  maximum_overrides:int option ->
  initial_models:Registry.t ->
  skipped_overrides:Interprocedural.OverrideGraph.skipped_overrides ->
  override_graph_shared_memory:Interprocedural.OverrideGraph.SharedMemory.t ->
  initial_callables:FetchCallables.t ->
  t ->
  unit

val type_environment : t -> (unit -> TypeEnvironment.t) -> TypeEnvironment.t * t

val class_hierarchy_graph
  :  t ->
  (unit -> ClassHierarchyGraph.Heap.t) ->
  ClassHierarchyGraph.Heap.t * t

val initial_callables : t -> (unit -> FetchCallables.t) -> FetchCallables.t * t

val class_interval_graph
  :  t ->
  (unit -> ClassIntervalSetGraph.Heap.t) ->
  ClassIntervalSetGraph.Heap.t * t

val metadata_to_json : t -> Yojson.Safe.t

val override_graph
  :  initial_models:Registry.t ->
  maximum_overrides:int option ->
  t ->
  (initial_models:Registry.t ->
  maximum_overrides:int option ->
  unit ->
  Interprocedural.OverrideGraph.whole_program_overrides) ->
  Interprocedural.OverrideGraph.whole_program_overrides * t
