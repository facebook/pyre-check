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

val save : t -> unit

val type_environment : t -> (unit -> TypeEnvironment.t) -> TypeEnvironment.t

val class_hierarchy_graph : t -> (unit -> ClassHierarchyGraph.Heap.t) -> ClassHierarchyGraph.Heap.t

val initial_callables : t -> (unit -> FetchCallables.t) -> FetchCallables.t
