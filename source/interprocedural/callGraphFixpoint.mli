(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val compute
  :  scheduler:Scheduler.t ->
  scheduler_policy:Scheduler.Policy.t ->
  pyre_api:CallGraph.PyrePysaEnvironment.ReadOnly.t ->
  call_graph:CallGraph.SharedMemory.call_graphs ->
  dependency_graph:DependencyGraph.whole_program_dependency_graph ->
  override_graph_shared_memory:OverrideGraph.SharedMemory.t ->
  initial_callables:FetchCallables.t ->
  max_iterations:int ->
  t

val get_model : t -> Target.t -> CallGraph.HigherOrderCallGraph.t option

val cleanup : t -> unit
