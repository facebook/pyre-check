(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type fixpoint

type t = {
  fixpoint: fixpoint;
  whole_program_call_graph: CallGraph.WholeProgramCallGraph.t;
  get_define_call_graph: Target.t -> CallGraph.DefineCallGraph.t option;
}

val compute
  :  scheduler:Scheduler.t ->
  scheduler_policy:Scheduler.Policy.t ->
  pyre_api:CallGraph.PyrePysaEnvironment.ReadOnly.t ->
  call_graph:CallGraph.SharedMemory.call_graphs ->
  dependency_graph:DependencyGraph.whole_program_dependency_graph ->
  override_graph_shared_memory:OverrideGraph.SharedMemory.t ->
  skip_analysis_targets:Target.Set.t ->
  decorator_resolution:CallGraph.DecoratorResolution.Results.t ->
  method_kinds:CallGraph.MethodKind.SharedMemory.ReadOnly.t ->
  max_iterations:int ->
  t

val analyzed_callables : fixpoint -> Target.t list

val get_model : t -> Target.t -> CallGraph.HigherOrderCallGraph.t option

val cleanup : t -> unit
