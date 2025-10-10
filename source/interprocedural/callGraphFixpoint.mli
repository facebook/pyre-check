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
  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  resolve_module_path:(Ast.Reference.t -> RepositoryPath.t option) option ->
  pyre_api:PyrePysaApi.ReadOnly.t ->
  callables_to_definitions_map:Target.CallablesSharedMemory.t ->
  callables_to_decorators_map:CallGraph.CallableToDecoratorsMap.SharedMemory.t ->
  type_of_expression_shared_memory:TypeOfExpressionSharedMemory.t ->
  call_graph:CallGraph.SharedMemory.call_graphs ->
  dependency_graph:DependencyGraph.whole_program_dependency_graph ->
  override_graph_shared_memory:OverrideGraph.SharedMemory.t ->
  skip_analysis_targets:Target.HashSet.t ->
  called_when_parameter:Target.HashSet.t ->
  t

val analyzed_callables : fixpoint -> Target.t list

val get_model : t -> Target.t -> CallGraph.HigherOrderCallGraph.t option

val cleanup : keep_models:bool -> fixpoint -> unit
