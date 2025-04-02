(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Interprocedural

val externalize
  :  taint_configuration:TaintConfiguration.Heap.t ->
  fixpoint_state:TaintFixpoint.State.ReadOnly.t ->
  resolve_module_path:(Ast.Reference.t -> RepositoryPath.t option) ->
  resolve_callable_location:(Target.t -> Ast.Location.WithModule.t option) ->
  override_graph:OverrideGraph.SharedMemory.ReadOnly.t ->
  Target.t ->
  Issue.t list ->
  Model.t ->
  NewlineDelimitedJson.Line.t list

val fetch_and_externalize
  :  taint_configuration:TaintConfiguration.Heap.t ->
  fixpoint_state:TaintFixpoint.State.ReadOnly.t ->
  resolve_module_path:(Ast.Reference.t -> RepositoryPath.t option) ->
  resolve_callable_location:(Target.t -> Ast.Location.WithModule.t option) ->
  override_graph:OverrideGraph.SharedMemory.ReadOnly.t ->
  sorted:bool ->
  dump_override_models:bool ->
  Target.t ->
  NewlineDelimitedJson.Line.t list

val produce_errors
  :  scheduler:Scheduler.t ->
  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  resolve_module_path:(Ast.Reference.t -> RepositoryPath.t option) ->
  taint_configuration:TaintConfiguration.SharedMemory.t ->
  callables:Target.Set.t ->
  fixpoint_step_logger:StepLogger.t ->
  fixpoint:TaintFixpoint.t ->
  Yojson.Safe.t list

val save_results_to_directory
  :  scheduler:Scheduler.t ->
  taint_configuration:TaintConfiguration.SharedMemory.t ->
  result_directory:PyrePath.t ->
  output_format:Configuration.TaintOutputFormat.t ->
  local_root:PyrePath.t ->
  resolve_module_path:(Ast.Reference.t -> RepositoryPath.t option) ->
  resolve_callable_location:(Target.t -> Ast.Location.WithModule.t option) ->
  override_graph:OverrideGraph.SharedMemory.ReadOnly.t ->
  skipped_overrides:Target.t list ->
  callables:Target.Set.t ->
  model_verification_errors:ModelVerificationError.t list ->
  fixpoint_state:TaintFixpoint.State.ReadOnly.t ->
  errors:Yojson.Safe.t list ->
  cache:Cache.t ->
  file_coverage:FileCoverage.t ->
  rule_coverage:RuleCoverage.t ->
  unit
