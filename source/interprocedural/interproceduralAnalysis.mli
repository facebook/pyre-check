(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Kind = AnalysisKind

val initialize_configuration
  :  Kind.abstract ->
  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  unit

val initialize_models
  :  Kind.abstract ->
  scheduler:Scheduler.t ->
  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  functions:Callable.t list ->
  stubs:Callable.t list ->
  InterproceduralResult.model_t InterproceduralResult.InitializedModels.t

val record_initial_models
  :  functions:Callable.t list ->
  stubs:Callable.t list ->
  InterproceduralResult.model_t Callable.Map.t ->
  unit

type expensive_callable = {
  time_to_analyze_in_ms: int;
  callable: Callable.t;
}

type result = {
  callables_processed: int;
  expensive_callables: expensive_callable list;
  callables_to_dump: Callable.Set.t;
}

val one_analysis_pass
  :  analysis:Kind.abstract ->
  step:Fixpoint.step ->
  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  callables:Callable.t list ->
  result

(* Returns number of iterations. *)
val compute_fixpoint
  :  scheduler:Scheduler.t ->
  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  analysis:Kind.abstract ->
  dependencies:DependencyGraph.t ->
  filtered_callables:Callable.Set.t ->
  all_callables:Callable.t list ->
  Fixpoint.Epoch.t ->
  int

val strip_for_callsite : InterproceduralResult.model_t -> InterproceduralResult.model_t

val report_results
  :  scheduler:Scheduler.t ->
  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  filename_lookup:(Ast.Reference.t -> string option) ->
  analysis:Kind.abstract ->
  callables:Callable.Set.t ->
  skipped_overrides:Ast.Reference.t list ->
  fixpoint_timer:Timer.t ->
  fixpoint_iterations:int option ->
  Yojson.Safe.json list
