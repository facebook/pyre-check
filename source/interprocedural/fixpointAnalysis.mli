(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
  callables:Target.t list ->
  stubs:Target.t list ->
  AnalysisResult.model_t AnalysisResult.initialize_result

module Testing : sig
  val record_initial_models
    :  targets:Target.t list ->
    stubs:Target.t list ->
    AnalysisResult.model_t Target.Map.t ->
    unit
end

val record_initial_models
  :  callables:Target.t list ->
  stubs:Target.t list ->
  AnalysisResult.model_t Target.Map.t ->
  unit

type expensive_callable = {
  time_to_analyze_in_ms: int;
  callable: Target.t;
}

type result = {
  callables_processed: int;
  expensive_callables: expensive_callable list;
  callables_to_dump: Target.Set.t;
}

val one_analysis_pass
  :  analysis:Kind.abstract ->
  step:FixpointState.step ->
  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  callables:Target.t list ->
  result

(* Returns number of iterations. *)
val compute_fixpoint
  :  scheduler:Scheduler.t ->
  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  analysis:Kind.abstract ->
  dependencies:DependencyGraph.t ->
  filtered_callables:Target.Set.t ->
  all_callables:Target.t list ->
  FixpointState.Epoch.t ->
  int

val strip_for_callsite : AnalysisResult.model_t -> AnalysisResult.model_t

val report_results
  :  scheduler:Scheduler.t ->
  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  filename_lookup:(Ast.Reference.t -> string option) ->
  analysis:Kind.abstract ->
  callables:Target.Set.t ->
  skipped_overrides:Ast.Reference.t list ->
  fixpoint_timer:Timer.t ->
  fixpoint_iterations:int option ->
  Yojson.Safe.json list
