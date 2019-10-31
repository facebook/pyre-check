(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

module Kind = AnalysisKind

type result = {
  callables_processed: int;
  callables_to_dump: Callable.Set.t;
}

val one_analysis_pass
  :  analyses:Kind.abstract list ->
  step:Fixpoint.step ->
  environment:Analysis.AnnotatedGlobalEnvironment.ReadOnly.t ->
  callables:Callable.t list ->
  result

(* Returns number of iterations. *)
val compute_fixpoint
  :  configuration:Configuration.Analysis.t ->
  scheduler:Scheduler.t ->
  environment:Analysis.AnnotatedGlobalEnvironment.ReadOnly.t ->
  analyses:Kind.abstract list ->
  dependencies:DependencyGraph.t ->
  filtered_callables:Callable.Set.t ->
  all_callables:Callable.t list ->
  Fixpoint.Epoch.t ->
  int

val externalize
  :  filename_lookup:(Ast.Reference.t -> string option) ->
  AnalysisKind.abstract ->
  Callable.t ->
  Yojson.Safe.json list

val extract_errors
  :  Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  Callable.t list ->
  InterproceduralError.t list

val save_results
  :  configuration:Configuration.StaticAnalysis.t ->
  filename_lookup:(Ast.Reference.t -> string option) ->
  analyses:AnalysisKind.abstract list ->
  Callable.t list ->
  unit

(* Calls init on all specified analyses to get initial models *)
val initialize
  :  Kind.abstract list ->
  configuration:Yojson.Safe.json ->
  environment:Analysis.AnnotatedGlobalEnvironment.ReadOnly.t ->
  functions:Callable.t list ->
  InterproceduralResult.model_t Callable.Map.t

val record_initial_models
  :  functions:Callable.t list ->
  stubs:Callable.t list ->
  InterproceduralResult.model_t Callable.Map.t ->
  unit

val strip_for_callsite : InterproceduralResult.model_t -> InterproceduralResult.model_t
