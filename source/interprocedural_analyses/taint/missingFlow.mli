(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Interprocedural

(* Returns true if the given target is a symbolic target that represents an unknown callee. *)
val is_unknown_callee : Target.t -> bool

(* Model for an unknown callee, with sinks on all parameters, in order to find missing flows. *)
val unknown_callee_model : Target.t -> AnalysisResult.model_t

(* Return the initial set of models, updated for the missing-flows=obscure analysis. *)
val add_obscure_models
  :  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  environment:Analysis.TypeEnvironment.ReadOnly.t ->
  stubs:Target.HashSet.t ->
  initial_models:Model.t Target.Map.t ->
  Model.t Target.Map.t

(* Register models in shared memory for the missing-flows=type analysis. *)
val add_unknown_callee_models
  :  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  callgraph:Target.t list Target.Map.t ->
  unit
