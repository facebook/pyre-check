(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Interprocedural
module Json = Yojson.Safe

val externalize
  :  taint_configuration:TaintConfiguration.Heap.t ->
  fixpoint_state:TaintFixpoint.t ->
  filename_lookup:(Ast.Reference.t -> string option) ->
  override_graph:OverrideGraph.SharedMemory.t ->
  Target.t ->
  Issue.t list ->
  Model.t ->
  Yojson.Safe.t list

val fetch_and_externalize
  :  taint_configuration:TaintConfiguration.Heap.t ->
  fixpoint_state:TaintFixpoint.t ->
  filename_lookup:(Ast.Reference.t -> string option) ->
  override_graph:OverrideGraph.SharedMemory.t ->
  Target.t ->
  Yojson.Safe.t list

val produce_errors
  :  scheduler:Scheduler.t ->
  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  filename_lookup:(Ast.Reference.t -> string option) ->
  taint_configuration:TaintConfiguration.SharedMemory.t ->
  callables:Target.Set.t ->
  fixpoint_timer:Timer.t ->
  fixpoint_state:TaintFixpoint.t ->
  Yojson.Safe.t list

val save_results_to_directory
  :  scheduler:Scheduler.t ->
  taint_configuration:TaintConfiguration.SharedMemory.t ->
  result_directory:PyrePath.t ->
  output_format:Configuration.TaintOutputFormat.t ->
  local_root:PyrePath.t ->
  filename_lookup:(Ast.Reference.t -> string option) ->
  override_graph:OverrideGraph.SharedMemory.t ->
  skipped_overrides:Target.t list ->
  callables:Target.Set.t ->
  model_verification_errors:ModelVerificationError.t list ->
  fixpoint_state:TaintFixpoint.t ->
  errors:Yojson.Safe.t list ->
  unit
