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
  fixpoint_state:Fixpoint.t ->
  filename_lookup:(Ast.Reference.t -> string option) ->
  override_graph:OverrideGraph.SharedMemory.t ->
  Target.t ->
  Issue.t list ->
  Model.t ->
  Yojson.Safe.t list

val fetch_and_externalize
  :  taint_configuration:TaintConfiguration.Heap.t ->
  fixpoint_state:Fixpoint.t ->
  filename_lookup:(Ast.Reference.t -> string option) ->
  override_graph:OverrideGraph.SharedMemory.t ->
  Target.t ->
  Yojson.Safe.t list

val report
  :  scheduler:Scheduler.t ->
  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  taint_configuration:TaintConfiguration.SharedMemory.t ->
  filename_lookup:(Ast.Reference.t -> string option) ->
  override_graph:OverrideGraph.SharedMemory.t ->
  callables:Target.Set.t ->
  skipped_overrides:Target.t list ->
  fixpoint_timer:Timer.t ->
  fixpoint_state:Fixpoint.t ->
  Yojson.Safe.t list
