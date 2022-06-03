(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Interprocedural
module Json = Yojson.Safe

val externalize
  :  fixpoint_state:Fixpoint.t ->
  filename_lookup:(Ast.Reference.t -> string option) ->
  override_graph:Interprocedural.OverrideGraph.SharedMemory.t ->
  Target.t ->
  Issue.t list ->
  Model.t ->
  Yojson.Safe.json list

val fetch_and_externalize
  :  fixpoint_state:Fixpoint.t ->
  filename_lookup:(Ast.Reference.t -> string option) ->
  override_graph:Interprocedural.OverrideGraph.SharedMemory.t ->
  Target.t ->
  Yojson.Safe.json list

val report
  :  scheduler:Scheduler.t ->
  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  filename_lookup:(Ast.Reference.t -> string option) ->
  override_graph:Interprocedural.OverrideGraph.SharedMemory.t ->
  callables:Target.Set.t ->
  skipped_overrides:Target.t list ->
  fixpoint_timer:Timer.t ->
  fixpoint_state:Fixpoint.t ->
  Yojson.Safe.json list
