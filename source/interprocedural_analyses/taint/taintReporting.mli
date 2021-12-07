(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Interprocedural
module Json = Yojson.Safe

val externalize
  :  filename_lookup:(Ast.Reference.t -> string option) ->
  Target.t ->
  Flow.issue list option ->
  Model.t ->
  Yojson.Safe.json list

val fetch_and_externalize
  :  filename_lookup:(Ast.Reference.t -> string option) ->
  Target.t ->
  Yojson.Safe.json list

val report
  :  scheduler:Scheduler.t ->
  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  environment:'e ->
  filename_lookup:(Ast.Reference.t -> string option) ->
  callables:Target.Set.t ->
  skipped_overrides:Ast.Reference.t list ->
  fixpoint_timer:Timer.t ->
  fixpoint_iterations:int option ->
  Yojson.Safe.json list
