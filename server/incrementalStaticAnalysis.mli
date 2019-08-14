(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

val compute_type_check_resolution
  :  configuration:Configuration.Analysis.t ->
  scheduler:Scheduler.t ->
  environment:Analysis.Environment.t ->
  source_paths:Ast.SourcePath.t list ->
  unit

val run_additional_check
  :  configuration:Configuration.Analysis.t ->
  scheduler:Scheduler.t ->
  environment:Analysis.Environment.t ->
  source_paths:Ast.SourcePath.t list ->
  check:string ->
  Analysis.Error.Instantiated.t list
