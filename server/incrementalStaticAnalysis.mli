(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

val run_additional_check
  :  configuration:Configuration.Analysis.t ->
  environment:Analysis.TypeEnvironment.t ->
  source_paths:Ast.SourcePath.t list ->
  check:string ->
  Analysis.AnalysisError.Instantiated.t list
