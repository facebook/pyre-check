(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type environment_data = {
  global_environment: Analysis.AnnotatedGlobalEnvironment.ReadOnly.t;
  qualifiers: Ast.Reference.t list;
}

val build_environment_data
  :  configuration:Configuration.Analysis.t ->
  scheduler:Scheduler.t ->
  unit ->
  environment_data

val run_infer
  :  configuration:Configuration.Analysis.t ->
  scheduler:Scheduler.t ->
  filename_lookup:(Ast.Reference.t -> string option) ->
  paths_to_modify:string list option ->
  environment_data ->
  TypeInference.Data.GlobalResult.t
