(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type result = {
  module_tracker: Analysis.ModuleTracker.t;
  ast_environment: Analysis.AstEnvironment.t;
  global_environment: Analysis.AnnotatedGlobalEnvironment.ReadOnly.t;
  errors: Analysis.InferenceError.t list;
}

val infer : configuration:Configuration.Analysis.t -> scheduler:Scheduler.t -> unit -> result

val infer_v2
  :  configuration:Configuration.Analysis.t ->
  scheduler:Scheduler.t ->
  unit ->
  TypeInference.Data.GlobalResult.t
