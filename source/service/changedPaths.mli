(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val compute_locally_changed_paths
  :  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  module_tracker:Analysis.ModuleTracker.t ->
  ast_environment:Analysis.AstEnvironment.ReadOnly.t ->
  PyrePath.t list
