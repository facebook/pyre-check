(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val save_current_paths
  :  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  module_tracker:Analysis.ModuleTracker.t ->
  unit

(* Return the list of paths to files that have changed between now and the previous call to
   `save_current_paths`. *)
val compute_locally_changed_paths
  :  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  old_module_tracker:Analysis.ModuleTracker.t ->
  new_module_tracker:Analysis.ModuleTracker.t ->
  ArtifactPath.t list
