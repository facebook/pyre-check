(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

type overlay_identifier = string

val create : ErrorsEnvironment.t -> t

val root : t -> ErrorsEnvironment.ReadOnly.t

val overlay : t -> overlay_identifier -> ErrorsEnvironment.ReadOnly.t option

val root_errors : t -> AnalysisError.t list

val overlay_errors : t -> overlay_identifier -> AnalysisError.t list

val update_root
  :  t ->
  scheduler:Scheduler.t ->
  ArtifactPath.t list ->
  ErrorsEnvironment.UpdateResult.t

val update_overlay_with_code
  :  t ->
  code_updates:(ArtifactPath.t * ModuleTracker.Overlay.CodeUpdate.t) list ->
  overlay_identifier ->
  ErrorsEnvironment.UpdateResult.t

val store : t -> unit

val load : EnvironmentControls.t -> t

val run_update_root : t -> scheduler:Scheduler.t -> ArtifactPath.t list -> unit

val run_update_overlay_with_code
  :  t ->
  code_updates:(ArtifactPath.t * ModuleTracker.Overlay.CodeUpdate.t) list ->
  overlay_identifier ->
  unit
