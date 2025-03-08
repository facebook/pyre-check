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

val get_or_create_overlay : t -> overlay_identifier -> ErrorsEnvironment.Overlay.t

val remove_overlay : t -> overlay_identifier -> unit

(** A convenience wrapper around {!get_or_create_overlay} followed by
    {!ErrorsEnvironment.Overlay.update_overlaid_code}. *)
val update_overlay_with_code
  :  t ->
  code_updates:SourceCodeIncrementalApi.Overlay.CodeUpdates.t ->
  overlay_identifier ->
  ErrorsEnvironment.UpdateResult.t

val store : t -> unit

val load : EnvironmentControls.t -> t

val run_update_root
  :  t ->
  scheduler:Scheduler.t ->
  ArtifactPath.Event.t list ->
  ErrorsEnvironment.UpdateResult.t

val run_update_overlay_with_code
  :  t ->
  code_updates:SourceCodeIncrementalApi.Overlay.CodeUpdates.t ->
  overlay_identifier ->
  unit

val update_only_root_for_testing
  :  t ->
  scheduler:Scheduler.t ->
  ArtifactPath.Event.t list ->
  ErrorsEnvironment.UpdateResult.t

module AssumeGlobalModuleListing : sig
  val global_module_paths_api : t -> GlobalModulePathsApi.t

  val root_errors : t -> AnalysisError.t list

  val overlay_errors : t -> overlay_identifier -> AnalysisError.t list
end
