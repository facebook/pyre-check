(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** This module defines the interfaces for a build system.

    From Pyre's perspective, a build system is defined as a component that remaps file paths: it
    allows the type checker to view a source file with certain path (i.e. the "source path") as a
    file with some other path (i.e. the "artifact path"). File paths matter for the type checker
    since it affects how Python modules are qualified.

    According to the definition above, a build system must provide interfaces that allow its client
    to query the associations between source paths and artifact paths. Since these associations may
    change throughout the lifetime of a Pyre server, additional hooks are also provided to allow
    clients to initialize and maintain them, if necessary. *)

(** The abstract type of a build system. *)
type t

(** {1 External Interfaces} *)

(** [update build_system source_paths] notifies [build_system] that certain [source_paths] may have
    been updated on the filesystem. The build system should use the provided info to update its
    internal mapping, and return a list of artifact paths whose content may change by this update.*)
val update : t -> SourcePath.Event.t list -> ArtifactPath.Event.t list Lwt.t

(** Given an artifact path, return the corresponding source path, which is guaranteed to be unique
    if exists. Return [None] if no such source path exists. *)
val lookup_source : t -> ArtifactPath.t -> SourcePath.t option

(** Given an source path, return the corresponding artifact paths. Return the empty list if no such
    artifact path exists. *)
val lookup_artifact : t -> SourcePath.t -> ArtifactPath.t list

(** Store the current build system into saved state. *)
val store : t -> unit

(** {1 Construction & Initialization} *)

(* This function allows the client to fully tweak the behavior of a build system. Expose for testing
   purpose only. *)
val create_for_testing
  :  ?update:(SourcePath.Event.t list -> ArtifactPath.Event.t list Lwt.t) ->
  ?lookup_source:(ArtifactPath.t -> SourcePath.t option) ->
  ?lookup_artifact:(SourcePath.t -> ArtifactPath.t list) ->
  ?store:(unit -> unit) ->
  unit ->
  t

(** This module provides APIs that facilitate build system creation. *)
module Initializer : sig
  (** A type alias to {!type:BuildSystem.t}. This alias is needed to avoid naming conflict with
      {!type:BuildSystem.Initializer.t}. *)
  type build_system = t

  (** The abstract type of a build system initializer. *)
  type t

  (** Construct a {!type:BuildSystem.t}. Additional work can be performed (e.g. copying or indexing
      files) to establish the source-to-artifact mapping, before the build system gets created.

      This API may or may not raise exceptions, depending on the behavior of each individual
      initializer. *)
  val run : t -> build_system Lwt.t

  (** Load a {!type:BuildSystem.t} from saved state.

      This API may or may not raise exceptions, depending on the behavior of each individual
      initializer. *)
  val load : t -> build_system Lwt.t

  (** This API allows the build system to perform additional work (e.g. removing temporary files)
      when the Pyre server is about to shut down.

      This API is defined on {!type: t} instead of {!type: build_system} because we want to ensure
      that the cleanup operation can be performed even if build system initialization process is
      interrupted before server initialization finishes. *)
  val cleanup : t -> unit Lwt.t

  (** [null] initializes a no-op build system. It does nothing on [update], and [cleanup], and it
      always assumes an identity source-to-artifact mapping. This can be used when the project being
      checked does not use a build system. This initializer never raises. *)
  val null : t

  (** [buck] initializes a build system that interops with Buck. See {!module:Buck} for more details
      about its behavior.

      The initialization process may fail with many kinds of exceptions:

      - {!Buck.Raw.BuckError} could happen when the underlying [buck] invocation has unexpected
        return code.
      - {!Buck.Builder.JsonError} could happen when the underlying [buck] invocation has unexpected
        output.
      - {!Buck.Builder.LinkTreeConstructionError} could happen when build artifact creation cannot
        function properly due to unexpected issues on the filesystem. *)
  val buck : builder:Buck.Builder.t -> artifact_root:PyrePath.t -> targets:string list -> unit -> t

  (** [track_unwatched_dependency] initializes a build system that keeps track of file changes in
      unwatched dependencies.

      See D33809915 for a detailed description on what the problem is and how the solution works. *)
  val track_unwatched_dependency : Configuration.UnwatchedDependency.t -> t

  (* This function allows the client to fully tweak the behavior of an initializer. Expose for
     testing purpose only. *)
  val create_for_testing
    :  initialize:(unit -> build_system Lwt.t) ->
    load:(unit -> build_system Lwt.t) ->
    cleanup:(unit -> unit Lwt.t) ->
    unit ->
    t
end

(** {1 Convenient Helpers}*)

(** [get_initializer source_paths] infers the right kind of build system initializer according to
    [source_paths] and returns it. *)
val get_initializer : Configuration.SourcePaths.t -> Initializer.t

(** [with_build_system ~f source_paths] creates a build system from [source_paths] and invoke [f] on
    it. The created build system will be automatically cleaned up after [f] returns.*)
val with_build_system : f:(t -> 'a Lwt.t) -> Configuration.SourcePaths.t -> 'a Lwt.t
