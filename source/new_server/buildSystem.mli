(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

type t
(** The abstract type of a build system. *)

(** {1 External Interfaces} *)

val update : t -> PyrePath.t list -> PyrePath.t list Lwt.t
(** [update build_system source_paths] notifies [build_system] that certain [source_paths] may have
    been updated on the filesystem. The build system should use the provided info to update its
    internal mapping, and return a list of artifact paths whose content may change by this update.*)

val cleanup : t -> unit Lwt.t
(** This API allows the build system to perform additional work (e.g. removing temporary files) when
    the Pyre server is about to shut down. *)

val lookup_source : t -> PyrePath.t -> PyrePath.t option
(** Given an artifact path, return the corresponding source path, which is guaranteed to be unique
    if exists. Return [None] if no such source path exists. *)

val lookup_artifact : t -> PyrePath.t -> PyrePath.t list
(** Given an source path, return the corresponding artifact paths. Return the empty list if no such
    artifact path exists. *)

val store : t -> unit
(** Store the current build system into saved state. *)

(** {1 Construction & Initialization} *)

(* This function allows the client to fully tweak the behavior of a build system. Expose for testing
   purpose only. *)
val create_for_testing
  :  ?update:(PyrePath.t list -> PyrePath.t list Lwt.t) ->
  ?cleanup:(unit -> unit Lwt.t) ->
  ?lookup_source:(PyrePath.t -> PyrePath.t option) ->
  ?lookup_artifact:(PyrePath.t -> PyrePath.t list) ->
  ?store:(unit -> unit) ->
  unit ->
  t

(** This module provides APIs that facilitate build system creation. *)
module Initializer : sig
  type build_system = t
  (** A type alias to {!type:BuildSystem.t}. This alias is needed to avoid naming conflict with
      {!type:BuildSystem.Initializer.t}. *)

  type t
  (** The abstract type of a build system initializer. *)

  val run : t -> build_system Lwt.t
  (** Construct a {!type:BuildSystem.t}. Additional work can be performed (e.g. copying or indexing
      files) to establish the source-to-artifact mapping, before the build system gets created.

      This API may or may not raise exceptions, depending on the behavior of each individual
      initializer. *)

  val load : t -> build_system Lwt.t
  (** Load a {!type:BuildSystem.t} from saved state.

      This API may or may not raise exceptions, depending on the behavior of each individual
      initializer. *)

  val null : t
  (** [null] initializes a no-op build system. It does nothing on [update], and [cleanup], and it
      always assumes an identity source-to-artifact mapping. This can be used when the project being
      checked does not use a build system. This initializer never raises. *)

  val buck : raw:Buck.Raw.t -> Configuration.Buck.t -> t
  (** [buck] initializes a build system that interops with Buck. See {!module:Buck} for more details
      about its behavior.

      The initialization process may fail with many kinds of exceptions:

      - {!Buck.Raw.BuckError} could happen when the underlying [buck] invocation has unexpected
        return code.
      - {!Buck.Builder.JsonError} could happen when the underlying [buck] invocation has unexpected
        output.
      - {!Buck.Builder.LinkTreeConstructionError} could happen when build artifact creation cannot
        function properly due to unexpected issues on the filesystem. *)

  (* This function allows the client to fully tweak the behavior of an initializer. Expose for
     testing purpose only. *)
  val create_for_testing
    :  initialize:(unit -> build_system Lwt.t) ->
    load:(unit -> build_system Lwt.t) ->
    unit ->
    t
end

(** {1 Convenient Helpers}*)

val with_build_system : f:(t -> 'a Lwt.t) -> Configuration.SourcePaths.t -> 'a Lwt.t
(** [with_build_system ~f source_paths] creates a build system from [source_paths] and invoke [f] on
    it. The created build system will be automatically cleaned up after [f] returns.*)
