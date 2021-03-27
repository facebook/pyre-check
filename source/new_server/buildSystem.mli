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

val initialize : t -> unit Lwt.t
(** This API allows the build system to perform additional work (e.g. copying or indexing files) to
    construct & establish the source-to-artifact mapping. *)

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

(** {2 Construction} *)

val null : t
(** [null] is a no-op build system. It does nothing on [initialize], [update], and [cleanup], and it
    always assumes an identity source-to-artifact mapping. This can be used when the project being
    checked does not use a build system. *)

val buck : ServerConfiguration.Buck.t -> t
(** [buck] is a build system that interops with Buck. See {!module:Buck} for more details about its
    behavior. *)

(* This function allows the client to fully tweak the behavior of a build system. Expose for testing
   purpose only. *)
val create_for_testing
  :  ?initialize:(unit -> unit Lwt.t) ->
  ?update:(PyrePath.t list -> PyrePath.t list Lwt.t) ->
  ?cleanup:(unit -> unit Lwt.t) ->
  ?lookup_source:(PyrePath.t -> PyrePath.t option) ->
  ?lookup_artifact:(PyrePath.t -> PyrePath.t list) ->
  unit ->
  t
