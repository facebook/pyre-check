(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** All build-related APIs in this file assumes that file names and directory names do not overlap
    in any of the given build map (see documentation for {!module:Buck.BuildMap}). If this
    assumption is broken, build result will be non-deterministic due to race conditions between file
    creation/removal and directory creation/removal. *)

module Path = Pyre.Path

val populate
  :  source_root:Path.t ->
  artifact_root:Path.t ->
  BuildMap.t ->
  (unit, string) Result.t Lwt.t
(** Populate the artifact directory given a source root, an artifact root, and a build map. Return
    [Result.Ok ()] if the build succeeds, and [Result.Error message] otherwise, where [message]
    contains the error message.

    Specifically, what [populate] does is, for each source path in the build map, to calculate the
    corresponding path of the artifact, and create a symlink to the original source file at the said
    path. All source paths in the build map will be rooted at the [source_root] argument, and all
    artifact paths in the build map will be rooted at the [artifact_root] argument. There is no
    guarantee on the order in which the artifact symlink gets created -- this API only guarantees
    that when the returned promise is resolved and no error occurs, all artifacts will be created.

    If either [source_root] or [artifact_root] is not a directory, an error will be returned. During
    the build process, new directories and symlinks may be created under [artifact_root] -- if any
    of the creation fails (e.g. creating a symlink at a location where there already exists a file,
    or when creating a directory at a location where there already exists a non-directory), the
    entire process fails. Directories created by this API will have the default permission of 0777
    (unless adjusted by [umask]).

    Although in its typical usage [artifact_root] would be an empty directory prior to the full
    build, this API makes no attempt to check for that. It can be fully functional as long as
    pre-existing files under [artifact_root] do not have naming conflicts with any of the artifacts.
    If cleaness of the artifact directory is required, it is expected that the caller would take
    care of that before invoking [full_build]. *)

val update
  :  source_root:Path.t ->
  artifact_root:Path.t ->
  BuildMap.Difference.t ->
  (unit, string) Result.t Lwt.t
(** Incrementally update the artifact directory given a source root, an artifact root, and a build
    map difference specificiation. Return [Result.Ok ()] if the build succeeds, and [Result.Error
    message] otherwise, where [message] contains the error message.

    Specifically, what an incremental update does is, for each artifact path in the difference spec,
    to figure out how to update the filesystem accordingly. If a new mapping is added, create a new
    artifact symlink at the corresponding location. If an old mapping is deleted, remove the old
    artifact symlink at the corresponding location. If the artifact is redirected to a different
    source file, update the symlink accordingly.

    All source paths in the build map will be rooted at the [source_root] argument, and all artifact
    paths in the build map will be rooted at the [artifact_root] argument. There is no guarantee on
    the order in which the artifact symlink gets updated -- this API only guarantees that when the
    returned promise is resolved and no error occurs, incremental build would be finished.

    If either [source_root] or [artifact_root] is not a directory, an error will be returned. During
    the build process, new directories and symlinks may be created under [artifact_root], and old
    symlinks may be deleted -- if any of the creation or removal fails (e.g. do not have the right
    permission to remove the files), the entire process fails. Directories created by this API will
    have the default permission of 0777 (unless adjusted by [umask]). *)
