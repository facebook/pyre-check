(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** This module contains high-level interfaces for invoking `buck` as an external tool. *)

type t

exception JsonError of string
(** Raised when `buck` returns malformed JSONs *)

exception LinkTreeConstructionError of string
(** Raised when artifact building fails. See {!val:Buck.Artifacts.populate}. *)

module BuildResult : sig
  type t = {
    build_map: BuildMap.t;
    targets: Target.t list;
  }
  (** The return type for all build-related APIs. It contains a build map as well as a list of buck
      targets that are successfully included in the build. *)
end

val create : ?mode:string -> ?isolation_prefix:string -> Raw.t -> t

val build
  :  source_root:PyrePath.t ->
  artifact_root:PyrePath.t ->
  targets:string list ->
  t ->
  BuildResult.t Lwt.t
(** Given a source root, an artifact root, and a list of buck target specificaitons to build,
    construct a build map for the targets and create a Python link tree at the given artifact root
    according to the build map. Return the constructed build map along with a list of targets that
    are covered by the build map.

    Concretely, the entire build process can be broken down into 4 steps:

    - Query `buck` to desugar any `...` wildcard and filter expressions.
    - Run `buck build` to force-generating all Python files and source databases.
    - Load all source databases generated from the previous step, and merge all of them into a
      single [BuildMap.t].
    - Construct the link tree under [artifact_root] based on the content of the [BuiltMap.t].

    The following exceptions may be raised by this API:

    - {!exception: JsonError} if `buck` returns malformed or inconsistent JSON blobs.
    - {!exception: Buck.Raw.BuckError} if `buck` quits in any unexpected ways when shelling out to
      it.
    - {!exception: LinkTreeConstructionError} if any error is encountered when constructing the link
      tree from the build map.

    Note this API does not ensure the artifact root to be empty before the build starts. If cleaness
    of the artifact directory is desirable, it is expected that the caller would take care of that
    before its invocation. *)

(* Raise [JsonError] on parsing error. Exposed for testing. *)
val parse_buck_query_output : string -> string list

(* Raise [JsonError] on parsing error. Exposed for testing. *)
val parse_buck_build_output : string -> (string * string) list

(* Merge given partial build maps into one full build map. Exposed for testing. *)
val merge_build_maps : (Target.t * BuildMap.Partial.t) list -> Target.t list * BuildMap.t
