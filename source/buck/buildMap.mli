(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

(** This module implements the build map data structure, which stores the association between
    artifact Python files and source Python files. Storing the association is necessary because if
    Buck builds a project, it will create copies/symlinks from the source files to artifact files,
    where artifact files do not necessarily end up being placed in the same relative path as they
    are placed in the source directories.

    The build map represents a one-to-many association: there can be only one source file mapped to
    a given artifact file, but there can be multiple artifact files mapped to the same source file.
    Mappings are interpreted in an order-insensitive way: it does not matter which items are
    inserted before or after which. As long as two mappings hold the same combination of items, they
    will be considered equivalent.

    The map only holds relative paths. Artifact paths (i.e. keys) stored in the build map are
    relative to the build root. Source paths (i.e. values) stored in the build map are relative to
    the source's buck root. *)

(** A partial build map is an link-tree-to-source mapping for all `.py` or `.pyi` files within a
    specific Buck target. It is usually build from a buck source-db JSON file. *)
module Partial : sig
  type t [@@deriving sexp]

  (** Result type for the [merge] operation. *)
  module MergeResult : sig
    module IncompatibleItem : sig
      type t = {
        key: string;
        left_value: string;
        right_value: string;
      }
      [@@deriving sexp, compare]
    end

    type nonrec t =
      | Ok of t
      | Incompatible of IncompatibleItem.t
  end

  val of_json_exn : Yojson.Safe.t -> t
  (** Create a partial build map from a JSON. The JSON must conform to Buck's Python source-db
      format. Raise an exception if the creation fails. *)

  val of_json : Yojson.Safe.t -> (t, string) Result.t
  (** Same as [of_json_exn] except failures are wrapped in a [Result.t]. *)

  val of_json_file_exn : Pyre.Path.t -> t
  (** Read JSON from the file at the given path, and invoke [of_json_exn] on it. Raise an exception
      if the file reading fails. *)

  val of_json_file : Pyre.Path.t -> (t, string) Result.t
  (** Same as [of_json_file] except failures are wrapped in a [Result.t]. *)

  val merge : t -> t -> MergeResult.t
  (** Given two partial build maps [l] and [r], [merge l r] returns [MergeResult.Ok m] where [m] is
      a new partial build map containing items in both maps, if the merging process succeeds.
      Merging would fail only if there exists at least one key which is mapped to different values
      in [l] and [r]. In the failing case, [MergeResult.Incompatible item] would be returned, where
      [item] would keep track of which values are different as well as what the corresponding key
      is. *)

  (* Create a partial build map from an associated list. Note that this API will attempt to drop all
     mappings that are not keyed on `.py` or `.pyi` files. Exposed for testing. *)
  val of_alist_exn : (string * string) list -> t

  (* Convert a partial build map into an associated list. Exposed for testing. *)
  val to_alist : t -> (string * string) list
end

(** Result type for the [index] operation. *)
module Indexed : sig
  type t

  val lookup_source : t -> string -> string option
  (** Lookup the source path that corresponds to the given artifact path. If there is no such
      artifact, return [None]. Time complexity of this operation is O(1).*)

  val lookup_artifact : t -> string -> string list
  (** Lookup all artifact paths that corresponds to the given source path. If there is no such
      artifact, return an empty list. Time complexity of this operation is O(1).*)
end

(** Result type for the [difference] operation. It represents a set of artifact paths where each
    path has an associated tag indicating whether the file is added, removed, or updated. *)
module Difference : sig
  module Kind : sig
    type t =
      | New of string
      | Deleted
      | Changed of string
    [@@deriving sexp, compare]
  end

  type t [@@deriving sexp]

  (* Convert a build map difference into an associated list. Exposed for testing. *)
  val to_alist : t -> (string * Kind.t) list

  val iter : f:(kind:Kind.t -> string -> unit) -> t -> unit
  (** [iter ~f difference] applies `f` to every path in [difference]. *)
end

type t
(** Type of the build map. *)

val create : Partial.t -> t
(** Create a build map from a partial build map. This is intended to be the only API for build map
    creation. *)

val index : t -> Indexed.t
(** Create a index for the given build map and return a pair of constant-time lookup functions that
    utilizes the index. *)

val to_alist : t -> (string * string) list
(** Convert a partial build map into an associated list. Each element in the list represent an
    (artifact_path, source_path) mapping. *)

val difference : original:t -> t -> Difference.t
(** [difference ~original current] computes the difference between the [original] build map and the
    [current] build map. Time complexity of this operation is O(n + m), where n and m are the sizes
    of the two build maps. *)
