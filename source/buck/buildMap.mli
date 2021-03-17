(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

(** A partial build map is an link-tree-to-source mapping for all `.py` or `.pyi` files within a
    specific Buck target. It is usually build from a buck source-db JSON file.

    The map only holds relative paths. Artifact paths (i.e. keys) stored in the build map are
    relative to the build root. Source paths (i.e. values) stored in the build map are relative to
    the source's buck root. *)
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
