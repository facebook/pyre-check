(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* ArtifactPath.t represents the path of source code as seen by the Pyre analyzer.

   We distinguish it from SourcePath.t, which is a path of code in the source tree, because a build
   system like buck might mean that the two paths are not the same. We need to be careful to always
   convert between user-facing paths and paths that the analyzer can understand. *)

(* TODO(T170743593) new warning with ppx_conv_sexp.v0.16.X *)
[@@@warning "-name-out-of-scope"]

type t = ArtifactPath of PyrePath.t [@@deriving sexp, compare, equal, hash, show]

let create raw = ArtifactPath raw

let raw (ArtifactPath raw) = raw

module Event = struct
  module Kind = struct
    type t =
      | CreatedOrChanged
      | Deleted
      | Unknown
    [@@deriving show, sexp, compare]
  end

  type artifact_path = t [@@deriving show, sexp, compare]

  type t = {
    kind: Kind.t;
    path: artifact_path;
  }
  [@@deriving show, sexp, compare]

  let create ~kind path = { kind; path }
end
