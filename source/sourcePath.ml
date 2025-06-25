(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* SourcePath.t represents the path of source code in a user's source tree.

   We distinguish it from ArtifactPath.t, which is a path of code the analyzer sees, because a build
   system like buck might mean that the two paths are not the same. We need to be careful to always
   convert between user-facing paths and paths that the analyzer can understand. *)

(* TODO(T170743593) new warning with ppx_conv_sexp.v0.16.X *)
[@@@warning "-name-out-of-scope"]

type t = SourcePath of PyrePath.t [@@deriving sexp, compare, hash, show]

let create raw = SourcePath raw

let raw (SourcePath raw) = raw

module Event = struct
  module Kind = struct
    type t =
      | CreatedOrChanged
      | Deleted
    [@@deriving sexp, compare, show]
  end

  type source_path = t [@@deriving sexp, compare, show]

  type t = {
    kind: Kind.t;
    path: source_path;
  }
  [@@deriving sexp, compare, show]

  let create ~kind path = { kind; path }
end
