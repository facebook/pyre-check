(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = ArtifactPath of PyrePath.t [@@deriving sexp, show, compare, hash]

let create raw = ArtifactPath raw

let raw (ArtifactPath raw) = raw

module Event = struct
  module Kind = struct
    type t =
      | CreatedOrChanged
      | Deleted
      | Unknown
    [@@deriving sexp, compare]
  end

  type artifact_path = t [@@deriving sexp, compare]

  type t = {
    kind: Kind.t;
    path: artifact_path;
  }
  [@@deriving sexp, compare]

  let create ~kind path = { kind; path }
end
