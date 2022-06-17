(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = ArtifactPath of PyrePath.t [@@deriving show, eq, compare, hash]

let create raw = ArtifactPath raw

let raw (ArtifactPath raw) = raw
