(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  sources: SanitizeTransform.SourceSet.t;
  sinks: SanitizeTransform.SinkSet.t;
}
[@@deriving compare, eq, hash, sexp, show]

val empty : t

val is_empty : t -> bool

val from_sources : SanitizeTransform.SourceSet.t -> t

val from_sinks : SanitizeTransform.SinkSet.t -> t

val union : t -> t -> t

val diff : t -> t -> t

val subset : t -> t -> bool

val mem : t -> SanitizeTransform.t -> bool
