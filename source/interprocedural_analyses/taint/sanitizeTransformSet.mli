(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type sets = {
  sources: SanitizeTransform.SourceSet.t;
  sinks: SanitizeTransform.SinkSet.t;
}
[@@deriving compare, equal, hash, sexp, show]

include Abstract.Domain.S with type t = sets

type t = sets [@@deriving compare, equal, hash, sexp, show]

val empty : t

val is_empty : t -> bool

val add_source : SanitizeTransform.Source.t -> t -> t

val add_sink : SanitizeTransform.Sink.t -> t -> t

val from_sources : SanitizeTransform.SourceSet.t -> t

val from_sinks : SanitizeTransform.SinkSet.t -> t

val diff : t -> t -> t

val mem : t -> SanitizeTransform.t -> bool

val all : t

val is_all : t -> bool
