(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Source : sig
  type t = Named of string [@@deriving compare, eq, hash, sexp, show]
end

module SourceSet : Data_structures.SerializableSet.S with type elt = Source.t

module Sink : sig
  type t = Named of string [@@deriving compare, eq, hash, sexp, show]
end

module SinkSet : Data_structures.SerializableSet.S with type elt = Sink.t

type t =
  | Source of Source.t
  | Sink of Sink.t
