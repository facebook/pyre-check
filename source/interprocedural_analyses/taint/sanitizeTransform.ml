(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module Source = struct
  type t = Named of string [@@deriving compare, eq, hash, sexp]

  let pp formatter (Named source) = Format.fprintf formatter "NotSource[%s]" source

  let show = Format.asprintf "%a" pp
end

module SourceSet = Data_structures.SerializableSet.Make (Source)

module Sink = struct
  type t = Named of string [@@deriving compare, eq, hash, sexp]

  let pp formatter (Named sink) = Format.fprintf formatter "NotSink[%s]" sink

  let show = Format.asprintf "%a" pp
end

module SinkSet = Data_structures.SerializableSet.Make (Sink)

type t =
  | Source of Source.t
  | Sink of Sink.t
