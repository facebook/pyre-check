(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  type elt

  type set [@@deriving compare, eq, hash, sexp, show]

  include Abstract.Domain.S with type t = set

  type t = set [@@deriving compare, eq, hash, sexp, show]

  val empty : t

  val is_empty : t -> bool

  val mem : elt -> t -> bool

  val diff : t -> t -> t

  val singleton : elt -> t

  val all : t

  val is_all : t -> bool

  val of_list : elt list -> t

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val to_json : t -> Yojson.Safe.t option
end

module Source : sig
  type t = Named of string [@@deriving compare, eq, hash, sexp, show]
end

module Sink : sig
  type t = Named of string [@@deriving compare, eq, hash, sexp, show]
end

module SourceSet : S with type elt = Source.t

module SinkSet : S with type elt = Sink.t

type t =
  | Source of Source.t
  | Sink of Sink.t
