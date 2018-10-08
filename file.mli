(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre


type t
[@@deriving eq, show, hash]

val create: ?content: string -> Path.t -> t

val path: t -> Path.t

val content: t -> string option
val lines: t -> (string list) option

val write: t -> unit

exception NonexistentHandle of string

module Handle : sig
  type t
  [@@deriving compare, eq, show, sexp, hash]

  val create: string -> t

  val is_stub: t -> bool

  val to_path: configuration: Configuration.Analysis.t -> t -> Path.t option

  include Hashable with type t := t
  module Map: Map.S with type Key.t = t
  module Set: Set.S with type Elt.t = t
end

module Set: Set.S with type Elt.t = t

val handle: configuration: Configuration.Analysis.t -> t -> Handle.t
