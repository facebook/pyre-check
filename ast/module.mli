(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


type t
[@@deriving eq, sexp, show]

val create
  :  qualifier: Reference.t
  -> local_mode: Source.mode
  -> ?handle: File.Handle.t
  -> stub: bool
  -> Statement.t list
  -> t

val empty_stub: t -> bool
val from_empty_stub: reference: Reference.t -> module_definition: (Reference.t -> t option) -> bool

val handle: t -> File.Handle.t option

val wildcard_exports: t -> Reference.t list

val aliased_export: t -> Reference.t -> Reference.t option

val in_wildcard_exports: t -> Reference.t -> bool

module Cache : sig
  val clear: unit -> unit
end
