(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

module Statement = AstStatement
module Source = AstSource

open Statement


type t
[@@deriving compare, eq, sexp, show]

val create
  :  qualifier: Access.t
  -> local_mode: Source.mode
  -> ?path: string
  -> stub: bool
  -> Statement.t list
  -> t

val empty_stub: t -> bool
val from_empty_stub: access: Access.t -> module_definition: (Access.t -> t option) -> bool

val path: t -> string option

val wildcard_exports: t -> Access.t list

val aliased_export: t -> Access.t -> Access.t option

val in_wildcard_exports: t -> Access.t -> bool
