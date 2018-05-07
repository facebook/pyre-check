(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

type path = string
[@@deriving eq, show, sexp, hash]

type absolute
[@@deriving eq, show, sexp, hash]

type relative
[@@deriving eq, show, sexp, hash]

type t =
  | Absolute of absolute
  | Relative of relative
[@@deriving eq, show, sexp, hash]

val absolute: t -> path
val relative: t -> path option
val uri: t -> path
val last: t -> path

val create_absolute: path -> t
val create_relative: root: t -> relative: path -> t
val get_relative_to_root: root: t -> path: t -> path option
val from_uri: path -> t option

val current_working_directory: unit -> t

val append: t -> element: path -> t

module AppendOperator : sig
  val (^|): t -> path -> t
end

val follow_symlinks: t -> t

val is_directory: t -> bool
val file_exists: t -> bool
val directory_contains: ?follow_symlinks:bool -> directory:t -> t -> bool

val remove: t -> unit

module Map: Map.S with type Key.t = t
