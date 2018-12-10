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
[@@deriving compare, eq, show, sexp, hash]

type path_t = t

val absolute: t -> path
val relative: t -> path option
val uri: t -> path
val last: t -> path

val create_absolute: ?follow_symbolic_links: bool -> path -> t
val create_relative: root: t -> relative: path -> t
val get_relative_to_root: root: t -> path: t -> path option
val from_uri: path -> t option

val current_working_directory: unit -> t

val append: t -> element: path -> t

module AppendOperator : sig
  val (^|): t -> path -> t
end

val real_path: t -> t

val is_directory: t -> bool
val file_exists: t -> bool
val list
  :  ?file_filter:(string -> bool)
  -> ?directory_filter:(string -> bool)
  -> root: t
  -> unit
  -> t list
val directory_contains: ?follow_symlinks: bool -> directory: t -> t -> bool
val search_upwards: target: string -> root: t -> t option

val remove: t -> unit

val readlink: t -> path option

module Map: Map.S with type Key.t = t

module SearchPath: sig
  type t =
    | Root of path_t
    | Subdirectory of { root: path_t; subdirectory: string }

  [@@deriving eq, show]

  val get_root: t -> path_t
  val to_path: t -> path_t
  val create: path -> t
end

val search_for_path: search_path: SearchPath.t list -> path:t -> t option
