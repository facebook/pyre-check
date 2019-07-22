(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

type path = string [@@deriving compare, eq, show, sexp, hash]

module AbsolutePath : sig
  type t [@@deriving compare, eq, show, sexp, hash]
end

module RelativePath : sig
  type t [@@deriving compare, eq, show, sexp, hash]

  val relative : t -> path
end

type t =
  | Absolute of AbsolutePath.t
  | Relative of RelativePath.t
[@@deriving compare, eq, show, sexp, hash, to_yojson]

val absolute : t -> path

val relative : t -> path option

val uri : t -> path

val last : t -> path

val create_absolute : ?follow_symbolic_links:bool -> path -> t

val create_relative : root:t -> relative:path -> t

val get_relative_to_root : root:t -> path:t -> path option

val from_uri : path -> t option

val current_working_directory : unit -> t

val append : t -> element:path -> t

module AppendOperator : sig
  val ( ^| ) : t -> path -> t
end

val real_path : t -> t

val follow_symbolic_link : t -> t option

val is_directory : t -> bool

val is_path_python_stub : string -> bool

val is_path_python_init : string -> bool

val is_python_stub : t -> bool

val is_python_init : t -> bool

val file_exists : t -> bool

val list
  :  ?file_filter:(string -> bool) ->
  ?directory_filter:(string -> bool) ->
  root:t ->
  unit ->
  t list

val directory_contains : directory:t -> t -> bool

val search_upwards : target:string -> root:t -> t option

val remove : t -> unit

val readlink : t -> path option

module Map : Map.S with type Key.t = t

module Set : Set.S with type Elt.t = t

val build_symlink_map : links:t list -> t Map.t

val with_suffix : t -> suffix:string -> t

val get_directory : t -> t
