(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type path = string [@@deriving compare, show, sexp, hash]

type t [@@deriving compare, show, sexp, hash]

val equal : t -> t -> bool

val absolute : t -> path

val last : t -> path

val create_absolute : ?follow_symbolic_links:bool -> path -> t

val create_relative : root:t -> relative:path -> t

val create_directory_recursively : ?permission:int -> t -> (unit, string) Result.t

val ensure_parent_directory_exists : ?permission:int -> t -> (unit, string) Result.t

val get_relative_to_root : root:t -> path:t -> path option

val current_working_directory : unit -> t

val append : t -> element:path -> t

val follow_symbolic_link : t -> t option

val is_directory : t -> bool

val get_suffix_path : t -> string

val is_path_python_stub : string -> bool

val is_path_python_init : string -> bool

val file_exists : t -> bool

val list
  :  ?file_filter:(string -> bool) ->
  ?directory_filter:(string -> bool) ->
  root:t ->
  unit ->
  t list

val read_directory_ordered : t -> t list

val directory_contains : directory:t -> t -> bool

module FileType : sig
  type t =
    | File
    | Directory
end

val search_upwards : target:string -> target_type:FileType.t -> root:t -> t option

val remove : t -> unit

val remove_if_exists : t -> unit

(* Remove every file under the given directory, but not the directory itself. *)
val remove_contents_of_directory : t -> (unit, string) Result.t

val with_suffix : t -> suffix:string -> t

val get_directory : t -> t

(* paths can be files or directories *)
val get_matching_files_recursively : suffix:string -> paths:t list -> t list
