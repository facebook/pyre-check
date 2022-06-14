(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type mode =
  | Debug
  | Strict
  | Unsafe
  | Declare
[@@deriving compare, show, sexp, hash]

type local_mode =
  | Strict
  | Unsafe
  | Declare
  | PlaceholderStub
[@@deriving compare, show, sexp, hash]

module TypecheckFlags : sig
  type t = {
    local_mode: local_mode Node.t option;
    unused_local_modes: local_mode Node.t list;
    ignore_codes: int list;
    ignore_lines: Ignore.t list;
  }
  [@@deriving compare, show, hash, sexp]

  val is_placeholder_stub : local_mode Node.t option -> bool

  val create_for_testing
    :  ?local_mode:local_mode Node.t ->
    ?unused_local_modes:local_mode Node.t list ->
    ?ignore_codes:int list ->
    ?ignore_lines:Ignore.t list ->
    unit ->
    t

  val parse : qualifier:Reference.t -> string list -> t
end

type t = {
  typecheck_flags: TypecheckFlags.t;
  module_path: ModulePath.t;
  top_level_unbound_names: Statement.Define.NameAccess.t list;
  statements: Statement.t list;
}
[@@deriving compare, hash, show, sexp]

val ignored_lines_including_format_strings
  :  ?collect_format_strings_with_ignores:
       (ignore_line_map:Ignore.t list Int.Map.t -> t -> (Expression.t * Ignore.t list) list) ->
  t ->
  Ignore.t list

val create_from_module_path
  :  ?collect_format_strings_with_ignores:
       (ignore_line_map:Ignore.t list Int.Map.t -> t -> (Expression.t * Ignore.t list) list) ->
  typecheck_flags:TypecheckFlags.t ->
  module_path:ModulePath.t ->
  Statement.t list ->
  t

val create
  :  ?typecheck_flags:TypecheckFlags.t ->
  ?relative:string ->
  ?is_external:bool ->
  ?priority:int ->
  Statement.t list ->
  t

val pp_all : Format.formatter -> t -> unit

val location_insensitive_compare : t -> t -> int

val mode : configuration:Configuration.Analysis.t -> local_mode:local_mode Node.t option -> mode

val ignore_lines : t -> Ignore.t list

val statements : t -> Statement.t list

val top_level_define : t -> Statement.Define.t

val top_level_define_node : t -> Statement.Define.t Node.t
