(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | SanitizeNamedSource of string
  | SanitizeNamedSink of string
[@@deriving compare, eq, sexp, show, hash]

val pp_transforms : Format.formatter -> t list -> unit

val show_transforms : t list -> string

val pp_kind
  :  formatter:Format.formatter ->
  pp_base:(Format.formatter -> 'a -> unit) ->
  local:t list ->
  global:t list ->
  base:'a ->
  unit

val filter_sanitized_sources : t list -> t list

val filter_sanitized_sinks : t list -> t list
