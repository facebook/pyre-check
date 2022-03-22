(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t [@@deriving compare, eq, hash, sexp]

val add_named_transforms : t -> TaintTransform.t list -> t

val add_sanitize_transforms : t -> SanitizeTransform.Set.t -> t

val discard_sanitize_transforms : t -> t

val empty : t

val get_named_transforms : t -> TaintTransform.t list

val get_sanitize_transforms : t -> SanitizeTransform.Set.t

val is_empty : t -> bool

val merge : local:t -> global:t -> t

val of_named_transforms : TaintTransform.t list -> t

val of_sanitize_transforms : SanitizeTransform.Set.t -> t

val pp_kind
  :  formatter:Format.formatter ->
  pp_base:(Format.formatter -> 'a -> unit) ->
  local:t ->
  global:t ->
  base:'a ->
  unit

val rev_add_named_transforms : t -> TaintTransform.t list -> t
