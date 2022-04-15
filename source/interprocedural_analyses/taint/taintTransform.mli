(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | Named of string
  (* Invariant: set is not empty. *)
  | Sanitize of SanitizeTransformSet.t
[@@deriving compare, eq, hash, sexp]

val pp : Format.formatter -> t -> unit

val show : t -> string

val is_named_transform : t -> bool

val is_sanitize_transforms : t -> bool

val get_sanitize_transforms : t -> SanitizeTransformSet.t option
