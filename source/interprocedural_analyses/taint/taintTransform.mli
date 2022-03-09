(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t =
  | Named of string
  | Sanitize of SanitizeTransform.t
[@@deriving compare, eq, hash, sexp]

val pp : Format.formatter -> t -> unit

val show : t -> string

val is_named_transform : t -> bool
