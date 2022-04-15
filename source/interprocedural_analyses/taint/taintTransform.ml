(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type t =
  | Named of string
  (* Invariant: set is not empty. *)
  | Sanitize of SanitizeTransformSet.t
[@@deriving compare, eq, hash, sexp]

let pp formatter = function
  | Named transform -> Format.fprintf formatter "%s" transform
  | Sanitize transforms -> SanitizeTransformSet.pp formatter transforms


let show = Format.asprintf "%a" pp

let is_named_transform = function
  | Named _ -> true
  | Sanitize _ -> false


let is_sanitize_transforms = function
  | Named _ -> false
  | Sanitize _ -> true


let get_sanitize_transforms = function
  | Named _ -> None
  | Sanitize sanitize -> Some sanitize
