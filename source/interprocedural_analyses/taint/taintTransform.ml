(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type t =
  | Named of string
  | Sanitize of SanitizeTransform.t
[@@deriving compare, eq, hash, sexp]

let pp formatter = function
  | Named transform -> Format.fprintf formatter "%s" transform
  | Sanitize sanitize_transform ->
      Format.fprintf formatter "%a" SanitizeTransform.pp sanitize_transform


let show = Format.asprintf "%a" pp

let is_named_transform = function
  | Named _ -> true
  | Sanitize _ -> false


let is_sanitize_transform = function
  | Named _ -> false
  | Sanitize _ -> true


let is_sanitize_source_transform = function
  | Sanitize (SanitizeTransform.NamedSource _) -> true
  | _ -> false


let is_sanitize_sink_transform = function
  | Sanitize (SanitizeTransform.NamedSink _) -> true
  | _ -> false


let get_sanitize_transform = function
  | Named _ -> None
  | Sanitize sanitize -> Some sanitize
