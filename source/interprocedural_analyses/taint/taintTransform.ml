(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TaintTransform: defines taint transforms.
 *
 * A taint transform is an operator that can be applied on a taint to change its
 * semantic.
 *)

open Core

type named_transform = {
  name: string;
  location: (JsonParsing.JsonAst.LocationWithPath.t option[@hash.ignore] [@sexp.opaque]);
}
[@@deriving compare, eq, hash, sexp]

type t =
  | Named of named_transform
  (* Invariant: set is not empty. *)
  | Sanitize of SanitizeTransformSet.t
[@@deriving compare, eq, hash, sexp]

let pp formatter = function
  | Named { name; _ } -> Format.fprintf formatter "%s" name
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


let get_location = function
  | Named { location; _ } -> location
  | Sanitize _ -> None
