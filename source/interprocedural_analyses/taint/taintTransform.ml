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

type t =
  | Named of string
  (* Invariant: set is not empty. *)
  | Sanitize of SanitizeTransformSet.t
  | TriggeredPartialSink of { triggering_source: string }
    (* We represent triggered partial sinks as partial sinks with TriggeredPartialSink
       transforms. *)
[@@deriving compare, equal, hash, sexp]

let pp formatter = function
  | Named transform -> Format.fprintf formatter "%s" transform
  | Sanitize transforms -> SanitizeTransformSet.pp formatter transforms
  | TriggeredPartialSink { triggering_source } ->
      Format.fprintf formatter "Triggered[%s]" triggering_source


let show = Format.asprintf "%a" pp

let is_named_transform = function
  | Named _ -> true
  | Sanitize _
  | TriggeredPartialSink _ ->
      false


let is_non_sanitize_transform = function
  | Named _
  | TriggeredPartialSink _ ->
      true
  | Sanitize _ -> false


let is_sanitize_transforms = function
  | Named _
  | TriggeredPartialSink _ ->
      false
  | Sanitize _ -> true


let get_sanitize_transforms = function
  | Named _
  | TriggeredPartialSink _ ->
      None
  | Sanitize sanitize -> Some sanitize
