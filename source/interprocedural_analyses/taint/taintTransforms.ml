(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TaintTransforms: defines a list of taint transforms, as well as basic
 * operations on them. See transform operations in `TaintTransformOperation`
 * for the actual logic that applies transforms on taint. *)

open Core

module T = struct
  type t = TaintTransform.t list [@@deriving compare, eq, hash, sexp]
end

include T

module Order = struct
  type t =
    (* A:B:C represents the transforms for x in `x = A(B(C(taint)))` *)
    | Forward
    (* A:B:C represents the transforms for x in `taint = C(B(A(x)))` *)
    | Backward
  [@@deriving show]
end

let empty = []

let is_empty = List.is_empty

let merge ~local ~global =
  (* Note that this might introduce consecutive `TaintTransform.Sanitize`, which is expected.
   * Preserving the order is required for trace expansion in the SAPP UI. *)
  local @ global


let of_named_transforms transforms = transforms

let get_named_transforms = List.filter ~f:TaintTransform.is_named_transform

(* Split a list of transforms into sanitizers present at the beginning and the rest. *)
let split_sanitizers transforms =
  (* Note that there might be consecutive `TaintTransform.Sanitize` elements. *)
  let rec split sofar transforms =
    match transforms with
    | []
    | TaintTransform.Named _ :: _ ->
        sofar, transforms
    | TaintTransform.Sanitize sanitizers :: tail ->
        let sofar = SanitizeTransformSet.join sofar sanitizers in
        split sofar tail
  in
  split SanitizeTransformSet.empty transforms


(* Return sanitizers that are still valid (i.e, before a named transform. *)
let get_sanitize_transforms transforms = fst (split_sanitizers transforms)

(* Discard all sanitizers, regardless of whether they are still valid or not. *)
let discard_sanitize_transforms = List.filter ~f:TaintTransform.is_named_transform

let discard_sanitize_source_transforms =
  List.filter_map ~f:(function
      | TaintTransform.Sanitize sanitizers ->
          let sanitizers = { sanitizers with sources = SanitizeTransform.SourceSet.empty } in
          if SanitizeTransformSet.is_empty sanitizers then
            None
          else
            Some (TaintTransform.Sanitize sanitizers)
      | transform -> Some transform)


let discard_sanitize_sink_transforms =
  List.filter_map ~f:(function
      | TaintTransform.Sanitize sanitizers ->
          let sanitizers = { sanitizers with sinks = SanitizeTransform.SinkSet.empty } in
          if SanitizeTransformSet.is_empty sanitizers then
            None
          else
            Some (TaintTransform.Sanitize sanitizers)
      | transform -> Some transform)


let show_transforms transforms =
  List.map transforms ~f:TaintTransform.show |> String.concat ~sep:":"


let pp_transforms formatter transforms = Format.fprintf formatter "%s" (show_transforms transforms)

let pp_kind ~formatter ~pp_base ~local ~global ~base =
  if is_empty local then
    Format.fprintf formatter "%a:%a" pp_transforms global pp_base base
  else if is_empty global then
    Format.fprintf formatter "%a@@%a" pp_transforms local pp_base base
  else
    Format.fprintf formatter "%a@@%a:%a" pp_transforms local pp_transforms global pp_base base


module Set = Stdlib.Set.Make (T)
