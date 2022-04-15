(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module T = struct
  type t = TaintTransform.t list [@@deriving compare, eq, hash, sexp]
end

include T
module Set = Stdlib.Set.Make (T)

module Order = struct
  type t =
    (* A:B:C represents the transforms for x in `x = A(B(C(taint)))` *)
    | Forward
    (* A:B:C represents the transforms for x in `taint = C(B(A(x)))` *)
    | Backward
  [@@deriving show]
end

let empty = []

let add_named_transform transforms named_transform = named_transform :: transforms

(* Split a list of transforms into sanitizers present at the beginning and the rest. *)
let split_sanitizers transforms =
  (* Note that there might be consecutive `TaintTransform.Sanitize` elements. *)
  let rec split sofar transforms =
    match transforms with
    | []
    | TaintTransform.Named _ :: _ ->
        sofar, transforms
    | TaintTransform.Sanitize sanitizers :: tail ->
        let sofar = SanitizeTransformSet.union sofar sanitizers in
        split sofar tail
  in
  split SanitizeTransformSet.empty transforms


let add_sanitize_transforms transforms sanitizers =
  let existing_sanitizers, rest = split_sanitizers transforms in
  let sanitizers = SanitizeTransformSet.union sanitizers existing_sanitizers in
  if SanitizeTransformSet.is_empty sanitizers then
    rest
  else
    TaintTransform.Sanitize sanitizers :: rest


let add_transform transforms = function
  | TaintTransform.Named _ as named_transform -> add_named_transform transforms named_transform
  | TaintTransform.Sanitize sanitizers -> add_sanitize_transforms transforms sanitizers


let add_backward_into_forward_transforms ~transforms ~to_add =
  List.fold_left to_add ~init:transforms ~f:add_transform


let add_backward_into_backward_transforms ~transforms ~to_add =
  List.fold_right to_add ~init:transforms ~f:(fun transform transforms ->
      add_transform transforms transform)


let add_transforms ~transforms ~order ~to_add ~to_add_order =
  match order, to_add_order with
  | Order.Forward, Order.Backward -> add_backward_into_forward_transforms ~transforms ~to_add
  | Order.Backward, Order.Backward -> add_backward_into_backward_transforms ~transforms ~to_add
  | _ ->
      Format.asprintf
        "unsupported: add_transforms ~order:%a ~to_add_order:%a"
        Order.pp
        order
        Order.pp
        to_add_order
      |> failwith


let of_named_transforms transforms = transforms

let of_sanitize_transforms sanitizers =
  if SanitizeTransformSet.is_empty sanitizers then [] else [TaintTransform.Sanitize sanitizers]


let is_empty = List.is_empty

let get_named_transforms = List.filter ~f:TaintTransform.is_named_transform

(* This only returns sanitizers that are still valid (i.e, before a named transform. *)
let get_sanitize_transforms transforms = fst (split_sanitizers transforms)

(* This discards all sanitizers, regardless of whether they are still valid or not. *)
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


let merge ~local ~global =
  (* Note that this might introduce consecutive `TaintTransform.Sanitize`, which is expected.
   * Preserving the order is required for trace expansion in the SAPP UI. *)
  local @ global


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
