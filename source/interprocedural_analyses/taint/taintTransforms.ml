(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type t = TaintTransform.t list [@@deriving compare, eq, hash, sexp]

let empty = []

let add_transform transforms transform = transform :: transforms

let add_named_transforms init named_transforms =
  List.fold_right named_transforms ~init ~f:(fun named_transform sofar ->
      add_transform sofar named_transform)


let rev_add_named_transforms init named_transforms =
  List.fold_left named_transforms ~init ~f:add_transform


let add_sanitize_transforms init sanitize_transforms =
  let existing_sanitizers, rest = List.split_while init ~f:TaintTransform.is_sanitize_transform in
  let existing_sanitizers =
    List.filter_map existing_sanitizers ~f:TaintTransform.get_sanitize_transform
    |> SanitizeTransform.Set.of_list
  in
  let sanitizers = SanitizeTransform.Set.union sanitize_transforms existing_sanitizers in
  SanitizeTransform.Set.fold
    (fun sanitize_transform sofar ->
      add_transform sofar (TaintTransform.Sanitize sanitize_transform))
    sanitizers
    rest


let of_named_transforms = add_named_transforms empty

let of_sanitize_transforms = add_sanitize_transforms empty

let is_empty = List.is_empty

let get_named_transforms = List.filter ~f:TaintTransform.is_named_transform

let get_sanitize_transforms transforms =
  List.take_while transforms ~f:TaintTransform.is_sanitize_transform
  |> List.filter_map ~f:TaintTransform.get_sanitize_transform
  |> SanitizeTransform.Set.of_list


let discard_sanitize_transforms = List.filter ~f:TaintTransform.is_named_transform

let merge ~local ~global = local @ global

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
