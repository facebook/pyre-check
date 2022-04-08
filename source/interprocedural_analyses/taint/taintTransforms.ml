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

let empty = []

let add_transform transforms transform = transform :: transforms

let add_named_transforms init named_transforms =
  List.fold_right named_transforms ~init ~f:(fun named_transform sofar ->
      add_transform sofar named_transform)


let rev_add_named_transforms init named_transforms =
  List.fold_left named_transforms ~init ~f:add_transform


(* Split a list of transforms into sanitizers present at the beginning and the rest. *)
let split_sanitizers transforms =
  let rec split sanitizers transforms =
    match transforms with
    | []
    | TaintTransform.Named _ :: _ ->
        sanitizers, transforms
    | TaintTransform.Sanitize sanitizer :: tail ->
        let sanitizers = SanitizeTransform.Set.add sanitizer sanitizers in
        split sanitizers tail
  in
  split SanitizeTransform.Set.empty transforms


let add_sanitize_transforms transforms sanitizers =
  let existing_sanitizers, rest = split_sanitizers transforms in
  let sanitizers = SanitizeTransform.Set.union sanitizers existing_sanitizers in
  SanitizeTransform.Set.fold
    (fun sanitizer transforms -> TaintTransform.Sanitize sanitizer :: transforms)
    sanitizers
    rest


let of_named_transforms = add_named_transforms empty

let of_sanitize_transforms = add_sanitize_transforms empty

let is_empty = List.is_empty

let get_named_transforms = List.filter ~f:TaintTransform.is_named_transform

let get_sanitize_transforms transforms = fst (split_sanitizers transforms)

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
