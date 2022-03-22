(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type t = {
  ordered: TaintTransform.t list;
  sanitize: SanitizeTransform.Set.t;
}
[@@deriving compare, eq, hash, sexp]

let empty = { ordered = []; sanitize = SanitizeTransform.Set.empty }

let add_transform { ordered; sanitize } transform =
  match transform with
  | TaintTransform.Named _ -> { ordered = transform :: ordered; sanitize }
  | TaintTransform.Sanitize sanitize_transform ->
      { ordered; sanitize = SanitizeTransform.Set.add sanitize_transform sanitize }


let add_named_transforms init named_transforms =
  List.fold_right named_transforms ~init ~f:(fun named_transform sofar ->
      add_transform sofar named_transform)


let rev_add_named_transforms init named_transforms =
  List.fold_left named_transforms ~init ~f:add_transform


let add_sanitize_transforms init sanitize_transforms =
  SanitizeTransform.Set.fold
    (fun sanitize_transform sofar ->
      add_transform sofar (TaintTransform.Sanitize sanitize_transform))
    sanitize_transforms
    init


let of_named_transforms = add_named_transforms empty

let of_sanitize_transforms = add_sanitize_transforms empty

let is_empty { ordered; sanitize } =
  List.is_empty ordered && SanitizeTransform.Set.is_empty sanitize


let get_named_transforms { ordered; _ } =
  ordered |> List.filter ~f:TaintTransform.is_named_transform


let get_sanitize_transforms { sanitize; _ } = sanitize

let discard_sanitize_transforms { ordered; _ } =
  {
    ordered = List.filter ordered ~f:TaintTransform.is_named_transform;
    sanitize = SanitizeTransform.Set.empty;
  }


let merge ~local ~global =
  {
    ordered = local.ordered @ global.ordered;
    sanitize = SanitizeTransform.Set.union local.sanitize global.sanitize;
  }


let show_transforms { ordered; sanitize } =
  (sanitize |> SanitizeTransform.Set.elements |> List.map ~f:SanitizeTransform.show)
  @ (ordered |> List.map ~f:TaintTransform.show)
  |> String.concat ~sep:":"


let pp_transforms formatter transforms = Format.fprintf formatter "%s" (show_transforms transforms)

let pp_kind ~formatter ~pp_base ~local ~global ~base =
  if is_empty local then
    Format.fprintf formatter "%a:%a" pp_transforms global pp_base base
  else if is_empty global then
    Format.fprintf formatter "%a@@%a" pp_transforms local pp_base base
  else
    Format.fprintf formatter "%a@@%a:%a" pp_transforms local pp_transforms global pp_base base
