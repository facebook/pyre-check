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

let is_empty { ordered; sanitize } =
  List.is_empty ordered && SanitizeTransform.Set.is_empty sanitize


let concat left right =
  {
    ordered = left.ordered @ right.ordered;
    sanitize = SanitizeTransform.Set.union left.sanitize right.sanitize;
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
