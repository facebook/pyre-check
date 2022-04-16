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


let preserve_sanitizers ~preserve_sanitize_sources ~preserve_sanitize_sinks sanitizers =
  let sanitizers =
    if not preserve_sanitize_sources then
      { sanitizers with SanitizeTransformSet.sources = SanitizeTransform.SourceSet.empty }
    else
      sanitizers
  in
  let sanitizers =
    if not preserve_sanitize_sinks then
      { sanitizers with SanitizeTransformSet.sinks = SanitizeTransform.SinkSet.empty }
    else
      sanitizers
  in
  sanitizers


let add_sanitize_transforms_internal
    ~preserve_sanitize_sources
    ~preserve_sanitize_sinks
    ~current_base
    ~global_sanitizers
    transforms
    sanitizers
  =
  match current_base with
  | Some base when SanitizeTransformSet.mem sanitizers base ->
      (* Taint is sanitized and should be removed. *)
      None
  | _ ->
      let sanitizers =
        preserve_sanitizers ~preserve_sanitize_sources ~preserve_sanitize_sinks sanitizers
      in
      let sanitizers = SanitizeTransformSet.diff sanitizers global_sanitizers in
      if SanitizeTransformSet.is_empty sanitizers then
        Some transforms
      else
        let existing_sanitizers, rest = split_sanitizers transforms in
        let sanitizers = SanitizeTransformSet.union sanitizers existing_sanitizers in
        Some (TaintTransform.Sanitize sanitizers :: rest)


let get_global_sanitizers ~local ~global =
  if List.exists local ~f:TaintTransform.is_named_transform then
    SanitizeTransformSet.empty
  else
    fst (split_sanitizers global)


let get_current_base ~local ~global ~base =
  match base with
  | None -> None
  | Some _ when List.exists local ~f:TaintTransform.is_named_transform -> None
  | Some _ when List.exists global ~f:TaintTransform.is_named_transform -> None
  | _ -> base


let add_sanitize_transforms
    ~preserve_sanitize_sources
    ~preserve_sanitize_sinks
    ~base
    ~local
    ~global
    sanitizers
  =
  let global_sanitizers = get_global_sanitizers ~local ~global in
  let current_base = get_current_base ~local ~global ~base in
  add_sanitize_transforms_internal
    ~preserve_sanitize_sources
    ~preserve_sanitize_sinks
    ~current_base
    ~global_sanitizers
    local
    sanitizers


type add_transform_result = {
  (* None represents a sanitized taint. *)
  transforms: TaintTransform.t list option;
  (* None represents a kind that cannot be sanitized. *)
  current_base: SanitizeTransform.t option;
  global_sanitizers: SanitizeTransformSet.t;
}

let add_transform
    ~preserve_sanitize_sources
    ~preserve_sanitize_sinks
    ~current_base
    ~global_sanitizers
    transforms
  = function
  | TaintTransform.Named _ as named_transform ->
      {
        transforms = Some (add_named_transform transforms named_transform);
        current_base = None;
        global_sanitizers = SanitizeTransformSet.empty;
      }
  | TaintTransform.Sanitize sanitizers ->
      let transforms =
        add_sanitize_transforms_internal
          ~preserve_sanitize_sources
          ~preserve_sanitize_sinks
          ~current_base
          ~global_sanitizers
          transforms
          sanitizers
      in
      { transforms; current_base; global_sanitizers }


let add_backward_into_forward_transforms
    ~preserve_sanitize_sources
    ~preserve_sanitize_sinks
    ~base
    ~local
    ~global
    ~to_add
  =
  let rec add ({ transforms; current_base; global_sanitizers } as sofar) to_add =
    match transforms, to_add with
    | None, _
    | Some _, [] ->
        sofar
    | Some transforms, head :: tail ->
        add
          (add_transform
             ~preserve_sanitize_sources
             ~preserve_sanitize_sinks
             ~current_base
             ~global_sanitizers
             transforms
             head)
          tail
  in
  let global_sanitizers = get_global_sanitizers ~local ~global in
  let current_base = get_current_base ~local ~global ~base in
  let { transforms; _ } = add { transforms = Some local; current_base; global_sanitizers } to_add in
  transforms


let add_backward_into_backward_transforms
    ~preserve_sanitize_sources
    ~preserve_sanitize_sinks
    ~base
    ~local
    ~global
    ~to_add
  =
  let rec add sofar = function
    | [] -> sofar
    | head :: tail -> (
        match add sofar tail with
        | { transforms = None; _ } as sofar -> sofar
        | { transforms = Some transforms; current_base; global_sanitizers } ->
            add_transform
              ~preserve_sanitize_sources
              ~preserve_sanitize_sinks
              ~current_base
              ~global_sanitizers
              transforms
              head)
  in
  let global_sanitizers = get_global_sanitizers ~local ~global in
  let current_base = get_current_base ~local ~global ~base in
  let { transforms; _ } = add { transforms = Some local; current_base; global_sanitizers } to_add in
  transforms


let add_transforms
    ~preserve_sanitize_sources
    ~preserve_sanitize_sinks
    ~base
    ~local
    ~global
    ~order
    ~to_add
    ~to_add_order
  =
  match order, to_add_order with
  | Order.Forward, Order.Backward ->
      add_backward_into_forward_transforms
        ~preserve_sanitize_sources
        ~preserve_sanitize_sinks
        ~base
        ~local
        ~global
        ~to_add
  | Order.Backward, Order.Backward ->
      add_backward_into_backward_transforms
        ~preserve_sanitize_sources
        ~preserve_sanitize_sinks
        ~base
        ~local
        ~global
        ~to_add
  | _ ->
      Format.asprintf
        "unsupported: add_transforms ~order:%a ~to_add_order:%a"
        Order.pp
        order
        Order.pp
        to_add_order
      |> failwith


let of_named_transforms transforms = transforms

let of_sanitize_transforms ~preserve_sanitize_sources ~preserve_sanitize_sinks ~base sanitizers =
  match base with
  | Some base when SanitizeTransformSet.mem sanitizers base -> None
  | _ ->
      let sanitizers =
        preserve_sanitizers ~preserve_sanitize_sources ~preserve_sanitize_sinks sanitizers
      in
      if SanitizeTransformSet.is_empty sanitizers then
        Some []
      else
        Some [TaintTransform.Sanitize sanitizers]


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
