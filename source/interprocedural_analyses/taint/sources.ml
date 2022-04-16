(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

let name = "source"

module T = struct
  type t =
    | Attach
    | NamedSource of string
    | ParametricSource of {
        source_name: string;
        subkind: string;
      }
    | Transform of {
        (* Invariant: concatenation of local @ global is non-empty. *)
        local: TaintTransforms.t;
        global: TaintTransforms.t;
        (* Invariant: not a transform. *)
        base: t;
      }
  [@@deriving compare, eq]

  let rec pp formatter = function
    | Attach -> Format.fprintf formatter "Attach"
    | NamedSource name -> Format.fprintf formatter "%s" name
    | ParametricSource { source_name; subkind } ->
        Format.fprintf formatter "%s[%s]" source_name subkind
    | Transform { local; global; base } ->
        TaintTransforms.pp_kind ~formatter ~pp_base:pp ~local ~global ~base


  let show = Format.asprintf "%a" pp
end

include T

let ignore_kind_at_call = function
  | Attach -> true
  | _ -> false


let apply_call = function
  | Transform { local; global; base } ->
      Transform
        { local = TaintTransforms.empty; global = TaintTransforms.merge ~local ~global; base }
  | source -> source


module Set = struct
  include Stdlib.Set.Make (struct
    include T
  end)

  let show set =
    set |> elements |> List.map ~f:T.show |> String.concat ~sep:", " |> Format.asprintf "[%s]"


  let pp format set = Format.fprintf format "%s" (show set)

  let to_sanitize_transforms_exn set =
    let to_transform = function
      | NamedSource name -> SanitizeTransform.Source.Named name
      | source -> Format.asprintf "cannot sanitize the source `%a`" T.pp source |> failwith
    in
    set |> elements |> List.map ~f:to_transform |> SanitizeTransform.SourceSet.of_list


  let is_singleton set =
    (* The only way to implement this in O(1) is with `for_all` or `exists`. *)
    (not (is_empty set))
    &&
    let count = ref 0 in
    for_all
      (fun _ ->
        incr count;
        !count = 1)
      set


  let as_singleton set = if is_singleton set then Some (choose set) else None
end

module Map = struct
  include Stdlib.Map.Make (struct
    include T
  end)

  let of_alist_exn =
    let add map (key, data) =
      update
        key
        (function
          | None -> Some data
          | Some _ -> failwith "key already exists")
        map
    in
    List.fold ~init:empty ~f:add


  let to_alist map =
    let gather key data sofar = (key, data) :: sofar in
    fold gather map []
end

let discard_subkind = function
  | ParametricSource { source_name; _ } -> NamedSource source_name
  | source -> source


let discard_transforms = function
  | Transform { base; _ } -> base
  | source -> source


let discard_sanitize_transforms = function
  | Transform { base; local; global } ->
      let local = TaintTransforms.discard_sanitize_transforms local in
      let global = TaintTransforms.discard_sanitize_transforms global in
      if TaintTransforms.is_empty local && TaintTransforms.is_empty global then
        base
      else
        Transform { base; local; global }
  | source -> source


let extract_sanitized_sources_from_transforms transforms =
  let extract (SanitizeTransform.Source.Named name) sources = Set.add (NamedSource name) sources in
  SanitizeTransform.SourceSet.fold extract transforms Set.empty


let extract_sanitize_transforms = function
  | Transform { local; global; _ } ->
      TaintTransforms.merge ~local ~global |> TaintTransforms.get_sanitize_transforms
  | _ -> SanitizeTransformSet.empty


let rec base_as_sanitizer = function
  | NamedSource name
  | ParametricSource { source_name = name; _ } ->
      Some (SanitizeTransform.Source (SanitizeTransform.Source.Named name))
  | Transform { base; _ } -> base_as_sanitizer base
  | Attach -> None


let apply_sanitize_transforms transforms source =
  match source with
  | Attach -> Some Attach
  | NamedSource _
  | ParametricSource _ -> (
      match
        TaintTransforms.of_sanitize_transforms
          ~preserve_sanitize_sources:false
          ~preserve_sanitize_sinks:true
          ~base:(base_as_sanitizer source)
          transforms
      with
      | None -> None
      | Some local when TaintTransforms.is_empty local -> Some source
      | Some local -> Some (Transform { local; global = TaintTransforms.empty; base = source }))
  | Transform { local; global; base } -> (
      match
        TaintTransforms.add_sanitize_transforms
          ~preserve_sanitize_sources:false
          ~preserve_sanitize_sinks:true
          ~base:(base_as_sanitizer base)
          ~local
          ~global
          transforms
      with
      | None -> None
      | Some local -> Some (Transform { local; global; base }))


let apply_transforms transforms order source =
  match source with
  | Attach -> Some Attach
  | NamedSource _
  | ParametricSource _ -> (
      match
        TaintTransforms.add_transforms
          ~preserve_sanitize_sources:false
          ~preserve_sanitize_sinks:true
          ~base:(base_as_sanitizer source)
          ~local:TaintTransforms.empty
          ~global:TaintTransforms.empty
          ~order:TaintTransforms.Order.Forward
          ~to_add:transforms
          ~to_add_order:order
      with
      | None -> None
      | Some local when TaintTransforms.is_empty local -> Some source
      | Some local -> Some (Transform { local; global = TaintTransforms.empty; base = source }))
  | Transform { local; global; base } -> (
      match
        TaintTransforms.add_transforms
          ~preserve_sanitize_sources:false
          ~preserve_sanitize_sinks:true
          ~base:(base_as_sanitizer base)
          ~local
          ~global
          ~order:TaintTransforms.Order.Forward
          ~to_add:transforms
          ~to_add_order:order
      with
      | None -> None
      | Some local -> Some (Transform { local; global; base }))


let get_named_transforms = function
  | Transform { local; global; _ } ->
      TaintTransforms.merge ~local ~global |> TaintTransforms.get_named_transforms
  | _ -> []


let contains_sanitize_transforms source sanitize_transforms =
  SanitizeTransformSet.subset sanitize_transforms (extract_sanitize_transforms source)
