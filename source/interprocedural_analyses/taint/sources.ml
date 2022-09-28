(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Sources: defines a source kind in our taint representation.
 *
 * For instance, `TaintSource[Header]` is represented as `Sources.NamedSource "Header"`.
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

let make_transform ~local ~global ~base =
  match local, global with
  | [], [] -> base
  | _ -> Transform { local; global; base }


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

  let to_sanitize_transform_set_exn set =
    let to_transform = function
      | NamedSource name -> SanitizeTransform.Source.Named name
      | source -> Format.asprintf "cannot sanitize the source `%a`" T.pp source |> failwith
    in
    set
    |> elements
    |> List.map ~f:to_transform
    |> SanitizeTransform.SourceSet.of_list
    |> SanitizeTransformSet.from_sources


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
      make_transform
        ~local:(TaintTransforms.discard_sanitize_transforms local)
        ~global:(TaintTransforms.discard_sanitize_transforms global)
        ~base
  | source -> source


let extract_sanitized_sources_from_transforms transforms =
  let extract (SanitizeTransform.Source.Named name) sources = Set.add (NamedSource name) sources in
  SanitizeTransform.SourceSet.fold extract transforms Set.empty


let to_sanitized_source_exn = function
  | NamedSource name -> SanitizeTransform.Source.Named name
  | ParametricSource { source_name = name; _ } -> SanitizeTransform.Source.Named name
  | _ -> failwith "Unsupported source sanitizer"


let from_sanitized_source (SanitizeTransform.Source.Named name) = NamedSource name

let extract_sanitize_transforms = function
  | Transform { local; global; _ } ->
      TaintTransforms.merge ~local ~global |> TaintTransforms.get_sanitize_transforms
  | _ -> SanitizeTransformSet.empty


let get_named_transforms = function
  | Transform { local; global; _ } ->
      TaintTransforms.merge ~local ~global |> TaintTransforms.get_named_transforms
  | _ -> []


let contains_sanitize_transforms source sanitize_transforms =
  SanitizeTransformSet.less_or_equal
    ~left:sanitize_transforms
    ~right:(extract_sanitize_transforms source)
