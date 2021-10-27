(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
        sanitize_local: SanitizeTransform.Set.t;
        sanitize_global: SanitizeTransform.Set.t;
        (* Invariant: not a transform. *)
        base: t;
      }
  [@@deriving compare, eq]

  let rec pp formatter = function
    | Attach -> Format.fprintf formatter "Attach"
    | NamedSource name -> Format.fprintf formatter "%s" name
    | ParametricSource { source_name; subkind } ->
        Format.fprintf formatter "%s[%s]" source_name subkind
    | Transform { sanitize_local; sanitize_global; base } ->
        SanitizeTransform.pp_kind
          ~formatter
          ~pp_base:pp
          ~local:sanitize_local
          ~global:sanitize_global
          ~base


  let show = Format.asprintf "%a" pp
end

include T

let ignore_kind_at_call = function
  | Attach -> true
  | _ -> false


let apply_call = function
  | Transform { sanitize_local; sanitize_global; base } ->
      Transform
        {
          sanitize_local = SanitizeTransform.Set.empty;
          sanitize_global = SanitizeTransform.Set.union sanitize_local sanitize_global;
          base;
        }
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
      | NamedSource name -> SanitizeTransform.NamedSource name
      | source -> Format.asprintf "cannot sanitize the source `%a`" T.pp source |> failwith
    in
    set |> elements |> List.map ~f:to_transform |> SanitizeTransform.Set.of_list
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


let discard_sanitize_transforms =
  (* For now, we only have sanitize taint transforms. *)
  discard_transforms


let extract_sanitized_sources_from_transforms transforms =
  let extract transform sources =
    match transform with
    | SanitizeTransform.NamedSource name -> Set.add (NamedSource name) sources
    | _ -> sources
  in
  SanitizeTransform.Set.fold extract transforms Set.empty


let extract_sanitize_transforms = function
  | Transform { sanitize_local; sanitize_global; _ } ->
      SanitizeTransform.Set.union sanitize_local sanitize_global
  | _ -> SanitizeTransform.Set.empty


let apply_sanitize_transforms transforms source =
  match source with
  | Attach -> Attach
  | NamedSource _
  | ParametricSource _ ->
      Transform
        {
          sanitize_local = transforms;
          sanitize_global = SanitizeTransform.Set.empty;
          base = source;
        }
  | Transform { sanitize_local; sanitize_global; base } ->
      let transforms = SanitizeTransform.Set.diff transforms sanitize_global in
      Transform
        {
          sanitize_local = SanitizeTransform.Set.union sanitize_local transforms;
          sanitize_global;
          base;
        }


let apply_sanitize_sink_transforms = apply_sanitize_transforms
