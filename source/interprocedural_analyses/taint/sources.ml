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
        { local = TaintTransforms.empty; global = TaintTransforms.concat local global; base }
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
  | Transform { base; local; global } -> (
      let local_ordered = List.filter local.ordered ~f:TaintTransform.is_named_transform in
      let global_ordered = List.filter global.ordered ~f:TaintTransform.is_named_transform in
      match local_ordered, global_ordered with
      | [], [] -> base
      | _, _ ->
          Transform
            {
              base;
              local = { ordered = local_ordered; sanitize = SanitizeTransform.Set.empty };
              global = { ordered = global_ordered; sanitize = SanitizeTransform.Set.empty };
            })
  | source -> source


let extract_sanitized_sources_from_transforms transforms =
  let extract transform sources =
    match transform with
    | SanitizeTransform.NamedSource name -> Set.add (NamedSource name) sources
    | _ -> sources
  in
  SanitizeTransform.Set.fold extract transforms Set.empty


let extract_sanitize_transforms = function
  | Transform { local; global; _ } -> SanitizeTransform.Set.union local.sanitize global.sanitize
  | _ -> SanitizeTransform.Set.empty


let apply_sanitize_transforms transforms source =
  match source with
  | Attach -> Attach
  | NamedSource _
  | ParametricSource _ ->
      Transform
        {
          local = { TaintTransforms.sanitize = transforms; ordered = [] };
          global = TaintTransforms.empty;
          base = source;
        }
  | Transform { local; global; base } ->
      let transforms = SanitizeTransform.Set.diff transforms global.sanitize in
      Transform
        {
          local = { local with sanitize = SanitizeTransform.Set.union local.sanitize transforms };
          global;
          base;
        }


let apply_sanitize_sink_transforms = apply_sanitize_transforms

let apply_named_transforms transforms source =
  match source with
  | Attach -> Attach
  | NamedSource _
  | ParametricSource _ ->
      Transform
        {
          local =
            {
              TaintTransforms.ordered = List.rev transforms;
              sanitize = SanitizeTransform.Set.empty;
            };
          global = TaintTransforms.empty;
          base = source;
        }
  | Transform { local; global; base } ->
      Transform
        { local = { local with ordered = List.rev_append transforms local.ordered }; global; base }


let get_named_transforms = function
  | Transform { local; global; _ } ->
      local.ordered @ global.ordered |> List.filter ~f:TaintTransform.is_named_transform
  | _ -> []


let contains_sanitize_transform source sanitize_transform =
  match source with
  | Transform
      { local = { sanitize = local_sanitize; _ }; global = { sanitize = global_sanitize; _ }; _ } ->
      SanitizeTransform.Set.mem sanitize_transform local_sanitize
      || SanitizeTransform.Set.mem sanitize_transform global_sanitize
  | _ -> false
