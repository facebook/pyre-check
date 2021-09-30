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
        (* Invariant: local @ global is the *reverse* temporal order in which transforms
         * are applied in the code. *)
        local: TaintTransform.t list;
        global: TaintTransform.t list;
        (* Invariant: not a transform. *)
        base: t;
      }
  [@@deriving compare, eq, sexp, hash]

  let rec pp formatter = function
    | Attach -> Format.fprintf formatter "Attach"
    | NamedSource name -> Format.fprintf formatter "%s" name
    | ParametricSource { source_name; subkind } ->
        Format.fprintf formatter "%s[%s]" source_name subkind
    | Transform { local; global; base } ->
        TaintTransform.pp_kind ~formatter ~pp_base:pp ~local ~global ~base


  let show = Format.asprintf "%a" pp
end

include T

let ignore_kind_at_call = function
  | Attach -> true
  | _ -> false


let apply_call = function
  | Transform { local; global; base } -> Transform { local = []; global = local @ global; base }
  | source -> source


module Set = struct
  include Stdlib.Set.Make (struct
    include T
  end)

  let show set =
    set |> elements |> List.map ~f:T.show |> String.concat ~sep:", " |> Format.asprintf "[%s]"


  let pp format set = Format.fprintf format "%s" (show set)

  let to_sanitize_taint_transforms_exn set =
    let to_transform = function
      | NamedSource name -> TaintTransform.SanitizeNamedSource name
      | source -> Format.asprintf "cannot sanitize the source `%a`" T.pp source |> failwith
    in
    set |> elements |> List.map ~f:to_transform
end

let discard_subkind = function
  | ParametricSource { source_name; _ } -> NamedSource source_name
  | source -> source


let discard_transforms = function
  | Transform { base; _ } -> base
  | source -> source


let extract_sanitized_sources_from_transforms =
  let extract sources = function
    | TaintTransform.SanitizeNamedSource name -> Set.add (NamedSource name) sources
    | _ -> sources
  in
  List.fold ~init:Set.empty ~f:extract


let extract_transforms = function
  | Transform { local; global; _ } -> local @ global
  | _ -> []


let apply_taint_transform source transform =
  match source with
  | Attach -> Attach
  | NamedSource _
  | ParametricSource _ ->
      Transform { local = [transform]; global = []; base = source }
  | Transform { local; global; base } ->
      if
        List.mem local transform ~equal:TaintTransform.equal
        || List.mem global transform ~equal:TaintTransform.equal
      then
        source
      else
        Transform { local = transform :: local; global; base }


(* Transforms must be provided in the temporal order in which they are applied. *)
let apply_taint_transforms transforms source =
  List.fold_left transforms ~init:source ~f:apply_taint_transform


let apply_sanitize_sink_transforms = apply_taint_transforms
