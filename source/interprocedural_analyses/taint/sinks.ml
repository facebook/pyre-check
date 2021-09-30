(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

let name = "sink"

module T = struct
  type partial_sink = {
    kind: string;
    label: string;
  }
  [@@deriving compare, eq, sexp, show, hash]

  type t =
    | Attach
    | PartialSink of partial_sink
    | TriggeredPartialSink of partial_sink
    | LocalReturn (* Special marker to describe function in-out behavior *)
    | NamedSink of string
    | ParametricSink of {
        sink_name: string;
        subkind: string;
      }
    | ParameterUpdate of int (* Special marker to describe side effect in-out behavior *)
    | AddFeatureToArgument
    (* Special marker to designate modifying the state the parameter passed in. *)
    | Transform of {
        (* Invariant: concatenation of local @ global is non-empty. *)
        (* Invariant: local @ global is the temporal order in which transforms
         * are applied in the code. *)
        local: TaintTransform.t list;
        global: TaintTransform.t list;
        (* Invariant: not a transform. *)
        base: t;
      }
  [@@deriving compare, eq, sexp, hash]

  let rec pp formatter = function
    | Attach -> Format.fprintf formatter "Attach"
    | PartialSink { kind; label } -> Format.fprintf formatter "PartialSink[%s[%s]]" kind label
    | TriggeredPartialSink { kind; label } ->
        Format.fprintf formatter "TriggeredPartialSink[%s[%s]]" kind label
    | LocalReturn -> Format.fprintf formatter "LocalReturn"
    | NamedSink name -> Format.fprintf formatter "%s" name
    | ParametricSink { sink_name; subkind } -> Format.fprintf formatter "%s[%s]" sink_name subkind
    | ParameterUpdate index -> Format.fprintf formatter "ParameterUpdate%d" index
    | AddFeatureToArgument -> Format.fprintf formatter "AddFeatureToArgument"
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
  | sink -> sink


module Set = struct
  include Stdlib.Set.Make (struct
    include T
  end)

  let show set =
    set |> elements |> List.map ~f:T.show |> String.concat ~sep:", " |> Format.asprintf "[%s]"


  let pp format set = Format.fprintf format "%s" (show set)

  let to_sanitize_taint_transforms_exn set =
    let to_transform = function
      | NamedSink name -> TaintTransform.SanitizeNamedSink name
      | sink -> Format.asprintf "cannot sanitize the sink `%a`" T.pp sink |> failwith
    in
    set |> elements |> List.map ~f:to_transform
end

let discard_subkind = function
  | ParametricSink { sink_name; _ } -> NamedSink sink_name
  | sink -> sink


let discard_transforms = function
  | Transform { base; _ } -> base
  | sink -> sink


let extract_sanitized_sinks_from_transforms =
  let extract sinks = function
    | TaintTransform.SanitizeNamedSink name -> Set.add (NamedSink name) sinks
    | _ -> sinks
  in
  List.fold ~init:Set.empty ~f:extract


let extract_transforms = function
  | Transform { local; global; _ } -> local @ global
  | _ -> []


let rec extract_partial_sink = function
  | Transform { base; _ } -> extract_partial_sink base
  | PartialSink { kind; label } -> Some { kind; label }
  | _ -> None


let apply_taint_transform transform sink =
  match sink with
  | Attach
  | AddFeatureToArgument ->
      sink
  | PartialSink _
  | TriggeredPartialSink _
  | LocalReturn
  | NamedSink _
  | ParametricSink _
  | ParameterUpdate _ ->
      Transform { local = [transform]; global = []; base = sink }
  | Transform { local; global; base } ->
      (* For now, we only have sanitized taint transforms.
       * They are subsumed by already existing transforms. *)
      if
        List.mem local transform ~equal:TaintTransform.equal
        || List.mem global transform ~equal:TaintTransform.equal
      then
        sink
      else
        Transform { local = transform :: local; global; base }


let apply_taint_transforms transforms sink =
  (* Transforms are provided in the temporal order in which they are applied,
   * but stored in the reverse temporal order, hence it's a `fold_right`. *)
  List.fold_right transforms ~init:sink ~f:apply_taint_transform


let apply_sanitize_sink_transform transform sink =
  match sink with
  | LocalReturn
  | Transform { base = LocalReturn; _ } ->
      apply_taint_transform transform sink
  | _ -> sink


let apply_sanitize_sink_transforms transforms sink =
  List.fold_right transforms ~init:sink ~f:apply_sanitize_sink_transform
