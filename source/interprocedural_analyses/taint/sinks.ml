(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
  [@@deriving compare, show]

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
        local: TaintTransforms.t;
        global: TaintTransforms.t;
        (* Invariant: not a transform. *)
        base: t;
      }
  [@@deriving compare]

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
        TaintTransforms.pp_kind ~formatter ~pp_base:pp ~local ~global ~base


  let equal = [%compare.equal: t]

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
  | sink -> sink


module Set = struct
  include Stdlib.Set.Make (struct
    include T
  end)

  let show set =
    set |> elements |> List.map ~f:T.show |> String.concat ~sep:", " |> Format.asprintf "[%s]"


  let pp format set = Format.fprintf format "%s" (show set)

  let to_sanitize_transforms_exn set =
    let to_transform = function
      | NamedSink name -> SanitizeTransform.NamedSink name
      | sink -> Format.asprintf "cannot sanitize the sink `%a`" T.pp sink |> failwith
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
  | ParametricSink { sink_name; _ } -> NamedSink sink_name
  | sink -> sink


let discard_transforms = function
  (* TODO(T90698159): Assumes only sanitizing transforms present, revisit. *)
  | Transform { base; _ } -> base
  | sink -> sink


let discard_sanitize_transforms =
  (* TODO(T90698159): Assumes only sanitizing transforms present, revisit. *)
  discard_transforms


let extract_sanitized_sinks_from_transforms transforms =
  let extract transform sinks =
    match transform with
    | SanitizeTransform.NamedSink name -> Set.add (NamedSink name) sinks
    | _ -> sinks
  in
  SanitizeTransform.Set.fold extract transforms Set.empty


let extract_sanitize_transforms = function
  | Transform { local; global; _ } -> SanitizeTransform.Set.union local.sanitize global.sanitize
  | _ -> SanitizeTransform.Set.empty


let rec extract_partial_sink = function
  | Transform { base; _ } -> extract_partial_sink base
  | PartialSink { kind; label } -> Some { kind; label }
  | _ -> None


let apply_sanitize_transforms transforms sink =
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
      Transform
        {
          local = { TaintTransforms.sanitize = transforms; ordered = [] };
          global = TaintTransforms.empty;
          base = sink;
        }
  | Transform { local; global; base } ->
      let transforms = SanitizeTransform.Set.diff transforms global.sanitize in
      Transform
        {
          local = { local with sanitize = SanitizeTransform.Set.union local.sanitize transforms };
          global;
          base;
        }


let apply_sanitize_sink_transforms transforms sink =
  match sink with
  | LocalReturn
  | Transform { base = LocalReturn; _ } ->
      apply_sanitize_transforms transforms sink
  | _ -> sink


let apply_ordered_transforms _transforms sink = sink
