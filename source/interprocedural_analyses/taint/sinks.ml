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
  [@@deriving compare, hash, sexp, show]

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
  [@@deriving compare, hash, sexp]

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
        { local = TaintTransforms.empty; global = TaintTransforms.merge ~local ~global; base }
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
  | ParametricSink { sink_name; _ } -> NamedSink sink_name
  | sink -> sink


let discard_transforms = function
  (* TODO(T90698159): Assumes only sanitizing transforms present, revisit. *)
  | Transform { base; _ } -> base
  | sink -> sink


let discard_sanitize_transforms = function
  | Transform { base; local; global } ->
      let local = TaintTransforms.discard_sanitize_transforms local in
      let global = TaintTransforms.discard_sanitize_transforms global in
      if TaintTransforms.is_empty local && TaintTransforms.is_empty global then
        base
      else
        Transform { base; local; global }
  | sink -> sink


let extract_sanitized_sinks_from_transforms transforms =
  let extract transform sinks =
    match transform with
    | SanitizeTransform.NamedSink name -> Set.add (NamedSink name) sinks
    | _ -> sinks
  in
  SanitizeTransform.Set.fold extract transforms Set.empty


let extract_sanitize_transforms = function
  | Transform { local; global; _ } ->
      TaintTransforms.merge ~local ~global |> TaintTransforms.get_sanitize_transforms
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
          local = TaintTransforms.of_sanitize_transforms transforms;
          global = TaintTransforms.empty;
          base = sink;
        }
  | Transform { local; global; base } ->
      let transforms = SanitizeTransform.Set.diff transforms (extract_sanitize_transforms sink) in
      Transform { local = TaintTransforms.add_sanitize_transforms local transforms; global; base }


let apply_sanitize_sink_transforms transforms sink =
  match sink with
  | LocalReturn
  | Transform { base = LocalReturn; _ } ->
      apply_sanitize_transforms transforms sink
  | _ -> sink


let apply_transforms transforms order sink =
  (* We should only apply sink sanitizers on tito. *)
  let transforms =
    match sink with
    | LocalReturn
    | Transform { base = LocalReturn; _ } ->
        transforms
    | _ -> TaintTransforms.discard_sanitize_sink_transforms transforms
  in
  match sink with
  | _ when TaintTransforms.is_empty transforms -> sink
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
          local =
            TaintTransforms.add_transforms
              ~transforms:TaintTransforms.empty
              ~order:TaintTransforms.Order.Backward
              ~to_add:transforms
              ~to_add_order:order;
          global = TaintTransforms.empty;
          base = sink;
        }
  | Transform { local; global; base } ->
      Transform
        {
          local =
            TaintTransforms.add_transforms
              ~transforms:local
              ~order:TaintTransforms.Order.Backward
              ~to_add:transforms
              ~to_add_order:order;
          global;
          base;
        }


let get_named_transforms = function
  | Transform { local; global; _ } ->
      TaintTransforms.merge ~local ~global |> TaintTransforms.get_named_transforms
  | _ -> []


let contains_sanitize_transform sink sanitize_transform =
  SanitizeTransform.Set.mem sanitize_transform (extract_sanitize_transforms sink)
