(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Sinks: defines a sink kind in our taint representation.
 *
 * For instance, `TaintSink[SQL]` is represented as `Sinks.NamedSink "SQL"`.
 *)

open Core

let name = "sink"

module T = struct
  type partial_sink = {
    kind: string;
    label: string;
  }
  [@@deriving compare, hash, sexp]

  let show_partial_sink { kind; label } = Format.sprintf "%s[%s]" kind label

  let pp_partial_sink format partial_sink =
    Format.fprintf format "%s" (show_partial_sink partial_sink)


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
    | ExtraTraceSink (* Special marker to show subtraces that end with this sink *)
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
    | PartialSink partial_sink ->
        Format.fprintf formatter "PartialSink[%s]" (show_partial_sink partial_sink)
    | TriggeredPartialSink partial_sink ->
        Format.fprintf formatter "TriggeredPartialSink[%s]" (show_partial_sink partial_sink)
    | LocalReturn -> Format.fprintf formatter "LocalReturn"
    | NamedSink name -> Format.fprintf formatter "%s" name
    | ParametricSink { sink_name; subkind } -> Format.fprintf formatter "%s[%s]" sink_name subkind
    | ParameterUpdate index -> Format.fprintf formatter "ParameterUpdate%d" index
    | AddFeatureToArgument -> Format.fprintf formatter "AddFeatureToArgument"
    | Transform { local; global; base } ->
        TaintTransforms.pp_kind ~formatter ~pp_base:pp ~local ~global ~base
    | ExtraTraceSink -> Format.fprintf formatter "ExtraTraceSink"


  let equal = [%compare.equal: t]

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
  | sink -> sink


module Set = struct
  include Stdlib.Set.Make (struct
    include T
  end)

  let show set =
    set |> elements |> List.map ~f:T.show |> String.concat ~sep:", " |> Format.asprintf "[%s]"


  let pp format set = Format.fprintf format "%s" (show set)

  let to_sanitize_transform_set_exn set =
    let to_transform = function
      | NamedSink name -> SanitizeTransform.Sink.Named name
      | sink -> Format.asprintf "cannot sanitize the sink `%a`" T.pp sink |> failwith
    in
    set
    |> elements
    |> List.map ~f:to_transform
    |> SanitizeTransform.SinkSet.of_list
    |> SanitizeTransformSet.from_sinks


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
  | Transform { base; _ } -> base
  | sink -> sink


let discard_sanitize_transforms = function
  | Transform { base; local; global } ->
      make_transform
        ~local:(TaintTransforms.discard_sanitize_transforms local)
        ~global:(TaintTransforms.discard_sanitize_transforms global)
        ~base
  | sink -> sink


let extract_sanitized_sinks_from_transforms transforms =
  let extract (SanitizeTransform.Sink.Named name) sinks = Set.add (NamedSink name) sinks in
  SanitizeTransform.SinkSet.fold extract transforms Set.empty


let to_sanitized_sink_exn = function
  | NamedSink name -> SanitizeTransform.Sink.Named name
  | ParametricSink { sink_name = name; _ } -> SanitizeTransform.Sink.Named name
  | _ -> failwith "Unsupported sink sanitizer"


let from_sanitized_sink (SanitizeTransform.Sink.Named name) = NamedSink name

let extract_sanitize_transforms = function
  | Transform { local; global; _ } ->
      TaintTransforms.merge ~local ~global |> TaintTransforms.get_sanitize_transforms
  | _ -> SanitizeTransformSet.empty


let rec extract_partial_sink = function
  | Transform { base; _ } -> extract_partial_sink base
  | PartialSink { kind; label } -> Some { kind; label }
  | _ -> None


let get_named_transforms = function
  | Transform { local; global; _ } ->
      TaintTransforms.merge ~local ~global |> TaintTransforms.get_named_transforms
  | _ -> []


let contains_sanitize_transforms sink sanitize_transforms =
  SanitizeTransformSet.less_or_equal
    ~left:sanitize_transforms
    ~right:(extract_sanitize_transforms sink)
