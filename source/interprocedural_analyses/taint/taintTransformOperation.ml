(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module implements common operations on taint transforms.
 * Most importantly, it implements  adding transforms on a source or sink.
 *
 * This is intentionally in a separate file to have access to both `Sources` and
 * `Sinks`, even though they use `TaintTransforms.t`. This avoids a cyclic
 * dependency.
 *)

open Core
open Pyre

module InsertLocation = struct
  type t =
    | Front
    | Back
  [@@deriving show]
end

let add_named_transform transforms named_transform = named_transform :: transforms

type 'a kind_transforms =
  | NonTransformable of 'a
  | Base of 'a
  | Transformed of {
      local: TaintTransforms.t;
      global: TaintTransforms.t;
      base: 'a;
    }

module type KIND_ARG = sig
  type t

  val deconstruct : t -> t kind_transforms

  val order : TaintTransforms.Order.t

  val preserve_sanitize_sources : t -> bool

  val preserve_sanitize_sinks : t -> bool

  val base_as_sanitizer : t -> SanitizeTransform.t option

  val make_transform : local:TaintTransforms.t -> global:TaintTransforms.t -> base:t -> t
end

module Make (Kind : KIND_ARG) = struct
  let preserve_sanitizers ~base sanitizers =
    let sanitizers =
      if not (Kind.preserve_sanitize_sources base) then
        { sanitizers with SanitizeTransformSet.sources = SanitizeTransform.SourceSet.empty }
      else
        sanitizers
    in
    let sanitizers =
      if not (Kind.preserve_sanitize_sinks base) then
        { sanitizers with SanitizeTransformSet.sinks = SanitizeTransform.SinkSet.empty }
      else
        sanitizers
    in
    sanitizers


  let prepend_sanitize_transforms
      ~base
      ~has_name_transform
      ~global_sanitizers
      transforms
      ({ SanitizeTransformSet.sources = sanitizer_sources; sinks = sanitizer_sinks } as sanitizers)
    =
    match Kind.base_as_sanitizer base with
    | _ when SanitizeTransform.SourceSet.is_all sanitizer_sources -> None
    | _ when SanitizeTransform.SinkSet.is_all sanitizer_sinks -> None
    | Some base when (not has_name_transform) && SanitizeTransformSet.mem sanitizers base ->
        (* Taint is sanitized and should be removed. *)
        None
    | _ ->
        let sanitizers = preserve_sanitizers ~base sanitizers in
        let sanitizers = SanitizeTransformSet.diff sanitizers global_sanitizers in
        if SanitizeTransformSet.is_empty sanitizers then
          Some transforms
        else
          let existing_sanitizers, rest = TaintTransforms.split_sanitizers transforms in
          let sanitizers = SanitizeTransformSet.join sanitizers existing_sanitizers in
          Some (TaintTransform.Sanitize sanitizers :: rest)


  let add_sanitize_transforms_internal
      ~base
      ~has_name_transform
      ~global_sanitizers
      ~insert_location
      transforms
      sanitizers
    =
    match insert_location with
    | InsertLocation.Front ->
        prepend_sanitize_transforms
          ~base
          ~has_name_transform
          ~global_sanitizers
          transforms
          sanitizers
    | InsertLocation.Back ->
        if SanitizeTransformSet.is_empty global_sanitizers then
          match
            prepend_sanitize_transforms
              ~base
              ~has_name_transform
              ~global_sanitizers
              (List.rev transforms)
              sanitizers
          with
          | None -> None
          | Some list -> Some (List.rev list)
        else
          failwith "Unable to insert sanitizers in the back when there exist global sanitizers"


  let get_global_sanitizers ~local ~global =
    if List.exists local ~f:TaintTransform.is_named_transform then
      SanitizeTransformSet.empty
    else
      fst (TaintTransforms.split_sanitizers global)


  let has_name_transform ~local ~global =
    List.exists local ~f:TaintTransform.is_named_transform
    || List.exists global ~f:TaintTransform.is_named_transform


  let add_sanitize_transforms ~base ~local ~global ~insert_location sanitizers =
    let global_sanitizers = get_global_sanitizers ~local ~global in
    let has_name_transform = has_name_transform ~local ~global in
    add_sanitize_transforms_internal
      ~base
      ~has_name_transform
      ~global_sanitizers
      ~insert_location
      local
      sanitizers


  type add_transform_result = {
    (* None represents a sanitized taint. *)
    transforms: TaintTransform.t list option;
    has_name_transform: bool;
    global_sanitizers: SanitizeTransformSet.t;
  }

  let add_transform ~base ~has_name_transform ~global_sanitizers ~insert_location transforms
    = function
    | TaintTransform.Named _ as named_transform ->
        {
          transforms = Some (add_named_transform transforms named_transform);
          has_name_transform = true;
          global_sanitizers = SanitizeTransformSet.empty;
        }
    | TaintTransform.Sanitize sanitizers ->
        let transforms =
          add_sanitize_transforms_internal
            ~base
            ~has_name_transform
            ~global_sanitizers
            ~insert_location
            transforms
            sanitizers
        in
        { transforms; has_name_transform; global_sanitizers }


  let add_backward_into_forward_transforms ~base ~local ~global ~insert_location ~to_add =
    let rec add ({ transforms; has_name_transform; global_sanitizers } as sofar) to_add =
      match transforms, to_add with
      | None, _
      | Some _, [] ->
          sofar
      | Some transforms, head :: tail ->
          add
            (add_transform
               ~base
               ~has_name_transform
               ~global_sanitizers
               ~insert_location
               transforms
               head)
            tail
    in
    let global_sanitizers = get_global_sanitizers ~local ~global in
    let has_name_transform = has_name_transform ~local ~global in
    let { transforms; _ } =
      add { transforms = Some local; has_name_transform; global_sanitizers } to_add
    in
    transforms


  let add_backward_into_backward_transforms ~base ~local ~global ~insert_location ~to_add =
    let rec add sofar = function
      | [] -> sofar
      | head :: tail -> (
          match add sofar tail with
          | { transforms = None; _ } as sofar -> sofar
          | { transforms = Some transforms; has_name_transform; global_sanitizers } ->
              add_transform
                ~base
                ~has_name_transform
                ~global_sanitizers
                ~insert_location
                transforms
                head)
    in
    let global_sanitizers = get_global_sanitizers ~local ~global in
    let has_name_transform = has_name_transform ~local ~global in
    let { transforms; _ } =
      add { transforms = Some local; has_name_transform; global_sanitizers } to_add
    in
    transforms


  let add_transforms ~base ~local ~global ~order ~insert_location ~to_add ~to_add_order =
    match order, to_add_order with
    | TaintTransforms.Order.Forward, TaintTransforms.Order.Backward ->
        add_backward_into_forward_transforms ~base ~local ~global ~insert_location ~to_add
    | TaintTransforms.Order.Backward, TaintTransforms.Order.Backward ->
        add_backward_into_backward_transforms ~base ~local ~global ~insert_location ~to_add
    | _ ->
        Format.asprintf
          "unsupported: add_transforms ~order:%a ~to_add_order:%a"
          TaintTransforms.Order.pp
          order
          TaintTransforms.Order.pp
          to_add_order
        |> failwith


  let of_sanitize_transforms ~base sanitizers =
    prepend_sanitize_transforms
      ~base
      ~has_name_transform:false
      ~global_sanitizers:SanitizeTransformSet.empty
      []
      sanitizers


  let apply_sanitize_transforms transforms insert_location kind =
    match Kind.deconstruct kind with
    | NonTransformable kind -> Some kind
    | Base base ->
        of_sanitize_transforms ~base transforms
        >>| fun local -> Kind.make_transform ~local ~global:TaintTransforms.empty ~base
    | Transformed { local; global; base } ->
        add_sanitize_transforms ~base ~local ~global ~insert_location transforms
        >>| fun local -> Kind.make_transform ~local ~global ~base


  let apply_transforms transforms insert_location order kind =
    match Kind.deconstruct kind with
    | NonTransformable kind -> Some kind
    | Base base ->
        add_transforms
          ~base
          ~local:TaintTransforms.empty
          ~global:TaintTransforms.empty
          ~order:Kind.order
          ~insert_location
          ~to_add:transforms
          ~to_add_order:order
        >>| fun local -> Kind.make_transform ~local ~global:TaintTransforms.empty ~base
    | Transformed { local; global; base } ->
        add_transforms
          ~base
          ~local
          ~global
          ~order:Kind.order
          ~insert_location
          ~to_add:transforms
          ~to_add_order:order
        >>| fun local -> Kind.make_transform ~local ~global ~base
end

module Source = Make (struct
  type t = Sources.t

  let deconstruct source =
    match source with
    | Sources.Attach -> NonTransformable source
    | Sources.NamedSource _
    | Sources.ParametricSource _ ->
        Base source
    | Sources.Transform { local; global; base } -> Transformed { local; global; base }


  let order = TaintTransforms.Order.Forward

  let preserve_sanitize_sources _ = false

  let preserve_sanitize_sinks _ = true

  let rec base_as_sanitizer = function
    | Sources.NamedSource name
    | Sources.ParametricSource { source_name = name; _ } ->
        Some (SanitizeTransform.Source (SanitizeTransform.Source.Named name))
    | Sources.Transform { base; _ } -> base_as_sanitizer base
    | Sources.Attach -> None


  let make_transform ~local ~global ~base =
    match local, global with
    | [], [] -> base
    | _ -> Sources.Transform { local; global; base }
end)

module Sink = Make (struct
  type t = Sinks.t

  let deconstruct sink =
    match sink with
    | Sinks.Attach
    | Sinks.AddFeatureToArgument ->
        NonTransformable sink
    | Sinks.PartialSink _
    | Sinks.TriggeredPartialSink _
    | Sinks.LocalReturn
    | Sinks.NamedSink _
    | Sinks.ParametricSink _
    | Sinks.ParameterUpdate _ ->
        Base sink
    | Sinks.Transform { local; global; base } -> Transformed { local; global; base }


  let order = TaintTransforms.Order.Backward

  let preserve_sanitize_sources _ = true

  (* We should only apply sink sanitizers on tito. *)
  let rec preserve_sanitize_sinks = function
    | Sinks.LocalReturn
    | Sinks.ParameterUpdate _ ->
        true
    | Sinks.Transform { base; _ } -> preserve_sanitize_sinks base
    | _ -> false


  let rec base_as_sanitizer = function
    | Sinks.NamedSink name
    | Sinks.ParametricSink { sink_name = name; _ } ->
        Some (SanitizeTransform.Sink (SanitizeTransform.Sink.Named name))
    | Sinks.Transform { base; _ } -> base_as_sanitizer base
    | Sinks.Attach
    | Sinks.AddFeatureToArgument
    | Sinks.PartialSink _
    | Sinks.TriggeredPartialSink _
    | Sinks.LocalReturn
    | Sinks.ParameterUpdate _ ->
        None


  let make_transform ~local ~global ~base =
    match local, global with
    | [], [] -> base
    | _ -> Sinks.Transform { local; global; base }
end)
