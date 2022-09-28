(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Sanitize: defines the representation of sanitizers in models, as an abstract
 * domain. *)

open Core

type sanitize = {
  sources: SanitizeTransform.SourceSet.t;
  sinks: SanitizeTransform.SinkSet.t;
  tito: SanitizeTransformSet.t;
}
[@@deriving show, eq]

module T = struct
  include Abstract.SimpleDomain.Make (struct
    type t = sanitize

    let name = "sanitize"

    let bottom =
      {
        sources = SanitizeTransform.SourceSet.bottom;
        sinks = SanitizeTransform.SinkSet.bottom;
        tito = SanitizeTransformSet.bottom;
      }


    let less_or_equal ~left ~right =
      if phys_equal left right then
        true
      else
        SanitizeTransform.SourceSet.less_or_equal ~left:left.sources ~right:right.sources
        && SanitizeTransform.SinkSet.less_or_equal ~left:left.sinks ~right:right.sinks
        && SanitizeTransformSet.less_or_equal ~left:left.tito ~right:right.tito


    let join left right =
      if phys_equal left right then
        left
      else
        let sources = SanitizeTransform.SourceSet.join left.sources right.sources in
        let sinks = SanitizeTransform.SinkSet.join left.sinks right.sinks in
        let tito = SanitizeTransformSet.join left.tito right.tito in
        { sources; sinks; tito }


    let meet a b = if less_or_equal ~left:b ~right:a then b else a

    let show = show_sanitize
  end)

  let equal = equal_sanitize

  let all =
    {
      sources = SanitizeTransform.SourceSet.all;
      sinks = SanitizeTransform.SinkSet.all;
      tito = SanitizeTransformSet.all;
    }


  let is_all { sources; sinks; tito } =
    SanitizeTransform.SourceSet.is_all sources
    && SanitizeTransform.SinkSet.is_all sinks
    && SanitizeTransformSet.is_all tito


  let from_sources_only sources =
    { sources; sinks = SanitizeTransform.SinkSet.empty; tito = SanitizeTransformSet.empty }


  let from_sinks_only sinks =
    { sources = SanitizeTransform.SourceSet.empty; sinks; tito = SanitizeTransformSet.empty }


  let from_tito_only tito =
    { sources = SanitizeTransform.SourceSet.empty; sinks = SanitizeTransform.SinkSet.empty; tito }


  let empty = bottom

  let is_empty = is_bottom

  let to_json
      {
        sources;
        sinks;
        tito = { SanitizeTransformSet.sources = tito_sources; sinks = tito_sinks } as tito;
      }
    =
    let sources_json =
      match SanitizeTransform.SourceSet.to_json sources with
      | None -> []
      | Some sources_json -> ["sources", sources_json]
    in
    let sinks_json =
      match SanitizeTransform.SinkSet.to_json sinks with
      | None -> []
      | Some sinks_json -> ["sinks", sinks_json]
    in
    let tito_json =
      if SanitizeTransformSet.is_empty tito then
        []
      else
        let tito_sources =
          match SanitizeTransform.SourceSet.to_json tito_sources with
          | None -> []
          | Some tito_sources -> ["sources", tito_sources]
        in
        let tito_sinks =
          match SanitizeTransform.SinkSet.to_json tito_sinks with
          | None -> []
          | Some tito_sinks -> ["sinks", tito_sinks]
        in
        ["tito", `Assoc (tito_sources @ tito_sinks)]
    in
    `Assoc (sources_json @ sinks_json @ tito_json)
end

include T

(** Map from parameters or return value to a sanitizer. *)
module RootMap = struct
  include
    Abstract.MapDomain.Make
      (struct
        let name = "sanitize"

        include AccessPath.Root

        let absence_implicitly_maps_to_bottom = true
      end)
      (T)

  let roots map = fold Key ~f:List.cons ~init:[] map

  let to_json map =
    map
    |> to_alist
    |> List.map ~f:(fun (root, sanitize) ->
           let (`Assoc fields) = T.to_json sanitize in
           let port = AccessPath.create root [] |> AccessPath.to_json in
           `Assoc (("port", port) :: fields))
    |> fun elements -> `List elements
end
