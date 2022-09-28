(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* SanitizeTransformSet: defines a set of sanitize transforms. *)

open Core

type sets = {
  sources: SanitizeTransform.SourceSet.t;
  sinks: SanitizeTransform.SinkSet.t;
}
[@@deriving compare, eq, hash, sexp]

let pp_sets formatter { sources; sinks } =
  let is_empty_sources = SanitizeTransform.SourceSet.is_empty sources in
  let is_empty_sinks = SanitizeTransform.SinkSet.is_empty sinks in
  if is_empty_sources then
    if is_empty_sinks then
      Format.fprintf formatter ""
    else
      SanitizeTransform.SinkSet.pp formatter sinks
  else if is_empty_sinks then
    SanitizeTransform.SourceSet.pp formatter sources
  else
    Format.fprintf
      formatter
      "%a:%a"
      SanitizeTransform.SourceSet.pp
      sources
      SanitizeTransform.SinkSet.pp
      sinks


let show_sets = Format.asprintf "%a" pp_sets

include Abstract.SimpleDomain.Make (struct
  type t = sets

  let show = show_sets

  let name = "sanitize tito"

  let bottom =
    { sources = SanitizeTransform.SourceSet.bottom; sinks = SanitizeTransform.SinkSet.bottom }


  let less_or_equal
      ~left:({ sources = left_sources; sinks = left_sinks } as left)
      ~right:({ sources = right_sources; sinks = right_sinks } as right)
    =
    if phys_equal left right then
      true
    else
      SanitizeTransform.SourceSet.less_or_equal ~left:left_sources ~right:right_sources
      && SanitizeTransform.SinkSet.less_or_equal ~left:left_sinks ~right:right_sinks


  let join
      ({ sources = left_sources; sinks = left_sinks } as left)
      ({ sources = right_sources; sinks = right_sinks } as right)
    =
    if phys_equal left right then
      left
    else
      {
        sources = SanitizeTransform.SourceSet.join left_sources right_sources;
        sinks = SanitizeTransform.SinkSet.join left_sinks right_sinks;
      }


  let meet
      ({ sources = left_sources; sinks = left_sinks } as left)
      ({ sources = right_sources; sinks = right_sinks } as right)
    =
    if phys_equal left right then
      left
    else
      {
        sources = SanitizeTransform.SourceSet.meet left_sources right_sources;
        sinks = SanitizeTransform.SinkSet.meet left_sinks right_sinks;
      }
end)

type t = sets [@@deriving compare, eq, hash, sexp]

let empty = bottom

let is_empty = is_bottom

let add_source source { sources; sinks } =
  { sources = SanitizeTransform.SourceSet.add source sources; sinks }


let add_sink sink { sources; sinks } = { sinks = SanitizeTransform.SinkSet.add sink sinks; sources }

let from_sources sources = { sources; sinks = SanitizeTransform.SinkSet.empty }

let from_sinks sinks = { sources = SanitizeTransform.SourceSet.empty; sinks }

let diff
    { sources = sources_left; sinks = sinks_left }
    { sources = sources_right; sinks = sinks_right }
  =
  {
    sources = SanitizeTransform.SourceSet.diff sources_left sources_right;
    sinks = SanitizeTransform.SinkSet.diff sinks_left sinks_right;
  }


let mem { sources; sinks } = function
  | SanitizeTransform.Source source -> SanitizeTransform.SourceSet.mem source sources
  | SanitizeTransform.Sink sink -> SanitizeTransform.SinkSet.mem sink sinks


let all = { sources = SanitizeTransform.SourceSet.all; sinks = SanitizeTransform.SinkSet.all }

let is_all { sources; sinks } =
  SanitizeTransform.SourceSet.is_all sources && SanitizeTransform.SinkSet.is_all sinks
