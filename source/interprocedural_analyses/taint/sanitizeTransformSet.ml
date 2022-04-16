(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type t = {
  sources: SanitizeTransform.SourceSet.t;
  sinks: SanitizeTransform.SinkSet.t;
}
[@@deriving compare, eq, hash, sexp]

let empty = { sources = SanitizeTransform.SourceSet.empty; sinks = SanitizeTransform.SinkSet.empty }

let is_empty { sources; sinks } =
  SanitizeTransform.SourceSet.is_empty sources && SanitizeTransform.SinkSet.is_empty sinks


let pp formatter { sources; sinks } =
  let sources =
    SanitizeTransform.SourceSet.elements sources |> List.map ~f:SanitizeTransform.Source.show
  in
  let sinks = SanitizeTransform.SinkSet.elements sinks |> List.map ~f:SanitizeTransform.Sink.show in
  sources @ sinks |> String.concat ~sep:":" |> Format.fprintf formatter "%s"


let show = Format.asprintf "%a" pp

let from_sources sources = { sources; sinks = SanitizeTransform.SinkSet.empty }

let from_sinks sinks = { sources = SanitizeTransform.SourceSet.empty; sinks }

let union
    { sources = sources_left; sinks = sinks_left }
    { sources = sources_right; sinks = sinks_right }
  =
  {
    sources = SanitizeTransform.SourceSet.union sources_left sources_right;
    sinks = SanitizeTransform.SinkSet.union sinks_left sinks_right;
  }


let diff
    { sources = sources_left; sinks = sinks_left }
    { sources = sources_right; sinks = sinks_right }
  =
  {
    sources = SanitizeTransform.SourceSet.diff sources_left sources_right;
    sinks = SanitizeTransform.SinkSet.diff sinks_left sinks_right;
  }


let subset
    { sources = sources_left; sinks = sinks_left }
    { sources = sources_right; sinks = sinks_right }
  =
  SanitizeTransform.SourceSet.subset sources_left sources_right
  && SanitizeTransform.SinkSet.subset sinks_left sinks_right


let mem { sources; sinks } = function
  | SanitizeTransform.Source source -> SanitizeTransform.SourceSet.mem source sources
  | SanitizeTransform.Sink sink -> SanitizeTransform.SinkSet.mem sink sinks
