(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast

module Environment = AnalysisEnvironment
module Type = AnalysisType


type t

val create_of_source: (module Environment.Handler) -> Source.t -> t

val get_annotation:
  t ->
  position: Location.position ->
  source_text: string ->
  (Location.Instantiated.t * Type.t) option

val get_all_annotations: t -> (Location.t * Type.t) list

val get_definition: t -> position: Location.position -> Location.Instantiated.t option

val get_all_definitions: t -> (Location.t * Location.t) list
