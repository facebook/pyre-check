(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast

module Environment = AnalysisEnvironment
module Type = AnalysisType


type t = Type.t Location.Table.t

val create_of_source:
  (module Environment.Handler) ->
  Source.t ->
  Type.t Location.Table.t

val get_annotation: t -> position: Location.position -> (Location.t * Type.t) option

val get_definition: t -> Location.position -> Location.t option
