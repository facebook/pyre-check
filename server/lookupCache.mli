(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Analysis

type types_by_path = {
  path: PyrePath.t;
  types_by_location: (Location.Instantiated.t * Type.t) list option;
}

val evict : state:State.t -> Reference.t -> unit

val evict_path : state:State.t -> configuration:Configuration.Analysis.t -> PyrePath.t -> unit

val find_annotation
  :  state:State.t ->
  configuration:Configuration.Analysis.t ->
  path:PyrePath.t ->
  position:Location.position ->
  (Location.Instantiated.t * Type.t) option

val find_all_annotations
  :  state:State.t ->
  configuration:Configuration.Analysis.t ->
  path:PyrePath.t ->
  (Location.Instantiated.t * Type.t) list option

val find_all_annotations_batch
  :  state:State.t ->
  configuration:Configuration.Analysis.t ->
  paths:PyrePath.t list ->
  types_by_path list

val find_definition
  :  state:State.t ->
  configuration:Configuration.Analysis.t ->
  PyrePath.t ->
  Location.position ->
  Location.Reference.t option
