(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Analysis

type error_reason =
  | StubShadowing
  | FileNotFound

type types_by_path = {
  path: PyrePath.t;
  types_by_location: ((Location.t * Type.t) list, error_reason) Core.Result.t;
}

type qualified_names_by_path = {
  path: PyrePath.t;
  qualified_names_by_location: (Location.t * Reference.t) list option;
  error_reason: error_reason option;
}

val find_annotation
  :  state:State.t ->
  configuration:Configuration.Analysis.t ->
  path:PyrePath.t ->
  position:Location.position ->
  (Location.t * Type.t) option

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
  Location.t option
