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

(* TODO (T82533515): All the `TypeEnvironment.t` arguments should really be typed as
   `TypeEnvironment.ReadOnly.t`. But we are prevented from doing it at the moment due to the lack of
   a read-only variant of `ModuleTracker.t`. *)

val find_annotation
  :  environment:TypeEnvironment.t ->
  configuration:Configuration.Analysis.t ->
  path:PyrePath.t ->
  position:Location.position ->
  (Location.t * Type.t) option

val find_all_annotations_batch
  :  environment:TypeEnvironment.t ->
  configuration:Configuration.Analysis.t ->
  paths:PyrePath.t list ->
  types_by_path list

val find_definition
  :  environment:TypeEnvironment.t ->
  configuration:Configuration.Analysis.t ->
  PyrePath.t ->
  Location.position ->
  Location.t option
