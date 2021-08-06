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
  path: string;
  types_by_location: ((Location.t * Type.t) list, error_reason) Core.Result.t;
}

(* TODO (T82533515): All the `TypeEnvironment.t` arguments should really be typed as
   `TypeEnvironment.ReadOnly.t`. But we are prevented from doing it at the moment due to the lack of
   a read-only variant of `ModuleTracker.t`. *)

val find_all_annotations_batch
  :  environment:TypeEnvironment.t ->
  build_system:BuildSystem.t ->
  configuration:Configuration.Analysis.t ->
  string list ->
  types_by_path list
