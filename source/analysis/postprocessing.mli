(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
module Error = AnalysisError

val run_on_qualifier
  :  TypeEnvironment.ReadOnly.t ->
  dependency:SharedMemoryKeys.DependencyKey.registered option ->
  Reference.t ->
  Error.t list

val run
  :  scheduler:Scheduler.t ->
  environment:TypeEnvironment.ReadOnly.t ->
  Reference.t list ->
  Error.t list
