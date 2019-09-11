(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Analysis

val populate
  :  Environment.t ->
  UnannotatedGlobalEnvironment.ReadOnly.t ->
  configuration:Configuration.Analysis.t ->
  scheduler:Scheduler.t ->
  update_result:AliasEnvironment.UpdateResult.t ->
  Source.t list ->
  unit
