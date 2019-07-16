(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Analysis

val register_ignores
  :  configuration:Configuration.Analysis.t ->
  Scheduler.t ->
  Ast.SourcePath.t list ->
  unit

val ignore
  :  configuration:Configuration.Analysis.t ->
  Scheduler.t ->
  Ast.SourcePath.t list ->
  Error.t list ->
  Error.t list
