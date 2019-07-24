(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Analysis

val populate
  :  Environment.t ->
  configuration:Configuration.Analysis.t ->
  scheduler:Scheduler.t ->
  Source.t list ->
  unit

val shared_handler : Environment.t

val populate_shared_memory
  :  configuration:Configuration.Analysis.t ->
  scheduler:Scheduler.t ->
  Reference.t list ->
  unit
