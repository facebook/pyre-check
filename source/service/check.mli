(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Analysis

val check
  :  scheduler:Scheduler.t ->
  configuration:Configuration.Analysis.t ->
  populate_call_graph:bool ->
  ErrorsEnvironment.t
