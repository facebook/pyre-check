(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val run_taint_analysis
  :  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  build_system:Server.BuildSystem.t ->
  scheduler:Scheduler.t ->
  unit ->
  unit
