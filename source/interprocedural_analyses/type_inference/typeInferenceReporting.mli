(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module TypeEnvironment = Analysis.TypeEnvironment

val report
  :  scheduler:'a ->
  static_analysis_configuration:Configuration.StaticAnalysis.t ->
  environment:TypeEnvironment.ReadOnly.t ->
  filename_lookup:'b ->
  callables:Interprocedural.Target.Set.t ->
  skipped_overrides:'c ->
  fixpoint_timer:Timer.t ->
  fixpoint_iterations:int option ->
  Yojson.Safe.t list
