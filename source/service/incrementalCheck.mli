(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type errors = Analysis.AnalysisError.t list [@@deriving show]

(* Given a list of changed files, perform incremental type check and update the type environment
   along with the error table. This is a stateful API: both `environment` and `errors` get mutated
   after this function is invoked. *)
(* Note that the return value is a list of rechecked modules and the type errors contained within
   those modules only. It does NOT return a list of all type errors after the recheck. If one wants
   all errors instead, query the `errors` table. *)
val recheck
  :  configuration:Configuration.Analysis.t ->
  scheduler:Scheduler.t ->
  environment:Analysis.TypeEnvironment.t ->
  errors:errors Ast.Reference.Table.t ->
  PyrePath.Built.t list ->
  Ast.Reference.t list * errors
