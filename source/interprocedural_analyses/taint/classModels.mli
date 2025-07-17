(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val infer
  :  scheduler:Scheduler.t ->
  scheduler_policies:Configuration.SchedulerPolicies.t ->
  pyre_api:Interprocedural.PyrePysaApi.ReadOnly.t ->
  user_models:SharedModels.ReadOnly.t ->
  Registry.t
