(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val infer
  :  pyre_api:Analysis.PyrePysaEnvironment.ReadOnly.t ->
  user_models:SharedModels.ReadOnly.t ->
  Registry.t
