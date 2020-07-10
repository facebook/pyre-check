(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

type errors = Analysis.AnalysisError.t list [@@deriving show]

val recheck_with_state
  :  state:State.t ->
  configuration:Configuration.Analysis.t ->
  PyrePath.t list ->
  errors
