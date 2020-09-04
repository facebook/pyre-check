(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Domains

module Backward : sig
  type model = {
    taint_in_taint_out: BackwardState.t;
    sink_taint: BackwardState.t;
  }
  [@@deriving show]

  val empty : model
end

module Forward : sig
  type model = { source_taint: ForwardState.t } [@@deriving show]

  val empty : model
end

type sanitize_kind =
  | SanitizeSources
  | SanitizeSinks
  | SanitizeTITO
  | SanitizeAll
[@@deriving show, compare]

type mode =
  | SkipAnalysis (* Don't analyze at all *)
  | Sanitize of sanitize_kind list (* Analyze, but throw away inferred model *)
  | Normal
[@@deriving show]

type call_model = {
  forward: Forward.model;
  backward: Backward.model;
  mode: mode;
}

val empty_skip_model : call_model (* Skips analysis *)

type result = Flow.issue list

include
  Interprocedural.Result.ANALYSIS_RESULT_WITH_REGISTRATION
    with type result := result
     and type call_model := call_model
