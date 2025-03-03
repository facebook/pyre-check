(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Analysis = struct
  let perf_data_file_name = "taint-analysis"

  module ApplyCallStep = struct
    type t =
      | ApplyCallForArgumentSinks
      | ApplyCallForArgumentSources
      | ApplyCallForReturn
      | ApplyCallEffects
      | CheckIssuesForArgument
      | BuildTaintInTaintOutMapping
      | ApplyTitoForArgument
    [@@deriving sexp, compare]

    let pp_short formatter = function
      | ApplyCallForArgumentSinks -> Format.fprintf formatter "apply-call-for-arg-sinks"
      | ApplyCallForArgumentSources -> Format.fprintf formatter "apply-call-for-arg-sources"
      | ApplyCallForReturn -> Format.fprintf formatter "apply-call-for-return"
      | ApplyCallEffects -> Format.fprintf formatter "apply-call-effects"
      | CheckIssuesForArgument -> Format.fprintf formatter "check-issues-for-arg"
      | BuildTaintInTaintOutMapping -> Format.fprintf formatter "build-tito-map"
      | ApplyTitoForArgument -> Format.fprintf formatter "apply-tito-for-arg"
  end
end

include Interprocedural.IntraproceduralProfiler.Make (Analysis)
