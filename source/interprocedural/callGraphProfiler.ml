(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Analysis = struct
  let perf_data_file_name = "call-graph"

  module ApplyCallStep = struct
    type t =
      | AnalyzeArguments
      | FetchReturnedCallables
      | CreateParameterizedTargets
    [@@deriving sexp, compare]

    let pp_short formatter = function
      | AnalyzeArguments -> Format.fprintf formatter "analyze-arguments"
      | FetchReturnedCallables -> Format.fprintf formatter "fetch-returned-callables"
      | CreateParameterizedTargets -> Format.fprintf formatter "create-parameterized-targets"
  end
end

include IntraproceduralProfiler.Make (Analysis)
