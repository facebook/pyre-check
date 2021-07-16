(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ResultArgument = struct
  type result = TypeInferenceData.LocalResult.t

  type call_model = TypeInferenceDomain.t [@@deriving show]

  let name = "type_inference"

  let empty_model = TypeInferenceDomain.bottom

  let obscure_model = TypeInferenceDomain.bottom

  let join ~iteration:_ = TypeInferenceDomain.join

  let widen ~iteration ~previous ~next = join ~iteration previous next

  let reached_fixpoint ~iteration:_ ~previous ~next =
    TypeInferenceDomain.less_or_equal ~left:next ~right:previous


  let strip_for_callsite model = model
end

include ResultArgument
include Interprocedural.AnalysisResult.Make (ResultArgument)
