(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Interprocedural.AnalysisResult.Make (struct
  let name = "taint"

  type result = Issue.t list

  type call_model = Model.t

  let show_call_model = Model.show

  let obscure_model = Model.obscure_model

  let empty_model = Model.empty_model

  let pp_call_model = Model.pp

  let join ~iteration:_ left right = Model.join left right

  let widen = Model.widen

  let reached_fixpoint = Model.reached_fixpoint

  let strip_for_callsite = Model.strip_for_callsite
end)
