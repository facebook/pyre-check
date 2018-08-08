(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


(* Registers the Taint analysis with the interprocedural analysis framework. *)
include TaintResult.Register(struct
    include TaintResult

    let init ~types ~functions = ()

    let analyze callable define =
      let forward, result = TaintForwardAnalysis.run define in
      let backward = TaintBackwardAnalysis.run define in
      let model = { forward; backward; } in
      result, model
  end)
