(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

module Callable = Interprocedural.Callable


(* Registers the Taint analysis with the interprocedural analysis framework. *)
include TaintResult.Register(struct
    include TaintResult

    let init ~types:_ ~functions:_ = ()

    let analyze ~callable ~environment ~define ~existing =
      match existing with
      | Some model when model.skip_analysis ->
          let () = Log.info "Skipping taint analysis of %a" Callable.pretty_print callable in
          [], model
      | _ ->
          let forward, result = ForwardAnalysis.run ~environment ~define in
          let backward = BackwardAnalysis.run ~environment ~define in
          let model = { forward; backward; skip_analysis = false; } in
          result, model
  end)
