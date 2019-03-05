(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

module Callable = Interprocedural.Callable


(* Registers the Taint analysis with the interprocedural analysis framework. *)
include TaintResult.Register(struct
    include TaintResult

    let init ~types:_ ~functions:_ = ()

    let analyze ~callable:_ ~environment ~define ~mode =
      let forward, result = ForwardAnalysis.run ~environment ~define in
      let backward = BackwardAnalysis.run ~environment ~define in
      let model =
        if mode = Normal then
          { forward; backward; mode; }
        else
          { empty_model with mode }
      in
      result, model

    let analyze ~callable ~environment ~define ~existing =
      match existing with
      | Some ({ mode = SkipAnalysis; _ } as model) ->
          let () = Log.info "Skipping taint analysis of %a" Callable.pretty_print callable in
          [], model
      | Some { mode; _ } ->
          analyze ~callable ~environment ~define ~mode
      | None ->
          analyze ~callable ~environment ~define ~mode:Normal

  end)
