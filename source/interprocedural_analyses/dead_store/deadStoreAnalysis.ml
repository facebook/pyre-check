(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Interprocedural

(* Registers the liveness analysis with the interprocedural analysis framework. *)
include DeadStoreResult.Register (struct
  let init ~static_analysis_configuration:_ ~scheduler:_ ~environment:_ ~functions:_ ~stubs:_ =
    { Result.initial_models = Callable.Map.empty; skip_overrides = Ast.Reference.Set.empty }


  let analyze ~callable:_ ~environment:_ ~qualifier:_ ~define:_ ~existing:_ = "A", 5

  let report
      ~scheduler:_
      ~static_analysis_configuration:_
      ~filename_lookup:_
      ~callables:_
      ~skipped_overrides:_
      ~fixpoint_timer:_
      ~fixpoint_iterations:_
    =
    []
end)
