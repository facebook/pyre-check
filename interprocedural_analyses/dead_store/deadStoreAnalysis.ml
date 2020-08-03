(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Interprocedural

(* Registers the liveness analysis with the interprocedural analysis framework. *)
include DeadStoreResult.Register (struct
  let init ~configuration:_ ~environment:_ ~functions:_ ~stubs:_ =
    { Result.initial_models = Callable.Map.empty; skip_overrides = Ast.Reference.Set.empty }


  let analyze ~callable:_ ~environment:_ ~qualifier:_ ~define:_ ~existing:_ = "A", 5
end)
