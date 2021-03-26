(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Interprocedural

include TypeInferenceResult.Register (struct
  let init ~configuration:_ ~scheduler:_ ~environment:_ ~functions:_ ~stubs:_ =
    { Result.initial_models = Callable.Map.empty; skip_overrides = Ast.Reference.Set.empty }


  let analyze ~callable:_ ~environment:_ ~qualifier:_ ~define:_ ~existing:_ =
    ["placeholder_result"], "placeholder_model"
end)
