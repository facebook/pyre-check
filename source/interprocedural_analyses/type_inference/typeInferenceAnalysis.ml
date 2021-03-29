(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Interprocedural

include TypeInferenceResult.Register (struct
  let init ~configuration:_ ~scheduler:_ ~environment:_ ~functions:_ ~stubs:_ =
    { Result.initial_models = Callable.Map.empty; skip_overrides = Ast.Reference.Set.empty }


  let analyze ~callable:_ ~environment:_ ~qualifier:_ ~define:_ ~existing =
    let new_model = Option.value existing ~default:TypeInferenceDomain.bottom in
    ["placeholder_result"], new_model
end)
