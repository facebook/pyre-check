(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Interprocedural

(* Registers the liveness analysis with the interprocedural analysis framework. *)
include DeadStoreResult.Register (struct
  let init ~configuration:_ ~environment:_ ~functions:_ = Callable.Map.empty

  let analyze ~callable:_ ~environment:_ ~define:_ ~existing:_ = "A", 5
end)
