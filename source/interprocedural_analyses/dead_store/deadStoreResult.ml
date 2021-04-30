(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module ResultArgument = struct
  type result = string

  type call_model = int [@@deriving show]

  let name = "liveness"

  let empty_model = 0

  let obscure_model = -1

  let join ~iteration:_ a b = a + b

  let widen ~iteration ~previous ~next = join ~iteration previous next

  let reached_fixpoint ~iteration:_ ~previous ~next = next <= previous

  let strip_for_callsite model = model
end

include Interprocedural.Result.Make (ResultArgument)
