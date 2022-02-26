(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Analysis
open Ast
open Expression

let recognized_callable_target_types = Type.Set.of_list [Type.Primitive "TestCallableTarget"]

let redirect ~resolution { Call.callee; arguments } =
  match Node.value callee with
  | Name (Name.Attribute { base; attribute = "async_delay" | "async_schedule"; _ }) ->
      let is_async_task =
        Resolution.resolve_expression_to_type resolution base
        |> fun annotation -> Set.exists recognized_callable_target_types ~f:(Type.equal annotation)
      in
      if is_async_task then
        Some { Call.callee = base; arguments }
      else
        None
  | _ -> None
