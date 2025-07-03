(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Analysis
open Ast
open Expression
open Pyre

let recognized_callable_target_types = Type.Set.of_list [Type.Primitive "TestCallableTarget"]

let shim_calls ~resolve_expression_to_type { Call.callee; arguments; origin = _ } =
  let open Shims.ShimArgumentMapping in
  let is_async_task base =
    resolve_expression_to_type base |> Set.mem recognized_callable_target_types
  in
  match Node.value callee with
  | Name (Name.Attribute { base; attribute = ("async_delay" | "async_schedule") as attribute; _ })
    when is_async_task base ->
      Some
        {
          identifier = "async_task";
          callee = Target.GetAttributeBase { inner = Target.Callee; attribute };
          arguments =
            List.mapi arguments ~f:(fun index { Call.Argument.name; _ } ->
                { Argument.name = name >>| Node.value; value = Target.Argument { index } });
        }
  | _ -> None
