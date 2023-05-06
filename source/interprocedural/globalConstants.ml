(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Statement
open Expression

type t = { global_constants: Expression.t Ast.Reference.Map.t }

let from_source source =
  let extract_string = function
    | {
        Node.value =
          {
            Assign.target = { Node.value = Expression.Name (_ as name); _ };
            Assign.value = { Node.value = Expression.Constant (Constant.String _); _ } as value;
            _;
          };
        _;
      }
      when Option.is_some (Ast.Expression.name_to_reference name) ->
        Some (Ast.Expression.name_to_reference_exn name, value)
    | _ -> None
  in
  let split_for_map = function
    | name, value -> name, value
  in
  source
  |> Preprocessing.toplevel_assigns
  |> List.filter_map ~f:extract_string
  |> List.map ~f:split_for_map
  (* Overwrite with the newer expression for duplicate global assigns *)
  |> Ast.Reference.Map.of_alist_reduce ~f:(fun _old updated -> updated)
  |> fun x -> { global_constants = x }
