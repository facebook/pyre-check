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

module Heap = struct
  type t = StringLiteral.t Reference.Map.t

  let empty = Reference.Map.empty

  let from_source source =
    let extract_string = function
      | {
          Node.value =
            {
              Assign.target = { Node.value = Expression.Name (_ as name); _ };
              Assign.value = { Node.value = Expression.Constant (Constant.String (_ as value)); _ };
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


  let from_qualifiers ~environment ~qualifiers =
    let ast_environment = Analysis.TypeEnvironment.ReadOnly.ast_environment environment in
    let build_per_qualifier qualifier =
      match Analysis.AstEnvironment.ReadOnly.get_processed_source ast_environment qualifier with
      | None -> Reference.Map.empty
      | Some source -> from_source source
    in
    let join =
      let merge ~key:_ = function
        (* TODO(T152494938): Error here after fixing imports as assign issue *)
        | `Both (left, _) -> Some left
        | `Left left -> Some left
        | `Right right -> Some right
      in
      Reference.Map.merge ~f:merge
    in
    qualifiers
    |> List.map ~f:build_per_qualifier
    |> Algorithms.fold_balanced ~init:Reference.Map.empty ~f:join
end

module SharedMemory = struct
  include
    Memory.WithCache.Make
      (Analysis.SharedMemoryKeys.ReferenceKey)
      (struct
        type t = StringLiteral.t

        let prefix = Prefix.make ()

        let description = "Mapping from fully qualified global name to expression"
      end)

  type t = Handle

  let from_heap heap =
    Reference.Map.iteri heap ~f:(fun ~key ~data -> add key data);
    Handle


  let get Handle = get

  let from_qualifiers ~environment ~qualifiers =
    from_heap (Heap.from_qualifiers ~environment ~qualifiers)
end
