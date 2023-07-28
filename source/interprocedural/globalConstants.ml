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
      (* __module__ affects name resolution, due to __module__ specifying the module something was
         defined in, so a solution is just to skip __module__ assignments *)
      | {
          Node.value =
            {
              Assign.target =
                { Node.value = Expression.Name (Attribute { attribute = "__module__"; _ }); _ };
              _;
            };
          _;
        } ->
          None
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
          let as_local = Ast.Expression.name_to_reference_exn name in
          Some (Ast.Reference.delocalize as_local, value)
      | _ -> None
    in
    let split_for_map = function
      | name, value -> name, value
    in
    source
    |> Preprocessing.toplevel_assigns
    |> List.concat_map ~f:Preprocessing.toplevel_expand_tuple_assign
    |> List.filter_map ~f:extract_string
    |> List.map ~f:split_for_map
    (* Overwrite with the newer expression for duplicate global assigns *)
    |> Ast.Reference.Map.of_alist_reduce ~f:(fun _old updated -> updated)


  let from_qualifiers ~environment ~qualifiers =
    let ast_environment = Analysis.TypeEnvironment.ReadOnly.ast_environment environment in
    let build_per_qualifier qualifier =
      match Analysis.AstEnvironment.ReadOnly.get_processed_source ast_environment qualifier with
      | None -> empty
      | Some source -> from_source source
    in
    let reduce =
      let merge ~key = function
        | `Both (left, right) when not (StringLiteral.equal left right) ->
            failwith
              (Format.asprintf
                 "Two different globals in different modules with the same unqualified name `%s` \
                  and different values `%s` and `%s`."
                 (StringLiteral.show left)
                 (StringLiteral.show right)
                 (Reference.show key))
        | `Both (value, _)
        | `Left value
        | `Right value ->
            Some value
      in
      Reference.Map.merge ~f:merge
    in
    qualifiers |> List.map ~f:build_per_qualifier |> Algorithms.fold_balanced ~init:empty ~f:reduce
end

module SharedMemory = struct
  include
    Memory.WithCache.Make
      (Analysis.SharedMemoryKeys.ReferenceKey)
      (struct
        type t = StringLiteral.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "Mapping from fully qualified global name to expression"
      end)

  type t = Handle

  let from_heap heap =
    Reference.Map.iteri heap ~f:(fun ~key ~data -> add key data);
    Handle


  let get Handle = get

  let from_qualifiers ~scheduler ~environment ~qualifiers =
    Scheduler.iter
      scheduler
      ~policy:
        (Scheduler.Policy.fixed_chunk_count
           ~minimum_chunks_per_worker:1
           ~minimum_chunk_size:50
           ~preferred_chunks_per_worker:1
           ())
      ~f:(fun qualifiers ->
        let (_ : t) = from_heap (Heap.from_qualifiers ~environment ~qualifiers) in
        ())
      ~inputs:qualifiers;
    Handle
end
